{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Pipeline where

import Control.Applicative                             ( (<$>) )
import Data.Conduit
import Data.Scientific
import Data.Text                                       ( isPrefixOf )
import Network.HTTP.Types                              
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db                            ( dispatchDbAction, escVal' )
import Trombone.Dispatch.NodeJs
import Trombone.Dispatch.Static
import Trombone.Pipeline
import Trombone.RoutePattern

import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

dispatchPipeline :: Pipeline -> [(Text, EscapedText)] -> Value -> Dispatch RouteResponse
dispatchPipeline q ps (Array a) =
    liftM resp $ mapM (dispatchPipeline q ps) (Vect.toList a)
  where val (RouteResponse _ _ x) = x
        resp = RouteResponse [] 202 . Array . Vect.fromList . map val
dispatchPipeline (Pipeline pcs conns _) ps obj = do
    mq <- initMq
    stabilize (Pipeline pcs conns mq) 
  where initMq = do
            Context{ dispatchRequest = r } <- ask
            -- Set up preliminary message queue from incoming connections
            return $ broadcast (outgoingConns In conns) start
        inject :: (Text, EscapedText) -> Value -> Value
        inject   (k, EscapedText v) (Object o) = Object $ HMS.insert k (String v) o
        inject p@(k, EscapedText v) (Array  a) = Array  $ Vect.map (inject p) a
        inject   (k, EscapedText v) Null       = Object $ HMS.insert k (String v) HMS.empty
        inject _                            x  = x
        start                                  = foldr inject obj ps

stabilize :: Pipeline -> Dispatch RouteResponse
stabilize sys@(Pipeline _ _ mq) = do
    Context pool _ _ _ _ loud _ <- ask
    when loud $ liftIO $ print mq
    s@(Pipeline _ _ mq') <- integrate sys 
    if mq == mq'  -- Has the message queue changed?
        then case processorMsgs Out mq' of
               [] -> return $ errorResponse ErrorServerGeneric 
                                "Bad pipeline. (Null response)"
               msgs -> let o = combine msgs
                        in return $ RouteResponse []
                            (returnCode $ HMS.lookup "responseCode" o) 
                            (Object o)
        else stabilize s
  where returnCode (Just (Number n)) = 
            case floatingOrInteger n of
              Left  r -> 200
              Right i -> fromIntegral i
        returnCode _ = 200

combine :: [Message] -> Object
combine = foldr f HMS.empty
  where f (Message _ o1) o2 = 
            case HMS.toList o1 of
              [(key, val)] -> 
                  let ins a = HMS.insert key (Array $ Vect.cons val a) o2 ;
                        o1' = HMS.fromList [(Text.tail key, Array $ Vect.singleton val)] in
                  case HMS.lookup key o2 of
                    Just (Array a) -> ins a 
                    Just v         -> ins (Vect.singleton v) 
                    Nothing -> if "*" `Text.isPrefixOf` key
                                  then o1' `HMS.union` o2
                                  else o1  `HMS.union` o2
              _ -> o1 `HMS.union` o2
 
-- | Run a single integration step on a pipeline.
integrate :: Pipeline -> Dispatch Pipeline
integrate s@(Pipeline _ _ []) = return s
integrate (Pipeline pcs conns mq) = 
    liftM (Pipeline pcs conns . (++) (processorMsgs Out mq) 
                              . concat) (mapM f pcs)
  where f p = let pid = Id $ processorId p
                  msgs = processorMsgs pid mq in
                runProcessor p msgs $ outgoingConns pid conns

runProcessor :: Processor -> [Message] -> [Connection] -> Dispatch [Message]
runProcessor _ [] _ = return []
runProcessor (Processor pid fields mtd uri exp) msgs conns = do
    Context _ _ _ _ _ loud _ <- ask
    let o = buildJsonRequest msgs
        v = expand exp o
    case (saturated fields v, fill uri o) of
        (True, Just x) -> do
            when loud $ liftIO $ putStrLn (show mtd ++ " " ++ Text.unpack x)
            r <- lookupRoute mtd x
            case r of
                Nothing    -> return []
                Just route -> do
                    (RouteResponse _ _ val) <- disp route v
                    return $ broadcast conns val
        _ -> return msgs

disp :: (Route, [(Text, EscapedText)]) -> Value -> Dispatch RouteResponse
disp (Route _ _ (RouteSql    q   ) , ps) v = dispatchDbAction q ps v
disp (Route _ _ (RouteStatic resp) , _ ) _ = dispatchStatic resp
disp (Route _ _ (RouteInline pl  ) , ps) v = dispatchPipeline pl ps v
disp _                                   _ = return $ RouteResponse [] 500 Null

fill :: RoutePattern -> Object -> Maybe Text
fill (RoutePattern rp) o = Text.intercalate "/" <$> f rp []
  where f [] ps = Just $ reverse ps
        f (Atom a:xs) ps = f xs (a:ps)
        f (Variable v:xs) ps = 
            case HMS.lookup (Text.cons ':' v) o of
                Just (String x)  -> f xs (strip x:ps) 
                Just n           -> f xs (escVal' n:ps)
                _                -> Nothing

strip :: Text -> Text
strip = s . Text.reverse . s . Text.reverse 
  where s t |  "'" `isPrefixOf` t = Text.tail t
            | otherwise           = t

saturated :: [Text] -> Value -> Bool
saturated [] _ = True
saturated (x:xs) (Object o) | HMS.member x o = saturated xs (Object o)
                            | otherwise      = False
saturated xs (Array a) = all (saturated xs) (Vect.toList a)
saturated _ _ = False

lookupRoute :: Method -> Text -> Dispatch (Maybe (Route, [(Text, EscapedText)]))
lookupRoute mtd uri = do
    Context{ dispatchRoutes = routes } <- ask
    run routes 
  where run [] = return Nothing
        run (route@(Route method pattern _):rs) 
                | mtd /= method = run rs
                | otherwise = 
            case match pattern segments of
                Params ps -> return $ Just (route, params ps)
                _         -> run rs
        segments = filterNot Text.null $ Text.splitOn "/" uri

