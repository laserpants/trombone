{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Pipeline where

import Control.Applicative                             ( (<$>) )
import Data.Conduit
import Data.Scientific
import Network.HTTP.Types                              
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db                            ( escVal', dispatchDbAction )
import Trombone.Pipeline
import Trombone.RoutePattern

import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

dispatchMeshAction :: Pipeline -> [(Text, EscapedText)] -> Value -> Dispatch RouteResponse
dispatchMeshAction (Pipeline pcs conns _) ps obj = do
    mq <- initMq
    stabilize (Pipeline pcs conns mq) 
  where initMq = do
            Context{ dispatchRequest = r } <- ask
            -- Set up preliminary message queue from incoming connections
            return $ broadcast (outgoingConns In conns) $ foldr inject obj ps
        inject :: (Text, EscapedText) -> Value -> Value
        inject   (k, EscapedText v) (Object o) = Object $ HMS.insert k (String v) o
        inject p@(k, EscapedText v) (Array  a) = Array  $ Vect.map (inject p) a
        inject _                            x  = x

stabilize :: Pipeline -> Dispatch RouteResponse
stabilize sys@(Pipeline _ _ mq) = do
    liftIO $ print mq
    s@(Pipeline _ _ mq') <- integrate sys 
    if mq == mq'  -- Has the message queue changed?
        then case processorMsgs Out mq of
               [] -> return $ errorResponse ErrorServerGeneric 
                                "Bad pipeline. (Null response)"
               msgs -> let o = combine msgs
                        in return $ RouteResponse 
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
                  let ins a = HMS.insert key (Array $ Vect.cons val a) o2 in
                  case HMS.lookup key o2 of
                    Just (Array a) -> ins a 
                    Just v         -> ins (Vect.singleton v) 
                    Nothing -> o1 `HMS.union` o2
              _             -> o1 `HMS.union` o2
   
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
runProcessor (Processor pid fields mtd uri exp) msgs conns = do
    let o = buildJsonRequest msgs
        v = expand exp o
    case (saturated fields v, fill uri o) of
        (True, Just x) -> do
            r <- lookupRoute mtd x
            case r of
                Nothing -> return []
                Just (Route _ _ (RouteSql    q), ps) -> do
                    RouteResponse _ r <- dispatchDbAction q ps v
                    return $ broadcast conns r
                Just (Route _ _ (RoutePipes  _), ps) -> return [] -- @todo
                Just (Route _ _ (RouteNodeJs _), ps) -> return [] -- @todo
        _ -> return msgs

fill :: RoutePattern -> Object -> Maybe Text
fill (RoutePattern rp) o = Text.intercalate "/" <$> f rp []
  where f [] ps = Just ps
        f (Atom a:xs) ps = f xs (a:ps)
        f (Variable v:xs) ps = 
            case HMS.lookup v o of
                Nothing -> Nothing
                Just x  -> f xs $ escVal' x:ps
        
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

