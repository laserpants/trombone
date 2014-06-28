{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Mesh where

import Data.Conduit
import Data.Scientific
import Network.HTTP.Types                              
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db                            ( escVal, dispatchDbAction_ )
import Trombone.Mesh
import Trombone.RoutePattern

import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

dispatchMeshAction :: System -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatchMeshAction (System pcs conns _) ps = do
    mq <- initMq
    runEagerly (System pcs conns mq) 
  where initMq = do
            Context _ r _ _ <- ask
            body <- lift (requestBody r $$ CL.consume) 
            let obj = requestObj body
            return $ broadcast (outgoingConns In conns) $ foldr inject obj ps
        inject :: (Text, EscapedText) -> Value -> Value
        inject   (k, EscapedText v) (Object o) = Object $ HMS.insert k (String v) o
        inject p@(k, EscapedText v) (Array  a) = Array  $ Vect.map (inject p) a
        inject _                            x  = x

-- | Run the system and terminate with a response as soon as a message is
-- delivered to the Out processor.
runEagerly :: System -> Dispatch RouteResponse
runEagerly (System _ _ []) = return $ errorResponse ErrorServerConfiguration
                                "Mesh message queue exhausted."
runEagerly sys@(System _ _ mq) = 
    case processorMsgs Out mq of
        [] -> integrate sys >>= runEagerly 
        om -> do
            let o  = buildJsonRequest om
            let rc = returnCode $ HMS.lookup "responseCode" o
            return $ RouteResponse rc $ Object o
  where returnCode (Just (Number n)) = 
            case floatingOrInteger n of
              Left  r -> 200
              Right i -> fromIntegral i
        returnCode _ = 200

-- | Run a single integration step on the system.
integrate :: System -> Dispatch System
integrate s@(System _ _ []) = return s
integrate (System pcs conns mq) = do
    liftIO $ do
        print "#######################################################"
        print mq
    liftM (System pcs conns . concat) (mapM f pcs)
  where f pc@(Processor p _ _ _) = do
            let pid = Id p
                msgs = processorMsgs pid mq
                count = connections pid conns
            -- Compare the number of messages in the queue to the 
            -- number of incoming connections for this processor
            if count destination == length msgs
                then runProcessor pc msgs $ outgoingConns pid conns
                -- This processor is still waiting for more results
                else return msgs

runProcessor :: Processor
             -> [Message]
             -> [Connection]
             -> Dispatch [Message]
runProcessor (Processor pid mtd uri exp) msgs conns = do

    liftIO $ print $ "Running processor: " ++ show pid

-- temp
--    if uri == "forward"
--        then do
--            liftIO $ print "forward"
--            return [] -- broadcast conns $ compileMsgs exp msgs
--
--        else do

    r <- lookupRoute mtd uri
    case r of
        Just (Route _ _ (RouteSql q), ps) -> do
            RouteResponse _ v <- dispatchDbAction_ q ps $ compileMsgs exp msgs
            liftIO $ print v
            return $ broadcast conns v 
        Just (Route _ _ (RouteMesh   _), ps) -> return [] -- @todo
        Just (Route _ _ (RouteNodeJs _), ps) -> return [] -- @todo
        _                                    -> undefined -- @todo

lookupRoute :: Method -> Text -> Dispatch (Maybe (Route, [(Text, EscapedText)]))
lookupRoute mtd uri = do
    Context _ _ routes _ <- ask
    run routes 
  where run [] = return Nothing
        run (route@(Route method pattern _):rs) 
                | mtd /= method = run rs
                | otherwise = 
            case match pattern segments of
                Params ps -> return $ Just (route, params ps)
                _         -> run rs
        segments = filterNot Text.null $ Text.splitOn "/" uri

