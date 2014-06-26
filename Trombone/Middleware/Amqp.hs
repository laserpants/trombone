{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Trombone.Middleware.Amqp 
    ( module Network.AMQP
    , connectAmqp
    , amqp 
    ) where

import Control.Monad                          ( when )
import Data.ByteString.Char8                  ( split, filter )
import Data.ByteString.Lazy                   ( fromStrict )
import Data.List                              ( intersperse )
import Data.Text                              ( Text )
import Data.Text.Encoding
import Network.AMQP
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Internal                   ( Request(..) )

import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C8
import qualified Data.Text                    as Text

import Prelude                                hiding ( filter )

-- | Connect to the AMQP service. Called prior to starting the server in order
-- to obtain the connection-channel pair.
connectAmqp :: Text -> Text -> IO (Connection, Channel)
connectAmqp user pass = do
    conn <-  openConnection "127.0.0.1" "/" user pass
    chan <- openChannel conn
    declareExchange chan newExchange { exchangeName = "trombone"
                                     , exchangeType = "direct" }
    return (conn, chan)
 
-- | The AMQP middleware is an asynchronous messaging subsystem based on the 
-- Advanced Message Queuing Protocol. It allows consumer applications to receive
-- asynchronous notifications when server resources are modified.
amqp :: Channel -> (Request -> IO Response) -> Request -> IO Response
amqp chan app req@Request{..} = do
    resp <- app req
    when (effectful && status resp == 200) $
        publishMsg chan "trombone" "api" 
                newMsg { msgBody         = fromStrict $ encodeUtf8 body
                       , msgDeliveryMode = Just Persistent }
    return resp
  where effectful = requestMethod `elem` ["POST", "PUT", "DELETE", "PATCH"]
        body = Text.concat $ [decodeUtf8 requestMethod, " "] 
             ++ intersperse "/" pathInfo
        status = statusCode . responseStatus 

