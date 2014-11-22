{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.NodeJs 
    ( dispatchNodeJs
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class                          ( liftIO )
import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.Text                                       ( Text, unpack )
import System.Process
import Trombone.Dispatch.Core
import Trombone.Response

import qualified Data.ByteString.Lazy.Char8            as L8

data NodeResponse = NodeResponse 
    { nodeStatus :: Int
    , nodeBody   :: Value }

instance FromJSON NodeResponse where
    parseJSON (Object v)
        = NodeResponse <$> v .: "statusCode"
                       <*> v .: "body"
    parseJSON _ = mzero

dispatchNodeJs :: Text -> ByteString -> Dispatch IO RouteResponse
dispatchNodeJs path body = do
    r <- liftIO $ readProcess "node" [unpack path] $ transl body
    return $ case decode $ L8.pack r of
        Just nr -> (RouteResponse [] <$> nodeStatus <*> nodeBody) nr
        Nothing -> errorResponse ErrorServerGeneric 
                    "Invalid response from nodejs application script."
  where 
    transl :: ByteString -> String
    transl = L8.unpack . L8.fromStrict

