{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.NodeJs 
    ( dispatchNodeJs
    ) where

import Control.Applicative
import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.Text                                       ( Text, unpack )
import System.Process
import Trombone.Dispatch.Core

import qualified Data.ByteString.Lazy.Char8            as L8

data NodeResponse = NodeResponse 
    { nodeStatus :: Int
    , nodeBody   :: Value }

instance FromJSON NodeResponse where
    parseJSON (Object v)
        = NodeResponse <$> v .: "statusCode"
                       <*> v .: "body"
    parseJSON _ = mzero

dispatchNodeJs :: Text -> ByteString -> Dispatch RouteResponse
dispatchNodeJs path body = do
    r <- liftIO $ readProcess "node" [unpack path] $ transl body
    case decode $ L8.pack r of
        Nothing -> return $ errorResponse ErrorServerGeneric 
                    "Invalid response from nodejs application script."
        Just nr -> return $ RouteResponse [] (nodeStatus nr) (nodeBody nr)
  where transl = L8.unpack . L8.fromStrict

