{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Trombone.Pipeline.Json where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Scientific
import Network.HTTP.Types                              
import Trombone.Pipeline
import Trombone.RoutePattern

import qualified Data.HashMap.Strict                   as HMS
import qualified Data.ByteString.Lazy.Char8            as L8

instance FromJSON ProcessorId where
    parseJSON (Number n) = return $ Id $ fromIntegral $ coefficient n
    parseJSON _          = mzero

instance FromJSON Predicate where
    parseJSON (String "equalTo")            = return PredEqualTo
    parseJSON (String "notEqualTo")         = return PredNotEqualTo
    parseJSON (String "greaterThan")        = return PredGreaterThan
    parseJSON (String "greaterThanOrEqual") = return PredGreaterThanOrEqual
    parseJSON (String "lessThan")           = return PredLessThan
    parseJSON (String "lessThanOrEqual")    = return PredLessThanOrEqual
    parseJSON _ = mzero

instance FromJSON TransType where
    parseJSON (String "exclude")            = return TransExclude
    parseJSON (String "include")            = return TransInclude
    parseJSON (String "bind")               = return TransBind
    parseJSON (String "rename")             = return TransRename
    parseJSON (String "copy")               = return TransCopy
    parseJSON (String "aggregate")          = return TransAggregate
    parseJSON _ = mzero

instance FromJSON Filter where
    parseJSON (Object o)
        = Filter <$> o .: "property"
                 <*> o .: "predicate"
                 <*> o .: "value"
    parseJSON _ = mzero

instance FromJSON Transformer where
    parseJSON (Object o)
        = Transformer <$> o .: "action"
                      <*> o .: "arguments"
    parseJSON _ = mzero

instance FromJSON Connection where
    parseJSON (Object o) 
        = Connection <$> o .:? "source"        .!= In
                     <*> o .:? "destination"   .!= Out
                     <*> o .:  "transformers"  .!= []
                     <*> o .:  "filters"       .!= []
    parseJSON _ = mzero

instance FromJSON Method where
    parseJSON (String "GET")     = return "GET"
    parseJSON (String "POST")    = return "POST"
    parseJSON (String "PUT")     = return "PUT"
    parseJSON (String "DELETE")  = return "DELETE"
    parseJSON (String "PATCH")   = return "PATCH"
    parseJSON (String "OPTIONS") = return "OPTIONS"
    parseJSON _                  = mzero

instance FromJSON Processor where
    parseJSON (Object o)
        = Processor <$> o .:  "id"
                    <*> o .:  "fields"
                    <*> o .:  "method"
                    <*> o .:  "uri"
                    <*> o .:? "expand"
    parseJSON _ = mzero

instance FromJSON Pipeline where
    parseJSON (Object o)
        = Pipeline <$> o .: "processors"
                   <*> o .: "connections"
                   <*> return []
    parseJSON _ = mzero

parsePipesFromFile :: FilePath -> IO [(Text, Pipeline)]
parsePipesFromFile file = liftM (parsePipes . L8.pack ) (readFile file) 

parsePipes :: L8.ByteString -> [(Text, Pipeline)]
parsePipes p = case eitherDecode p of
                 Left  e -> error  $ "Error parsing pipeline : " ++ e
                 Right p -> HMS.toList p

instance FromJSON RoutePattern where
    parseJSON (String o) = return $ decompose o
    parseJSON _          = mzero

