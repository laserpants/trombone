{-# LANGUAGE PackageImports #-}
module Trombone.Dispatch.Core
    ( module Core
    , Context(..)
    , Dispatch(..)
    , requestObj
    , quoute
    ) where

import "mtl" Control.Monad.Reader             as Core 

import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.ByteString.Lazy                            ( fromStrict )
import Data.Maybe                                      ( listToMaybe, maybeToList, fromMaybe, mapMaybe )
import Data.Monoid                                     ( mconcat )
import Data.Text                                       ( Text )
import Database.Persist.Postgresql
import Network.Wai.Internal                   as Core  ( Request(..) )
import Trombone.Db.Template                   as Core  ( EscapedText ) 
import Trombone.Response                      as Core
import Trombone.Route                         as Core

import qualified Data.Text                    as Text

-- | Various state required to process a request.
data Context = Context ConnectionPool Request

-- | Monad transformer in which requests are dispatched.
type Dispatch = ReaderT Context IO

-- | Parse the raw (chunked) request body to JSON.
requestObj :: [ByteString] -> Value
requestObj body | null body = Null
                | otherwise = fromMaybe Null (f body)
  where f = decode . fromStrict . mconcat

-- | Enclose a text value in single quoutes.
quoute :: Text -> Text
quoute = flip Text.snoc '\'' . Text.cons '\'' 

