{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Trombone.Dispatch.Core
    ( module Core
    , Context(..)
    , Dispatch(..)
    , HmacKeyConf(..)
    , filterNot
    , params
    , quoute
    , requestObj
    ) where

import "mtl" Control.Monad.Reader             as Core 

import Control.Arrow                                   ( (***) )
import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.ByteString.Lazy                            ( fromStrict )
import Data.Char                                       ( isAlphaNum, isNumber )
import Data.Maybe                                      ( listToMaybe, maybeToList, fromMaybe, mapMaybe )
import Data.Text                                       ( Text )
import Database.Persist.Postgresql
import Network.Wai.Internal                   as Core  ( Request(..) )
import System.Log.FastLogger                           ( LoggerSet )
import Trombone.Db.Template
import Trombone.Db.Template                   as Core  ( EscapedText ) 
import Trombone.Pipeline
import Trombone.Response                      as Core
import Trombone.Route                         as Core
import Trombone.Server.Config

import qualified Data.Text                    as Text

-- | Various state required to process a request.
data Context = Context
    { dispatchPool    :: ConnectionPool     -- ^ PostgreSQL connection pool
    , dispatchRequest :: Request            -- ^ Request object
    , dispatchRoutes  :: [Route]            -- ^ Application routes
    , dispatchKeys    :: Maybe HmacKeyConf  -- ^ HMAC authentication keys
    , dispatchMesh    :: [(Text, Pipeline)] -- ^ Mesh lookup table
    , dispatchVerbose :: Bool               -- ^ Server output?
    , dispatchLogger  :: Maybe LoggerSet    -- ^ FastLogger instance
    }

-- | Monad transformer in which requests are dispatched.
type Dispatch = ReaderT Context IO

-- | Parse the raw request body to JSON.
requestObj :: ByteString -> Maybe Value
requestObj bs | bs == ""   = Just Null
              | otherwise = decode $ fromStrict bs 

-- | Enclose a text value in single quoutes.
quoute :: Text -> Text
{-# INLINE quoute #-}
quoute = flip Text.snoc '\'' . Text.cons '\'' 

filterNot :: (a -> Bool) -> [a] -> [a]
{-# INLINE filterNot #-}
filterNot f = filter (not . f)
 
params :: [(Text, Text)] -> [(Text, EscapedText)]
params = map (prefix *** escape) 
  where escape = EscapedText . q . sanitize
        q :: Text -> Text
        q s = if Text.all isNumber s
                  then s
                  else quoute s

prefix :: Text -> Text
{-# INLINE prefix #-}
prefix = Text.cons ':'

sanitize :: Text -> Text
sanitize = Text.filter pred 
  where pred :: Char -> Bool
        pred c | isAlphaNum c = True
               | otherwise    = c `elem` "-_!"
 
