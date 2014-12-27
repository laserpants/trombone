{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Trombone.Dispatch.Core 
    ( Context(..)
    , HmacKeyConf(..)
    , LoggerConf(..)
    , Dispatch
    , escVal
    , escVal'
    , quoute
    , filterNot
    , params
    , sanitize
    , printS
    , logS
    , logSql
    ) where

import Control.Arrow                                   ( (***) )
import Control.Monad                                   ( when )
import Control.Monad.IO.Class                          ( liftIO )
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.Char                                       ( isAlphaNum, isNumber )
import Data.HashMap                                    ( Map )
import Data.List                                       ( intersperse )
import Data.Scientific
import Data.Text                                       ( Text, cons, snoc, pack, empty )
import Data.Text.Lazy                                  ( toStrict )
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat
import Database.Persist.Postgresql
import Network.Wai
import Trombone.Db.Colorize
import Trombone.Db.Template
import Trombone.Middleware.Logger
import Trombone.Pipeline
import Trombone.Route

import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

-- | HMAC authentication configuration data.
data HmacKeyConf = HmacKeyConf 
    (Map ByteString ByteString)  -- ^ Hash map with client keys
    Bool                         -- ^ Bypass authentication for localhost?

data LoggerConf = NoLogger | LoggerConf LoggerSet Bool

-- | Various state required to process a request.
data Context = Context
    { dispatchPool    :: ConnectionPool     -- ^ PostgreSQL connection pool
    , dispatchRoutes  :: [Route]            -- ^ Application routes
    , dispatchKeys    :: Maybe HmacKeyConf  -- ^ HMAC authentication keys
    , dispatchMesh    :: [(Text, Pipeline)] -- ^ Mesh lookup table
    , dispatchVerbose :: Bool               -- ^ Verbose server output?
    , dispatchLogger  :: LoggerConf         -- ^ FastLogger instance
    }

-- | Monad transformer in which requests are dispatched.
type Dispatch m = ReaderT Context m 

-- | Enclose a text value in single quoutes.
quoute :: Text -> Text
{-# INLINE quoute #-}
quoute = flip snoc '\'' . cons '\'' 

filterNot :: (a -> Bool) -> [a] -> [a]
{-# INLINE filterNot #-}
filterNot f = filter (not . f)
 
params :: [(Text, Text)] -> [(Text, EscapedText)]
params = map (prefix *** escape) 
  where 
    escape = EscapedText . q . sanitize
    q :: Text -> Text
    q s = if Text.all isNumber s
              then s
              else quoute s

prefix :: Text -> Text
{-# INLINE prefix #-}
prefix = Text.cons ':'

sanitize :: Text -> Text
sanitize = Text.filter pred 
  where 
    pred :: Char -> Bool
    pred c | isAlphaNum c = True
           | otherwise    = c `elem` "-_!"
 
-- | Translate a JSON value to a format ready to be inserted into an SQL query
-- template. Special care must be taken w.r.t. string values. 
escVal :: Value -> EscapedText
-- Numeric values are inserted as they are.
escVal (Number n) = 
    case floatingOrInteger n of
      Left  r -> f (realFloat r)
      Right i -> f (decimal i)
  where 
    f = EscapedText . toStrict . toLazyText 
-- String values need to be properly escaped and enclosed in quoute marks.
escVal (String s) = EscapedText $ quoute $ foldr f s escapeChars
  where 
    f (from, to) = Text.replace from to
escVal (Bool True)  = EscapedText "'true'"
escVal (Bool False) = EscapedText "'false'"
-- Comma-separate array elements and surround the output with parentheses.
escVal (Array a) = listify a
  where 
    f v = let (EscapedText t) = escVal v in t
    listify = EscapedText . Text.concat . intersperse "," 
                          . map f . Vect.toList 
escVal Null = EscapedText "NULL"
escVal _ = EscapedText empty

escVal' :: Value -> Text
{-# INLINE escVal' #-}
escVal' v = let EscapedText t = escVal v in t

-- | A list of from-and-to character sequences used to escape SQL parameters.
escapeChars :: [(Text, Text)]
{-# INLINE escapeChars #-}
escapeChars = [("\"", "\\\""), ("'", "''")]

-- | Log a message to stdOut.
printS :: String -> Dispatch IO ()
{-# INLINE printS #-}
printS msg = ask >>= \Context{..} -> when dispatchVerbose $ liftIO $ putStrLn msg

-- | Log a message to logger middleware.
logS :: Text -> Dispatch IO ()
logS msg = ask >>= \Context{..} -> 
    case dispatchLogger of
      (LoggerConf ls _) -> liftIO $ pushLogStr ls $ toLogStr msg
      _                 -> return ()

-- | Log an SQL query to logger middleware.
logSql :: Text -> Dispatch IO ()
logSql sql = ask >>= \Context{..} -> 
    case dispatchLogger of
      (LoggerConf ls True ) -> log ls $ colorize sql
      (LoggerConf ls False) -> log ls sql
      _                     -> return ()
  where
    log ls = liftIO . pushLogStr ls . toLogStr

