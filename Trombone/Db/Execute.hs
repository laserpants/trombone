{-# LANGUAGE OverloadedStrings #-}
module Trombone.Db.Execute 
    ( SqlT
    , runDb
    , collection
    , item
    , executeCount
    , void
    , toJsonVal
    ) where

import Control.Arrow                                   ( second )
import Control.Exception                               ( Exception, SomeException, fromException )
import Control.Exception.Lifted                        ( try )
import Control.Monad                                   ( liftM )
import Control.Monad.Logger                            ( NoLoggingT, runNoLoggingT )
import Control.Monad.Trans.Resource                   
import Data.Aeson
import Data.Conduit
import Data.Maybe                                      ( fromMaybe, listToMaybe )
import Data.Scientific                                 ( fromFloatDigits )
import Data.Text                                       ( Text, pack )
import Data.Text.Encoding                              ( decodeUtf8 )
import Database.Persist
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple                      ( SqlError(..), ExecStatus(..) )

import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Vector                           as Vect

-- | Database monad transformer stack.
type SqlT = SqlPersistT (ResourceT (NoLoggingT IO))

-- | Run a database query and return the result in the IO monad.
--
-- Example use: 
--     runDb (collection "SELECT * FROM customer") pool >>= print
runDb :: SqlT a -> ConnectionPool -> IO a
runDb sql = catchExceptions . runNoLoggingT . runResourceT . runSqlPool sql
 
source :: Text -> Source SqlT [PersistValue]
{-# INLINE source #-}
source = flip rawQuery [] 

conduit :: Monad m => [Text] -> Conduit [PersistValue] m Value
conduit xs = CL.map $ Object . HMS.fromList . zip xs . map toJsonVal 

collection :: Text -> [Text] -> SqlT [Value]
collection q xs = source q $$ conduit xs =$ CL.consume

item :: Text -> [Text] -> SqlT (Maybe Value)
item q = liftM listToMaybe . collection q 

executeCount :: Text -> SqlT Int
executeCount query = liftM fromIntegral $ rawExecuteCount query []

void :: Text -> SqlT ()
{-# INLINE void #-}
void query = rawExecute query []

catchExceptions :: IO a -> IO a
catchExceptions sql = try sql >>= excp
  where 
    excp (Right r) = return r
    excp (Left  e) = throwM $ fromMaybe fatal (fromException e :: Maybe SqlError)

fatal :: SqlError
{-# INLINE fatal #-}
fatal = SqlError "An exception occured during database query." 
    FatalError "SQL error." "" ""

-------------------------------------------------------------------------------
-- Type conversion helper functions
-------------------------------------------------------------------------------

-- | Translate a PersistValue to a JSON Value.
toJsonVal :: PersistValue -> Value
toJsonVal pv = 
    case pv of
        PersistText       t -> String                   t
        PersistBool       b -> Bool                     b
        PersistByteString b -> String $ decodeUtf8      b
        PersistInt64      n -> Number $ fromIntegral    n
        PersistDouble     d -> Number $ fromFloatDigits d
        PersistRational   r -> Number $ fromRational    r
        PersistMap        m -> fromMap                  m
        PersistUTCTime    u -> showV                    u
        PersistTimeOfDay  t -> showV                    t
        PersistDay        d -> showV                    d
        PersistList      xs -> fromList                xs
        PersistNull         -> Null
        _                   -> String "[unsupported SQL type]"
  where
    showV :: Show a => a -> Value
    showV = String . pack . show

fromList :: [PersistValue] -> Value
{-# INLINE fromList #-}
fromList = Array . Vect.fromList . map toJsonVal

fromMap :: [(Text, PersistValue)] -> Value
{-# INLINE fromMap #-}
fromMap = Object . HMS.fromList . map (second toJsonVal)

