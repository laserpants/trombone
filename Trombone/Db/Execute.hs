{-# LANGUAGE OverloadedStrings #-}
module Trombone.Db.Execute
    ( Sql
    , getCount
    , getResult
    , getOne
    , noResult
    , runDb
    , runDbConn
    ) where

import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Control.Monad                                   ( liftM )
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.ByteString                                 ( ByteString )
import Data.Conduit
import Data.Maybe                                      ( listToMaybe )
import Data.Text                                       ( Text )
import Data.Text.Encoding                              ( encodeUtf8 )
import Database.Persist
import Database.Persist.Postgresql              hiding ( Sql )
import Database.PostgreSQL.Simple                      ( SqlError(..), ExecStatus(..) )

import qualified Data.Conduit.List                     as CL
import qualified Data.Text                             as Text

type Sql = SqlPersistT (ResourceT (NoLoggingT IO)) 

getCount :: Text -> Sql Int
getCount query = liftM fromIntegral $ rawExecuteCount query []

getResult :: Text -> Sql [[PersistValue]]
getResult query = rawQuery query [] $$ CL.consume 

getOne :: Text -> Sql (Maybe [PersistValue])
getOne = liftM listToMaybe . getResult 

noResult :: Text -> Sql ()
noResult query = rawExecute query []

-- | Run a database query using a connection pool and one of the (Text -> Sql a) 
-- helpers. Return the result in the IO monad.
--
-- Example use: 
--     runDb (getResult "select * from customer") pool >>= print
runDb :: Sql a -> ConnectionPool -> IO a
runDb sql = catchExceptions . runNoLoggingT . runResourceT . runSqlPool sql

-- | Same as runDb except that a single connection is used instead of a pool.
runDbConn :: Sql a -> Connection -> IO a
runDbConn sql = catchExceptions . runNoLoggingT . runResourceT . runSqlConn sql

catchExceptions :: IO a -> IO a
catchExceptions sql = try sql >>= excp
  where excp (Right r) = return r
        excp (Left  e) =
            case fromException e of
              Just e1 -> rethrow e1
              Nothing -> throwM $ SqlError "" FatalError "unknown sql error" "" ""
        rethrow :: SqlError -> IO a
        rethrow = throwM

