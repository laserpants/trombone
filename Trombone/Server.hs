{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Trombone.Server 
    ( DbConf(..)
    , runWithMiddleware
    ) where

import Control.Monad
import Data.ByteString                                 ( ByteString )
import Data.Text                                       ( Text )
import Database.Persist.Postgresql  
import Network.Wai                                     ( Middleware )
import Network.Wai.Handler.Warp                        ( run )
import Trombone.Dispatch.Core
import Trombone.Mesh
import Trombone.Route
import Trombone.Router

import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy.Char8            as L8
import qualified Text.Show.ByteString                  as Show

data DbConf = DbConf 
    { dbHost :: ByteString
    , dbPort :: Int
    , dbUser :: ByteString
    , dbPass :: ByteString
    , dbName :: ByteString
    }

buildConnectionString :: DbConf -> ConnectionString
buildConnectionString DbConf{..} =
    BS.concat [ "host="     ,                         dbHost , " "
              , "port="     , L8.toStrict $ Show.show dbPort , " "
              , "user="     ,                         dbUser , " "
              , "password=" ,                         dbPass , " "
              , "dbname="   ,                         dbName ]

runWithMiddleware :: Int               -- ^ Connection pool size
                  -> Int               -- ^ Server port number
                  -> DbConf            -- ^ Database connection settings
                  -> [Middleware]      -- ^ Middleware stack
                  -> [Route]           -- ^ Application routes
                  -> [(Text, System)]  -- ^ Mesh systems
                  -> IO ()
runWithMiddleware size port dbconf mws routes systems = 
    withPostgresqlPool (buildConnectionString dbconf) size $ \pool -> 
        run port $ foldr ($) `flip` mws $ \request -> do
            let context = Context pool request routes systems
            liftM sendJsonResponseOr404 $ runReaderT runRoutes context

