{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Trombone.Server 
    ( DbConf(..)
    , ServerConf(..)
    , runConf
    , initConf
    ) where

import Control.Exception.Lifted                        
import Control.Monad                                   ( when )
import Control.Monad.IO.Class                          ( liftIO ) 
import Control.Monad.Trans.Reader
import Data.ByteString                                 ( ByteString )
import Data.HashMap                                    ( Map )
import Data.Text                                       ( Text )
import Data.Text.Encoding
import Data.Version                                    ( showVersion )
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple                    
import Monitor.Warp
import Network.Wai                                     ( Middleware, Response, responseLBS )
import Network.Wai.Handler.Warp                        
import Network.Wai.Internal                            ( Request(..) )
import System.Environment                              ( getArgs )
import Trombone.Dispatch
import Trombone.Dispatch.Core
import Trombone.Pipeline
import Trombone.RequestJson
import Trombone.Response
import Trombone.Route
import Trombone.Router
import Trombone.Server.Config

import qualified Data.HashMap                          as Map
import qualified Data.Text                             as T

lookupKey :: ByteString -> HmacKeyConf -> Maybe ByteString
{-# INLINE lookupKey #-}
lookupKey key (HmacKeyConf hm _) = Map.lookup key hm

-- | Database connection settings.
data DbConf = DbConf 
    { dbHost :: ByteString
    , dbPort :: Int
    , dbUser :: ByteString
    , dbPass :: ByteString
    , dbName :: ByteString
    }

-- | Sever startup configuration settings.
data ServerConf = ServerConf
    { serverPort       :: Int                -- ^ Server port number
    , serverSqlPool    :: ConnectionPool     -- ^ Database connection pool
    , serverMiddleware :: [Middleware]       -- ^ Middleware stack
    , serverRoutes     :: [Route]            -- ^ Application routes
    , serverHmacConf   :: Maybe HmacKeyConf  -- ^ HMAC configuration
    , serverPipelines  :: [(Text, Pipeline)] -- ^ Pipeline map
    , serverVerbose    :: Bool               -- ^ Log output to stdout
    , serverLogger     :: LoggerConf         -- ^ FastLogger instance
    , serverDtors      :: [IO ()]            -- ^ Destructors
    }

initConf :: ConnectionPool -> IO ServerConf
{-# INLINE initConf #-}
initConf p = return $ ServerConf 0 p [] [] Nothing [] False NoLogger []

runConf :: ServerConf -> IO (ServerSettings (IO ()))
runConf ServerConf{..} = do

    let config = setOnExceptionResponse catchException -- $ setServerName (snd versionH) -- Since 3.0.2
               $ setBeforeMainLoop (putStrLn $ "Service starting. Trombone listening on port " ++ show serverPort ++ ".")
               $ setPort serverPort defaultSettings

    let context = Context serverSqlPool serverRoutes serverHmacConf [] True serverLogger

    return ServerSettings 
        { handler  = foldr ($) (\req resp -> 
            runReaderT (run req) context 
                >>= resp . sendJsonResponse) serverMiddleware
        , config   = config
        , response = serviceUnavailable
        , options  = sequence_ serverDtors
        }

  where
    run :: Request -> Dispatch IO RouteResponse
    run req = do
        Context{ dispatchVerbose = loud } <- ask
        res <- routeRequest req
        auth <- authRequest req 
        when loud $ liftIO $ print res
        dispatch res auth

serviceUnavailable :: Response
serviceUnavailable = sendJsonResponse $ 
    errorResponse ErrorServiceUnavailable "Server shutting down."

-------------------------------------------------------------------------------
-- Exception handling
-------------------------------------------------------------------------------

catchException :: SomeException -> Response
catchException e = 
    sendJsonResponse $
    case fromException e of
      Just e' -> catchSqlErrors e'
      Nothing -> errorResponse ErrorServerGeneric "Internal server error."
 
catchSqlErrors :: SqlError -> RouteResponse
catchSqlErrors SqlError{ sqlState = sqls, sqlErrorDetail = detail } = 
    case sqls of
      "23503" -> errorResponse ErrorSqlKeyConstraintViolation 
                    $ errorMsg "Foreign key constraint violation"
      "23505" -> errorResponse ErrorSqlUniqueViolation
                    $ errorMsg "Unique constraint violation"
      "42P01" -> errorResponse ErrorSqlGeneric
                    $ errorMsg "Undefined table"
      -- @todo: Add more error codes here!
      _       -> errorResponse ErrorSqlGeneric  
                    $ errorMsg $ T.concat 
                        ["SQL error ", T.pack $ show sqls]
  where 
    errorMsg t = T.concat $ (t:) $ if T.null d then ["."] else [": ", d]
    d = decodeUtf8 detail

