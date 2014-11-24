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
import Data.Conduit.Attoparsec                         
import Data.HashMap                                    ( Map )
import Data.Text                                       ( Text )
import Data.Text.Encoding
import Data.Text.Lazy                                  ( toStrict )
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat
import Data.Version                                    ( showVersion )
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple                    
import Monitor.Warp
import Network.Wai                                     ( Middleware, Response, responseLBS )
import Network.Wai.Handler.Warp                        
import Network.Wai.Internal                            ( Request(..) )
import System.Environment                              ( getArgs )
import Trombone.Db.Template
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db
import Trombone.Dispatch.NodeJs
import Trombone.Dispatch.Pipeline
import Trombone.Dispatch.Static
import Trombone.Hmac
import Trombone.Pipeline
import Trombone.RequestJson
import Trombone.Response
import Trombone.Route
import Trombone.Router
import Trombone.Server.Config

import qualified Data.ByteString                       as BS
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
    { serverPort       :: Int                  -- ^ Server port number
    , serverSqlPool    :: ConnectionPool       -- ^ Database connection pool
    , serverMiddleware :: [Middleware]         -- ^ Middleware stack
    , serverRoutes     :: [Route]              -- ^ Application routes
    , serverHmacConf   :: (Maybe HmacKeyConf)  -- ^ HMAC configuration
    , serverPipelines  :: [(Text, Pipeline)]   -- ^ Pipeline map
    , serverVerbose    :: Bool                 -- ^ Log output to stdout
    , serverLogger     :: LoggerConf           -- ^ FastLogger instance
    , serverDtors      :: [IO ()]              -- ^ Destructors
    }

initConf :: ConnectionPool -> IO ServerConf
{-# INLINE initConf #-}
initConf p = return $ ServerConf 0 p [] [] Nothing [] False NoLogger []

runConf :: ServerConf -> IO (ServerSettings (IO ()))
runConf ServerConf{..} = do

    let config = setOnExceptionResponse catchException -- $ setServerName (snd versionH) -- Since 3.0.2
               $ setBeforeMainLoop (putStrLn $ "Service starting. Trombone listening on port " ++ show serverPort ++ ".")
               $ setPort serverPort defaultSettings

    let context = Context serverSqlPool 
                          serverRoutes 
                          serverHmacConf 
                          serverPipelines 
                          serverVerbose 
                          serverLogger

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
        res <- routeRequest req
        auth <- authRequest req 
        printS $ show res
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

-------------------------------------------------------------------------------
-- Dispatch
-------------------------------------------------------------------------------

dispatch :: RouteResult -> RequestInfo -> Dispatch IO RouteResponse
-- Unauthorized
dispatch _ (RequestInfo _ Untrusted) = return unauthorized
-- Bad request
dispatch _ (RequestInfo (RequestBodyError _ (Position line col)) _) = 
    return $ errorResponse ErrorBadRequest $ T.concat 
        [ "Malformed JSON. Parsing failed on line "
        , tShow line , ", column "
        , tShow col  , "." ]
  where
    tShow = toStrict . toLazyText . decimal 
-- Empty request body
dispatch r (RequestInfo EmptyBody i) = runD r i Null BS.empty
-- Request object interpreted as JSON 
dispatch r (RequestInfo (JsonBody v bs) i) = runD r i v bs

runD :: RouteResult -> ClientIdentity -> Value -> ByteString -> Dispatch IO RouteResponse
-- No match: Error 404 
runD RouteNoResult _ _ _ = return $ errorResponse ErrorNotFound "Resource not found."
-- Database (SQL) route action
runD (RouteResult (RouteSql    q) xs) _ obj _ = dispatchDbAction q (params xs) obj
-- Pipeline
runD (RouteResult (RoutePipes  p) xs) _ obj _ = do
    Context{ dispatchMesh = table } <- ask
    case lookup p table of
        Nothing -> return $ errorResponse ErrorServerConfiguration
            $ T.concat ["Missing pipeline: '", p , "'."]
        Just s -> dispatchPipeline s (params xs) obj
-- Inline pipeline
runD (RouteResult (RouteInline p) xs) _ obj _ = dispatchPipeline p (params xs) obj
-- Node.js script
runD (RouteResult (RouteNodeJs j) _ ) _ _ raw = dispatchNodeJs j raw
-- Static response
runD (RouteResult (RouteStatic r) _ ) _ _ _   = dispatchStatic r

