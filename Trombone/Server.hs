{-# LANGUAGE RecordWildCards, OverloadedStrings, PackageImports #-}
module Trombone.Server 
    ( DbConf(..)
    , ServerConf(..)
    , buildConnectionString
    , nullConf
    , runWithArgs
    , runWithConf
    ) where

import "mtl" Control.Monad.State

import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Control.Monad
import Data.ByteString                                 ( ByteString )
import Data.Conduit
import Data.Text                                       ( Text )
import Data.Text.Encoding                              ( encodeUtf8 )
import Database.Persist.Postgresql                     ( ConnectionString, PersistValue(..), rawExecute, rawQuery, withPostgresqlConn, withPostgresqlPool )
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import Network.Wai                                     ( Middleware, Response, responseLBS )
import Network.Wai.Handler.Warp                        ( run )
import System.Console.GetOpt
import System.Environment                              ( getArgs )
import Trombone.Db.Execute
import Trombone.Dispatch.Core
import Trombone.Middleware.Amqp
import Trombone.Middleware.Cors
import Trombone.Middleware.Logger
import Trombone.Parse
import Trombone.Pipeline
import Trombone.Pipeline.Json
import Trombone.Route
import Trombone.RoutePattern
import Trombone.Router
import Trombone.Server.Config

import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy.Char8            as L8
import qualified Data.Conduit.List                     as CL
import qualified Text.Show.ByteString                  as Show

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
    { serverPoolSize   :: Int                -- ^ Connection pool size
    , serverPort       :: Int                -- ^ Server port number
    , serverDbConf     :: DbConf             -- ^ Database connection settings
    , serverMiddleware :: [Middleware]       -- ^ Middleware stack
    , serverRoutes     :: [Route]            -- ^ Application routes
    , serverHmacConf   :: Maybe HmacKeyConf  -- ^ HMAC configuration
    , serverPipelines  :: [(Text, Pipeline)] -- ^ Pipeline map
    }

nullConf :: ServerConf
{-# INLINE nullConf #-}
nullConf = ServerConf 0 0 (DbConf "" 0 "" "" "") [] [] Nothing []

buildConnectionString :: DbConf -> ConnectionString
buildConnectionString DbConf{..} =
    BS.concat [ "host="     ,                         dbHost , " "
              , "port="     , L8.toStrict $ Show.show dbPort , " "
              , "user="     ,                         dbUser , " "
              , "password=" ,                         dbPass , " "
              , "dbname="   ,                         dbName ]

-- | Run the server with configuration as specified by command line arguments.
runWithArgs :: IO ()
runWithArgs = do
    args <- getArgs
    translOpts args >>= \(args, _) -> run args
  where run :: Config -> IO ()
        run Config{ configShowVer  = True } = 
            putStrLn "Trombone server version 0.8" 
        run Config{ configShowHelp = True } = 
            putStrLn $ usageInfo "Usage: trombone [OPTION...]" options
        run cfg = do
            (_,(_,c)) <- flip runStateT (cfg, nullConf) $ do
                setupLogger
                setupAmqp
                setupCors
                setupPipes
                setupDbConf
                setupHmac
                setupRoutes 
            runWithConf $ c { serverPoolSize = configPoolSize cfg
                            , serverPort     = configServerPort cfg }

-- | Run the server with provided configuration.
runWithConf :: ServerConf -> IO ()
runWithConf ServerConf
    { serverPoolSize   = pconns
    , serverPort       = port
    , serverDbConf     = dbconf
    , serverMiddleware = midware
    , serverRoutes     = routes
    , serverHmacConf   = hconf
    , serverPipelines  = pipes 
    } = 
    withPostgresqlPool (buildConnectionString dbconf) pconns $ \pool -> do
        putStrLn $ "Trombone listening on port " ++ show port ++ "."
        run port $ foldr ($) `flip` midware $ \request app -> do
            let context = Context pool request routes hconf pipes
            runReaderT runRoutes context >>= app . sendJsonResponseOr404 

setupLogger :: StateT (Config, ServerConf) IO ()
setupLogger = do
    (c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) <- get
    when configEnLogging $ do
        logger <- lift $ buildLogger configLogBufSize configLogFile
        put (c, setup{ serverMiddleware = logger:mw })

setupAmqp :: StateT (Config, ServerConf) IO ()
setupAmqp = do
    (c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) <- get
    when configEnAmqp $ do
        (_, channel) <- lift $ connectAmqp configAmqpUser configAmqpPass
        put (c, setup{ serverMiddleware = amqp channel:mw })

setupCors :: StateT (Config, ServerConf) IO ()
setupCors = do
    (c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) <- get
    when configEnCors $ put (c, setup{ serverMiddleware = cors:mw })

setupPipes :: StateT (Config, ServerConf) IO ()
setupPipes = do
    (c@Config{..}, setup) <- get
    when configEnPipes $ do
        pipes <- lift $ parsePipesFromFile configPipesFile
        put (c, setup{ serverPipelines = pipes })

setupDbConf :: StateT (Config, ServerConf) IO ()
setupDbConf = modify $ \(c@Config{..}, setup) -> 
    let dbconf = DbConf { dbHost = configDbHost
                        , dbPort = configDbPort
                        , dbUser = configDbUser
                        , dbPass = configDbPass
                        , dbName = configDbName } 
    in (c, setup{ serverDbConf = dbconf })

setupHmac :: StateT (Config, ServerConf) IO ()
setupHmac = do
    (c@Config{..}, setup@ServerConf{..}) <- get
    when configEnHmac $ do
        hc <- lift $ readKeysFromDb $ buildConnectionString serverDbConf
        put (c, setup{ serverHmacConf = buildHmacConf hc configTrustLocal })

readKeysFromDb :: ConnectionString -> IO [(ByteString, ByteString)]
readKeysFromDb conn = do
    res <- try $ withPostgresqlConn conn $ \c -> 
        flip runDbConn c $ do
            rawExecute "CREATE TABLE IF NOT EXISTS trombone_keys \
                       \(id serial PRIMARY KEY, \
                       \client character varying(40), \
                       \key character varying(40));" []
            res <- rawQuery "SELECT client, key FROM \
                           \trombone_keys" [] $$ CL.consume
            return $ concatMap f res
    case res :: Either SqlError [(ByteString, ByteString)] of
        Left  e -> error $ "SQL error: " ++ C8.unpack (sqlErrorMsg e)
        Right r -> return r
  where f [PersistText c, PersistText k] = [(encodeUtf8 c, encodeUtf8 k)]
        f _ = []

setupRoutes :: StateT (Config, ServerConf) IO ()
setupRoutes = do
    (c@Config{..}, setup) <- get
    routes <- lift $ parseRoutesFromFile configRoutesFile
    -- Add a simple /ping response route
    let ping = Route "GET" (decompose "ping") 
                    (RouteStatic $ okResponse [("message", "Pong!")])
    put (c, setup{ serverRoutes = ping:routes })

