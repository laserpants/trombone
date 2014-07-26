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
import Data.Text                                       ( Text, replace )
import Data.Text.Encoding                              ( encodeUtf8 )
import Database.Persist.Postgresql                     ( ConnectionString, PersistValue(..), rawExecute, rawQuery, withPostgresqlConn, withPostgresqlPool )
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import Network.Wai                                     ( Middleware, Response, responseLBS )
import Network.Wai.Handler.Warp                        ( run )
import Network.Wai.Middleware.Static
import System.Console.GetOpt
import System.Environment                              ( getArgs )
import Trombone.Db.Execute
import Trombone.Db.Reflection                          ( uscToCamel )
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
    , serverVerbose    :: Bool               -- ^ Log output to stdout
    }

nullConf :: ServerConf
{-# INLINE nullConf #-}
nullConf = ServerConf 0 0 (DbConf "" 0 "" "" "") [] [] Nothing [] False

buildConnectionString :: DbConf -> ConnectionString
buildConnectionString DbConf{..} =
    BS.concat [ "host="     ,                         dbHost , " "
              , "port="     , L8.toStrict $ Show.show dbPort , " "
              , "user="     ,                         dbUser , " "
              , "password=" ,                         dbPass , " "
              , "dbname="   ,                         dbName ]

-- | Run the server with configuration as indicated by command line arguments.
runWithArgs :: IO ()
runWithArgs = do
    args <- getArgs
    translOpts args >>= \(args, _) -> run args
  where run :: Config -> IO ()
        run Config{ configShowVer  = True } = 
            putStrLn "Trombone server version 0.8.1" 
        run Config{ configShowHelp = True } = 
            putStrLn $ usageInfo "Usage: trombone [OPTION...]" options
        run cfg = do
            (_,(_,c)) <- flip runStateT (cfg, nullConf) $ do
                setupLogger
                setupAmqp
                setupStatic
                setupCors
                setupPipes
                setupDbConf
                setupHmac
                setupRoutes 
            runWithConf $ c { serverPoolSize = configPoolSize   cfg
                            , serverPort     = configServerPort cfg 
                            , serverVerbose  = configVerbose    cfg }

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
    , serverVerbose    = loud
    } = 
    withPostgresqlPool (buildConnectionString dbconf) pconns $ \pool -> do
        putStrLn $ "Trombone listening on port " ++ show port ++ "."
        run port $ foldr ($) `flip` midware $ \request app -> do
            let context = Context pool request routes hconf pipes loud
            flip runReaderT context $ runRoutes 
                >>= lift . app . sendJsonResponse . responseOr404 

type ServerState = StateT (Config, ServerConf) IO 

setupLogger :: ServerState ()
setupLogger = do
    (c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) <- get
    when configEnLogging $ do
        logger <- lift $ buildLogger configLogBufSize configLogFile
        put (c, setup{ serverMiddleware = logger:mw })

setupAmqp :: ServerState ()
setupAmqp = do
    (c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) <- get
    when configEnAmqp $ do
        (_, channel) <- lift $ connectAmqp configAmqpUser configAmqpPass
        put (c, setup{ serverMiddleware = amqp channel:mw })

setupStatic :: ServerState ()
setupStatic = do
    let static = staticPolicy (noDots >-> addBase "public")
    modify $ \(c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) -> 
        (c, setup{ serverMiddleware = static:mw })

setupCors :: ServerState ()
setupCors = do
    (c@Config{..}, setup@ServerConf{ serverMiddleware = mw }) <- get
    when configEnCors $ put (c, setup{ serverMiddleware = cors:mw })

setupPipes :: ServerState ()
setupPipes = do
    (c@Config{..}, setup) <- get
    when configEnPipes $ do
        pipes <- lift $ parsePipesFromFile configPipesFile
        put (c, setup{ serverPipelines = pipes })

setupDbConf :: ServerState ()
setupDbConf = modify $ \(c@Config{..}, setup) -> 
    let dbconf = DbConf { dbHost = configDbHost
                        , dbPort = configDbPort
                        , dbUser = configDbUser
                        , dbPass = configDbPass
                        , dbName = configDbName } 
    in (c, setup{ serverDbConf = dbconf })

setupHmac :: ServerState ()
setupHmac = do
    (c@Config{..}, setup@ServerConf{..}) <- get
    when configEnHmac $ do
        hc <- readKeysFromDb
        put (c, setup{ serverHmacConf = buildHmacConf hc configTrustLocal })

readKeysFromDb :: ServerState [(ByteString, ByteString)]
readKeysFromDb = liftM (concatMap f) (runStatement q)
  where q = rawExecute "CREATE TABLE IF NOT EXISTS trombone_keys \
                       \(id serial PRIMARY KEY, \
                       \client character varying(40), \
                       \key character varying(40));" []
            >> rawQuery "SELECT client, key FROM \
                       \trombone_keys;" [] $$ CL.consume
        f [PersistText c, PersistText k] = [(encodeUtf8 c, encodeUtf8 k)]
        f _ = []

-- | Run a database statement in the ServerState monad and return its result.
runStatement :: Sql a -> ServerState a
runStatement sql = do
    (_, ServerConf{..}) <- get
    let conn = buildConnectionString serverDbConf
    res <- try $ lift $ withPostgresqlConn conn $ runDbConn sql
    case res of
      Left  e -> error $ "SQL error: " ++ C8.unpack (sqlErrorMsg e)
      Right r -> return r

setupRoutes :: ServerState ()
setupRoutes = do
    (c@Config{..}, setup) <- get
    routes <- lift $ parseRoutesFromFile configRoutesFile
    -- Add a simple /ping response route
    let pong = RouteStatic $ okResponse [("message", "Pong!")]
        ping = Route "GET" (decompose "ping") pong
    rs <- mapM insertColNames routes
    put (c, setup{ serverRoutes = ping:rs })

-- Look up and insert column names for 'SELECT * FROM' type of queries.
insertColNames:: Route -> ServerState Route
insertColNames (Route m p (RouteSql (DbQuery q t))) = do
    r <- f q
    return $ Route m p (RouteSql (DbQuery r t))
  where f :: DbResult -> ServerState DbResult
        f (Item       ["*", tbl]) = fmap Item       $ columns tbl
        f (ItemOk     ["*", tbl]) = fmap ItemOk     $ columns tbl
        f (Collection ["*", tbl]) = fmap Collection $ columns tbl
        f x                       = return x 
insertColNames r = return r

-- | Find column names for a given table.
columns :: Text -> ServerState [Text]
columns table = liftM (reverse . concatMap txt) $ do
    runStatement $ rawQuery (replace "%" table q) [] $$ CL.consume
  where q = "SELECT column_name FROM information_schema.columns \
            \WHERE table_name = '%';"
        txt [PersistText n] = [uscToCamel n]
        txt _               = []
 
