{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Trombone.Server.Init 
    ( runServer
    ) where 

import Control.Applicative                             ( (<$>) )
import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Control.Monad.Logger                            
import Control.Monad.State
import Data.ByteString                                 ( ByteString )
import Data.Conduit
import Data.HashMap                                    ( Map )
import Data.Pool                                       ( destroyAllResources )
import Data.Text                                       ( Text )
import Data.Text.Encoding                              ( encodeUtf8 )
import Data.Version                                    ( showVersion )
import Database.Persist.Postgresql                     
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import Monitor.Warp 
import Paths_trombone                                  ( version )
import System.Console.GetOpt
import System.Environment                              ( getArgs )
import Trombone.Db.Execute
import Trombone.Db.Reflection
import Trombone.Db.Template
import Trombone.Dispatch.Core
import Trombone.Hmac
import Trombone.Middleware.Amqp
import Trombone.Middleware.Cors
import Trombone.Middleware.Logger
import Trombone.Parse
import Trombone.Pipeline.Json
import Trombone.Response
import Trombone.Route
import Trombone.RoutePattern
import Trombone.Server
import Trombone.Server.Config                   hiding ( options )

import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap                          as Map
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as TIO
import qualified Text.Show.ByteString                  as Show
import qualified Trombone.Server.Config                as Conf

buildConnectionString :: DbConf -> ConnectionString
buildConnectionString DbConf{..} =
    BS.concat [ "host="     ,                         dbHost , " "
              , "port="     , BL.toStrict $ Show.show dbPort , " "
              , "user="     ,                         dbUser , " "
              , "password=" ,                         dbPass , " "
              , "dbname="   ,                         dbName ]

type SetupStep = (Config, ServerConf) -> IO ServerConf

setupLogger :: SetupStep
setupLogger (Config{ .. }, conf@ServerConf{..}) = do
    (logger, m) <- buildLogger configLogBufSize configLogFile configUseColors
    return conf 
        { serverMiddleware = m:serverMiddleware
        , serverLogger     = LoggerConf logger configUseColors
        , serverDtors      = rmLoggerSet logger:serverDtors
        }

setupAmqp :: SetupStep
setupAmqp (Config{ .. }, conf@ServerConf{..}) = do
    (conn, channel) <- connectAmqp' configAmqpHost configAmqpUser configAmqpPass
    return conf
        { serverMiddleware = amqp channel:serverMiddleware
        , serverDtors      = closeConnection conn:serverDtors
        }

-- setupStatic :: SetupStep
-- setupStatic (Config{ .. }, conf@ServerConf{..}) = do 
--     let static = staticPolicy (noDots >-> addBase "public")
--     return conf { serverMiddleware = static:serverMiddleware }
 
setupCors :: SetupStep
setupCors (Config{ .. }, conf@ServerConf{..}) = 
    return conf { serverMiddleware = cors:serverMiddleware }

setupHmac :: SetupStep
setupHmac (Config{ .. }, conf@ServerConf{..}) = do
    hc <- readKeysFromDb serverSqlPool
    return conf { serverHmacConf = buildHmacConf hc configTrustLocal }
  where
    buildHmacConf keys = Just . HmacKeyConf (Map.fromList keys) 

readKeysFromDb :: ConnectionPool -> IO [(ByteString, ByteString)]
readKeysFromDb pool = runDbQ (translate . concat) q pool 
  where 
    q :: SqlT [[PersistValue]]
    q = rawExecute "CREATE TABLE IF NOT EXISTS trombone_keys \
                   \(id serial PRIMARY KEY, \
                   \client character varying(40), \
                   \key character varying(40));" [] 
        >> (rawQuery "SELECT client, key FROM trombone_keys;" [] 
                $$ CL.consume)
    translate [PersistText c, PersistText k] = [(encodeUtf8 c, encodeUtf8 k)]
    translate _ = []

setupRoutes :: SetupStep
setupRoutes (Config{ .. }, conf@ServerConf{..}) = do
    i <- case configRoutesFile of
            Nothing -> runDbQ (translate . concat) q serverSqlPool 
            Just f  -> TIO.readFile f 
    let routes = parseRoutes i
    -- Add /ping response route
    let pong = RouteStatic $ okResponse [("message", "Pong!")]
        ping = Route "GET" (decompose "ping") pong
    rs <- mapM (insertColNames serverSqlPool) routes 
    return conf { serverRoutes = ping:rs }
  where
    q :: SqlT [[PersistValue]]
    q = rawExecute "CREATE TABLE IF NOT EXISTS trombone_config \
                   \(id serial PRIMARY KEY, \
                   \key character varying(40), \
                   \val text);" []
        >> (rawQuery "SELECT val FROM trombone_config \
                    \WHERE key = 'routes';" [] 
                $$ CL.consume)
    translate :: [PersistValue] -> Text
    translate [PersistText v] = v
    translate _ = ""
  
runDbQ :: (a -> b) -> SqlT a -> ConnectionPool -> IO b
runDbQ fn q = liftM fn . runDb' q 

-- | Look up and insert column names for 'SELECT * FROM' type of queries.
insertColNames :: ConnectionPool -> Route -> IO Route
insertColNames pool (Route m p (RouteSql (DbQuery q t))) = 
    liftM (Route m p . RouteSql . flip DbQuery t) (f q)
  where
    f :: DbResult -> IO DbResult
    f (Item       ["*", tbl]) = Item       <$> columns pool tbl 
    f (ItemOk     ["*", tbl]) = ItemOk     <$> columns pool tbl 
    f (Collection ["*", tbl]) = Collection <$> columns pool tbl 
    f x                       = return x 
insertColNames _ r = return r

-- | Find column names for a given table.
columns :: ConnectionPool -> Text -> IO [Text]
columns pool table = 
    liftM (reverse . concatMap txt) $ 
        runDb (rawQuery (Text.replace "%" table q) [] $$ CL.consume) pool
  where 
    q = "SELECT column_name FROM information_schema.columns \
        \WHERE table_name = '%';"
    txt [PersistText n] = [uscToCamel n]
    txt _               = []
 
setupPipes :: SetupStep
setupPipes (Config{ .. }, conf@ServerConf{..}) = do
    pipes <- parsePipesFromFile configPipesFile
    return conf { serverPipelines = pipes }

onStartup :: Config -> IO (ServerSettings (IO ()))
onStartup c@Config{..} = do

    let dbConf = DbConf { dbHost = configDbHost
                        , dbPort = configDbPort
                        , dbUser = configDbUser
                        , dbPass = configDbPass
                        , dbName = configDbName } 
 
    -- Initialize SQL connection pool
    pool <- runNoLoggingT $ createPostgresqlPool 
        (buildConnectionString dbConf) configPoolSize

    serverConf <- foldr setup (initConf pool) $ concatMap opts
        [ ( configEnLogging , setupLogger )
        , ( configEnAmqp    , setupAmqp   )
--      , ( True            , setupStatic )
        , ( configEnCors    , setupCors   )
        , ( configEnPipes   , setupPipes  )
        , ( configEnHmac    , setupHmac   )
        , ( True            , setupRoutes ) 
        ]

    runConf $ serverConf 
        { serverPort       = configServerPort 
        , serverVerbose    = configVerbose 
        , serverDtors      = cleanup pool:serverDtors serverConf }

  where
    setup :: SetupStep -> IO ServerConf -> IO ServerConf
    setup f setup = setup >>= f . (,) c
    cleanup :: ConnectionPool -> IO ()
    cleanup pool = destroyAllResources pool 
        >> putStrLn "Releasing resource pool."
    opts (True, x) = [x]
    opts _         = []

onExit :: ServerSettings (IO ()) -> Shutdown -> IO ()
onExit ServerSettings{ options = destructors } action = do
    destructors
    case action of
        Restart -> putStrLn "Shutting down for restart."
        Exit    -> putStrLn "Service terminating."

runServer :: IO ()
runServer = getArgs >>= translOpts >>= \(c,_) ->
    case c of
        Config{ configShowVer = True } -> 
            putStrLn $ "Trombone server version " ++ showVersion version 
        Config{ configShowHelp = True } ->
            putStrLn $ usageInfo "Usage: trombone [OPTION...]" Conf.options
        _ -> sigServ (onStartup c) onExit

