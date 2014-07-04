{-# LANGUAGE OverloadedStrings #-}
module Trombone.Server.Config 
    ( Config(..)
    , HmacKeyConf(..)
    , allowLocal
    , buildHmacConf
    , defaultConfig
    , lookupKey
    , options
    , translOpts 
    ) where

import Data.ByteString                                 ( ByteString )
import Data.HashMap                                    ( Map )
import Data.List.Utils                                 ( split )
import Data.Maybe                                      ( catMaybes, fromMaybe )
import Data.Text                                       ( Text, pack, unpack )
import System.Console.GetOpt
import Trombone.Middleware.Logger
import Trombone.Pipeline

import qualified Data.ByteString.Char8        as BS
import qualified Data.HashMap                 as Map

-- | Server startup configuration parameters.
data Config = Config
    { configEnHmac     :: Bool
    -- ^ Enable message integrity authentication (HMAC)?
    , configEnCors     :: Bool
    -- ^ Support cross-origin resource sharing?
    , configEnAmqp     :: Bool
    -- ^ Whether RabbitMQ messaging middleware should be enabled.
    , configEnPipes    :: Bool
    -- ^ Enable request pipelines?
    , configEnLogging  :: Bool
    -- ^ Enable logging to file?
    , configServerPort :: Int
    -- ^ Port number on which the server should listen.
    , configLogFile    :: FilePath
    -- ^ Location of log file.
    , configLogBufSize :: BufSize
    -- ^ Application log file size limit.
    , configAmqpUser   :: Text
    -- ^ RabbitMQ username
    , configAmqpPass   :: Text
    -- ^ RabbitMQ password
    , configDbHost     :: ByteString
    -- ^ Database host
    , configDbName     :: ByteString
    -- ^ Database name
    , configDbUser     :: ByteString
    -- ^ Database username
    , configDbPass     :: ByteString
    -- ^ Database password
    , configDbPort     :: Int
    -- ^ Database port
    , configRoutesFile :: FilePath
    -- ^ Route pattern configuration file.
    , configPipesFile  :: FilePath
    -- ^ Pipelines configuration file.
    , configTrustLocal :: Bool
    -- ^ Skip HMAC authentication for requests originating from localhost?
    , configPoolSize   :: Int
    -- ^ The number of connections to keep in PostgreSQL connection pool.
    , configShowVer    :: Bool
    -- ^ Show version number?
    , configShowHelp   :: Bool
    -- ^ Show usage info?
    } deriving (Show)

-- | Default values for server startup.
defaultConfig :: Config
defaultConfig = Config
    { configEnHmac     = True
    , configEnCors     = False
    , configEnAmqp     = False
    , configEnPipes    = False
    , configEnLogging  = False
    , configServerPort = 3010
    , configLogFile    = "log/access.log"
    , configLogBufSize = defaultBufSize
    , configAmqpUser   = "guest"
    , configAmqpPass   = "guest"
    , configDbHost     = "localhost"
    , configDbName     = "trombone"
    , configDbUser     = "postgres"
    , configDbPass     = "postgres"
    , configDbPort     = 5432
    , configRoutesFile = "routes.conf"
    , configPipesFile  = "pipelines.conf"
    , configTrustLocal = False
    , configPoolSize   = 10
    , configShowVer    = False
    , configShowHelp   = False
    }

translOpts :: [String] -> IO (Config, [String])
translOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultConfig o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageInfo header options)
  where header = "Usage: trombone [OPTION...]"

options :: [OptDescr (Config -> Config)]
options =
    [ Option "V" ["version"]
      (NoArg $ \opts -> opts { configShowVer = True })
      "display version number and exit"
    , Option "?" ["help"]
      (NoArg $ \opts -> opts { configShowHelp = True })
      "display this help and exit"
    , Option "x" ["disable-hmac"]
      (NoArg $ \opts -> opts { configEnHmac = False })
      "disable message integrity authentication (HMAC)"
    , Option "C" ["cors"]
      (NoArg $ \opts -> opts { configEnCors = True })
      "enable support for cross-origin resource sharing"
    , Option "A" ["amqp"]
      (OptArg amqpOpts "USER:PASS")
      "enable RabbitMQ messaging middleware [username:password]"
    , Option "i" ["pipelines"]
      (OptArg (\d opts -> opts 
            { configEnPipes   = True
            , configPipesFile = fromMaybe "pipelines.conf" d }) "FILE")
      "enable request pipelines [configuration file]"
    , Option "s" ["port"]
      (ReqArg (\p opts -> opts { configServerPort = read p }) "PORT")
      "server port"
    , Option "l" ["access-log"]
      (OptArg (\d opts -> opts 
            { configEnLogging = True 
            , configLogFile   = fromMaybe "log/access.log" d }) "FILE")
      "enable logging to file [log file]"
    , Option [] ["size"]
      (ReqArg (\p opts -> opts 
            { configLogBufSize = fromMaybe defaultBufSize $ read p 
            }) "SIZE")
      "log file size"
    , Option "h" ["db-host"]
      (ReqArg (\p opts -> opts { configDbHost = BS.pack p }) "HOST")
      "database host"
    , Option "d" ["db-name"]
      (ReqArg (\p opts -> opts { configDbName = BS.pack p }) "DB")
      "database name"
    , Option "u" ["db-user"]
      (ReqArg (\p opts -> opts { configDbUser = BS.pack p }) "USER")
      "database user"
    , Option "p" ["db-password"]
      (ReqArg (\p opts -> opts { configDbPass = BS.pack p }) "PASS")
      "database password"
    , Option "P" ["db-port"]
      (ReqArg (\p opts -> opts { configDbPort = read p }) "PORT")
      "database port"
    , Option "r" ["routes-file"]
      (ReqArg (\p opts -> opts { configRoutesFile = p }) "FILE")
      "route pattern configuration file"
    , Option "t" ["trust-localhost"]
      (NoArg $ \opts -> opts { configTrustLocal = True })
      "skip HMAC authentication for requests from localhost"
    , Option [] ["pool-size"]
      (ReqArg (\p opts -> opts { configPoolSize = read p }) "SIZE")
      "number of connections to keep in PostgreSQL connection pool"
    ]
  where amqpOpts Nothing  opts = amqpOpts (Just "guest:guest") opts
        amqpOpts (Just d) opts = let [u, p] = pair $ split ":" d
                                 in  opts { configEnAmqp   = True
                                          , configAmqpUser = pack u
                                          , configAmqpPass = pack p } 
        pair [u, p] = [u, p]
        pair _      = ["guest", "guest"]

-- | HMAC authentication configuration data.
data HmacKeyConf = HmacKeyConf 
    (Map ByteString ByteString)  -- ^ Hash map with client keys
    Bool                         -- ^ Bypass authentication for localhost?

buildHmacConf :: [(ByteString, ByteString)] -> Bool -> Maybe HmacKeyConf
buildHmacConf keys = Just . HmacKeyConf (Map.fromList keys) 

lookupKey :: ByteString -> HmacKeyConf -> Maybe ByteString
{-# INLINE lookupKey #-}
lookupKey key (HmacKeyConf hm _) = Map.lookup key hm

allowLocal :: HmacKeyConf -> Bool
{-# INLINE allowLocal #-}
allowLocal (HmacKeyConf _ a) = a

