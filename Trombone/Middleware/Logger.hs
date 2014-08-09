module Trombone.Middleware.Logger 
    ( module System.Log.FastLogger
    , buildLogger
    ) where

import Data.Default
import Network.Wai                                     ( Middleware )
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger

-- | Build a logger middleware instance that will log requests to the specified 
-- file path location. This is a simple wrapper for the standard wai-extra 
-- request logger.
buildLogger :: BufSize        -- ^ Buffer size
            -> FilePath       -- ^ Log file location
            -> IO (LoggerSet, Middleware)
buildLogger bufsize path = do
    file <- newFileLoggerSet bufsize path
    mw   <- mkRequestLogger def { destination = Logger file }
    return (file, mw)

