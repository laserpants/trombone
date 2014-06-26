module Trombone.Middleware.Logger 
    ( module System.Log.FastLogger
    , buildLogger
    ) where

import Data.Default
import Network.Wai                                     ( Middleware )
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger

buildLogger :: BufSize        -- ^ Buffer size
            -> FilePath       -- ^ Log file location
            -> IO Middleware
buildLogger bufsize path = do
    file <- newFileLoggerSet bufsize path
    mkRequestLogger def { destination = Logger file }

