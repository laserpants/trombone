{-# LANGUAGE RecordWildCards #-}
module Trombone.Router 
    ( routeRequest
    ) where

import Control.Monad                                   ( when )
import Control.Monad.IO.Class                          ( liftIO ) 
import Control.Monad.Trans.Reader
import Data.Text                                       ( Text )
import Network.HTTP.Types.Method                       ( Method )
import Network.Wai
import Network.Wai.Internal                            ( Request(..) )
import Trombone.Dispatch.Core
import Trombone.Route
import Trombone.RoutePattern

import qualified Data.Text                             as Text

-- | Compare the incoming request against a list of routes for a possible match
-- and return the result.
routeRequest :: Request -> Dispatch IO RouteResult
routeRequest Request{..} = do
    Context pool routes _ _ loud _ <- ask
    printS $ show requestMethod ++ " " ++ show pathInfo 
    run requestMethod (filterNot Text.null pathInfo) routes 
  where
    run :: Method -> [Text] -> [Route] -> Dispatch IO RouteResult
    -- List exhausted
    run _ _ [] = do
        printS "(no match)"
        return RouteNoResult
    run mtd info (Route method pat action : rs)
        -- First check if the request methods match 
        | mtd /= method = run mtd info rs
        | otherwise = 
            case match pat info of
                Params ps -> return $ RouteResult action ps
                _ -> run mtd info rs

