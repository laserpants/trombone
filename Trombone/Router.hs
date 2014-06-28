{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Trombone.Router where

import Data.Text                                       ( Text )
import Network.HTTP.Types                              ( Method )
import Trombone.Dispatch
import Trombone.Dispatch.Core
import Trombone.RoutePattern

import qualified Data.Text                             as Text

runRoutes :: Dispatch (Maybe RouteResponse)
runRoutes = do
    Context pool Request{..} routes _ <- ask
    run routes requestMethod $ filterNot Text.null pathInfo
  where run [] _ _ = do
            liftIO $ print "(no match)"
            return Nothing -- List exhausted
        run (Route method pattern action:rs) mtd info 
                -- First check if the request methods match 
                | mtd /= method = run rs mtd info
                | otherwise =
            case match pattern info of
                Params ps -> liftM Just (dispatch action $ params ps)
                _         -> run rs mtd info
 
