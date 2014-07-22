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
    Context pool Request{..} routes _ _ loud <- ask
    when loud $ liftIO $ putStrLn $ show requestMethod ++ " " ++ show pathInfo 
    run loud routes requestMethod $ filterNot Text.null pathInfo
  where run v [] _ _ = do
            when v $ liftIO $ print "(no match)"
            return Nothing -- List exhausted
        run v (Route method pattern action:rs) mtd info 
                -- First check if the request methods match 
                | mtd /= method = run v rs mtd info
                | otherwise =
            case match pattern info of
                Params ps -> liftM Just (dispatch action $ params ps)
                _         -> run v rs mtd info
 
