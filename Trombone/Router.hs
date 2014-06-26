{-# LANGUAGE RecordWildCards #-}
module Trombone.Router where

import Control.Arrow                                   ( (***) )
import Data.Char                                       ( isAlphaNum )
import Data.Text                                       ( Text )
import Trombone.Db.Template
import Trombone.Dispatch
import Trombone.Dispatch.Core
import Trombone.RoutePattern

import qualified Data.Text                             as Text

runRoutes :: [Route] -> Dispatch (Maybe RouteResponse)
runRoutes routes = do
    Context pool Request{..} <- ask
    let info = filterNot Text.null pathInfo
    run routes requestMethod info 
  where run [] _ _ = do
            liftIO $ print "(no match)"
            return Nothing -- List exhausted
        run (Route method pattern action:rs) mtd info 
                -- First check if the request methods match 
                | mtd /= method = run rs mtd info
                | otherwise    =
            case match pattern info of
                Params ps -> liftM Just (dispatch action $ params ps)
                _         -> run rs mtd info
 
params :: [(Text, Text)] -> [(Text, EscapedText)]
params = map (prefix *** escape) 
  where escape = EscapedText . quoute . sanitize

prefix :: Text -> Text
prefix = Text.cons ':'

sanitize :: Text -> Text
sanitize = Text.filter pred 
  where pred :: Char -> Bool
        pred c | isAlphaNum c = True
               | otherwise    = c `elem` "-_!"
 
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

