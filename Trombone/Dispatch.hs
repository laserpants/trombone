{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch 
    ( dispatch
    ) where

import Control.Arrow                                   ( (***) )
import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.Char                                       ( isAlphaNum, isNumber )
import Data.Conduit.Attoparsec                         
import Data.Text                                       ( Text, cons, snoc, pack, empty )
import Data.Text.Lazy                                  ( toStrict )
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat
import Trombone.Db.Template
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db
import Trombone.Dispatch.NodeJs
import Trombone.Dispatch.Static
import Trombone.Hmac
import Trombone.RequestJson
import Trombone.Response
import Trombone.Route

import qualified Data.Text                             as Text
import qualified Data.ByteString                       as BS

params :: [(Text, Text)] -> [(Text, EscapedText)]
params = map (cons ':' *** escape) 
  where 
    escape = EscapedText . q . sanitize
    q :: Text -> Text
    q s = if Text.all isNumber s
              then s
              else quoute s

sanitize :: Text -> Text
sanitize = Text.filter pred 
  where 
    pred :: Char -> Bool
    pred c | isAlphaNum c = True
           | otherwise    = c `elem` "-_!"

dispatch :: RouteResult -> RequestInfo -> Dispatch IO RouteResponse
-- Unauthorized
dispatch _ (RequestInfo _ Untrusted) = return unauthorized
-- Bad request
dispatch _ (RequestInfo (RequestBodyError _ (Position line col)) _) = 
    return $ errorResponse ErrorBadRequest $ Text.concat 
        [ "Malformed JSON. Parsing failed on line "
        , tShow line , ", column "
        , tShow col  , "." ]
  where
    tShow = toStrict . toLazyText . decimal 
-- Empty request body
dispatch r (RequestInfo EmptyBody i) = runD r i Null BS.empty
-- Request object interpreted as JSON 
dispatch r (RequestInfo (JsonBody v bs) i) = runD r i v bs

runD :: RouteResult -> ClientIdentity -> Value -> ByteString -> Dispatch IO RouteResponse
-- No match: Error 404 
runD RouteNoResult _ _ _ = return $ errorResponse ErrorNotFound "Resource not found."
-- Database (SQL) route action
runD (RouteResult (RouteSql    q) xs) _ obj _ = dispatchDbAction q (params xs) obj
-- Pipeline
runD (RouteResult (RoutePipes  _) xs) _ obj _ = undefined
-- Inline pipeline
runD (RouteResult (RouteInline _) xs) _ obj _ = undefined
-- Node.js script
runD (RouteResult (RouteNodeJs j) _ ) _ _ raw = dispatchNodeJs j raw
-- Static response
runD (RouteResult (RouteStatic r) _ ) _ _ _   = dispatchStatic r

