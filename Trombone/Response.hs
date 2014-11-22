{-# LANGUAGE OverloadedStrings #-}
module Trombone.Response 
    ( RouteResponse(..)
    , ResponseError(..)
    , errorResponse
    , okResponse
    , responseCode
    , toText
    , unauthorized
    , sendJsonResponse
    ) where

import Data.Aeson
import Data.HashMap.Strict                             ( fromList, union )
import Data.List.Utils                                 ( addToAL )
import Data.Text                                       ( Text, pack )
import Network.HTTP.Types                              
import Network.Wai                                     ( Response, responseLBS )
import Trombone.Server.Config

-- | A response generated by a route action.
data RouteResponse = RouteResponse [Header] Int Value 
    deriving (Show)

data ResponseError
  = ErrorBadRequest
  | ErrorNotFound
  | ErrorUnauthorized
  | ErrorConflict
  | ErrorServiceUnavailable
  -- SQL errors
  | ErrorSqlKeyConstraintViolation
  | ErrorSqlUniqueViolation
  | ErrorSqlGeneric
  -- Configuration error
  | ErrorServerConfiguration
  -- Generic server error
  | ErrorServerGeneric

toText :: ResponseError -> Text
toText ErrorBadRequest                = "BAD_REQUEST"
toText ErrorNotFound                  = "NOT_FOUND"
toText ErrorUnauthorized              = "UNAUTHORIZED"
toText ErrorConflict                  = "CONFLICT"
toText ErrorSqlKeyConstraintViolation = "SQL_FOREIGN_KEY_CONSTRAINT_VIOLATION"
toText ErrorSqlUniqueViolation        = "SQL_UNIQUE_CONSTRAINT_VIOLATION"
toText ErrorSqlGeneric                = "SQL_ERROR"
toText ErrorServerConfiguration       = "SERVER_CONFIGURATION_ERROR"
toText ErrorServiceUnavailable        = "SERVICE_UNAVAILABLE"
toText _                              = "INTERNAL_SERVER_ERROR"

responseCode :: ResponseError -> Int
responseCode ErrorBadRequest          = 400
responseCode ErrorNotFound            = 404
responseCode ErrorUnauthorized        = 401
responseCode ErrorConflict            = 409
responseCode ErrorServiceUnavailable  = 503
responseCode _                        = 500

-- | JSON response used for errors.
data ErrorObj = ErrorObj Int Text Text

instance ToJSON ErrorObj where
    toJSON (ErrorObj code err msg)
        = object [ "status"       .= False
                 , "error"        .= err
                 , "responseCode" .= code
                 , "message"      .= msg ]

errorResponse :: ResponseError -> Text -> RouteResponse
errorResponse re = RouteResponse [] code . toJSON . ErrorObj code err 
  where 
    code = responseCode re
    err  = toText re

unauthorized :: RouteResponse
{-# INLINE unauthorized #-}
unauthorized = errorResponse ErrorUnauthorized "Unauthorized."

-- | Build and deliver a JSON response from the intermediate object.
sendJsonResponse :: RouteResponse -> Response
sendJsonResponse (RouteResponse hs st val) = responseLBS (Status st "") headers body
  where 
    headers = foldr f [ ("Content-Type", "application/json; charset=utf-8")
                      , versionH ] hs
    -- Insert request headers and replace any existing keys
    f (k, v) a = addToAL a k v
    body = encode val

-- | Generate a 200 OK response from the provided association list.
okResponse_ :: [(Text, Value)] -> RouteResponse
{-# INLINE okResponse_ #-}
okResponse_ = RouteResponse [] 200 . Object . fromList 

-- | Ok response generated from the list, with default message and status 
-- fields automatically inserted, unless already present.
okResponse :: [(Text, Value)] -> RouteResponse
okResponse r = RouteResponse [] i (decorate v)
  where (RouteResponse _ i v) = okResponse_ r
        decorate (Object o)   = Object $ union o defaults
        defaults = fromList [("message", "Ok."), ("status", Bool True)]

