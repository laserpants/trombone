{-# LANGUAGE OverloadedStrings #-}
module Trombone.Middleware.Cors 
    ( cors 
    ) where

import Data.List.Utils
import Database.Persist.Types
import Network.HTTP.Types.Status
import Network.Wai                           ( Middleware
                                             , Request
                                             , Response
                                             , requestHeaders
                                             , requestMethod
                                             , responseLBS )
import Network.Wai.Internal

cors :: (Request -> IO Response) -> Request -> IO Response
cors app req = do
    let head = requestHeaders req
        hdrs = responseHeaders (lookup "Origin" head) 
                               (lookup "Access-Control-Request-Headers" head)
    case requestMethod req of
        "OPTIONS" -> 
            -- Cross-site preflight request
            return $ responseLBS ok200 hdrs ""
        _ -> do
            -- Add 'Access-Control-Allow-Origin' header
            resp <- app req
            return $ case lookup "Origin" (requestHeaders req) of
                Nothing  -> resp
                Just org -> 
                    case resp of
                        ResponseFile s headers fp m -> 
                                    ResponseFile s (modified headers org) fp m
                        ResponseBuilder s headers b -> 
                                    ResponseBuilder s (modified headers org) b
                        ResponseSource s headers st -> 
                                    ResponseSource s (modified headers org) st
                        _ -> resp
  where modified = flip addToAL "Access-Control-Allow-Origin" 
        responseHeaders origin headers = 
            let h = case (origin, headers) of
                        (Just origin', Just headers') -> 
                            [ ("Access-Control-Allow-Origin",  origin')
                            , ("Access-Control-Allow-Headers", headers') ]
                        _ -> []
                allowed = "POST, GET, PUT, PATCH, DELETE, OPTIONS"
            in [ ("Access-Control-Allow-Methods", allowed)
               , ("Access-Control-Max-Age",       "1728000")
               , ("Content-Type",                 "text/plain") 
               ] ++ h

