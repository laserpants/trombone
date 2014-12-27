{-# LANGUAGE OverloadedStrings #-}
module Trombone.Middleware.Cors 
    ( cors 
    ) where

import Data.ByteString
import Data.List.Utils
import Database.Persist.Types
import Network.HTTP.Types.Header             ( HeaderName )
import Network.HTTP.Types.Status
import Network.Wai                           
import Network.Wai.Internal

-- | The CORS middleware component provides support for accepting cross-domain 
-- requests. For more information about cross-origin resource sharing, see: 
-- http://enable-cors.org.
cors :: Middleware
cors app req cback = do
     let head = requestHeaders req
         hdrs = responseHeaders (lookup "Origin" head) 
                                (lookup "Access-Control-Request-Headers" head)
     case (requestMethod req, hasKeyAL "Access-Control-Request-Method" head) of
         ("OPTIONS", True) -> -- Cross-site preflight request
             cback $ responseLBS ok200 hdrs ""
         _ -> -- Add 'Access-Control-Allow-Origin' header
             app req $ \resp -> 
                cback $ case lookup "Origin" (requestHeaders req) of
                          Nothing  -> resp
                          Just org -> addHeader org resp
  where 
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

addHeader :: ByteString -> Response -> Response
addHeader org (ResponseFile    s headers fp m) = 
               ResponseFile    s (modified headers org) fp m
addHeader org (ResponseBuilder s headers b)    = 
               ResponseBuilder s (modified headers org) b
addHeader org (ResponseStream  s headers b)    = 
               ResponseStream  s (modified headers org) b
addHeader org (ResponseRaw     s resp)         = 
               ResponseRaw     s (addHeader org resp)

modified :: [(HeaderName, a)] -> a -> [(HeaderName, a)]
{-# INLINE modified #-}
modified = flip addToAL "Access-Control-Allow-Origin" 

