{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Static where

import Data.Aeson
import Data.Text.Encoding                              ( encodeUtf8 )
import Trombone.Dispatch.Core
import Trombone.Response

import qualified Data.HashMap.Strict                   as HMS

-- | A static response route contains a fixed JSON object. These routes also
-- support a special <Allow> keyword, used to specify such header in the
-- response. A possible use-case for doing so is to expose documentation as 
-- part of a web service, typically in conjunction with the OPTIONS method.
--
-- Example:
--
-- > OPTIONS /photo  {..}  
-- >    {
-- >      "<Allow>" : "GET,POST,OPTIONS",
-- >      "GET"     : {"description":"Retreive a list of all photos."},
-- >      "POST"    : {"description":"Create a new photo."}
-- >    }
--
-- The above route would generate a response:
-- 
-- > < HTTP/1.1 200
-- > < Allow: 'GET,POST,OPTIONS'
-- > < Content-Type: application/json; charset=utf-8
-- > {"GET":{"description":"Retreive a list of all customers."},"POST":{"description":"Create a new customer."}}
--
dispatchStatic :: RouteResponse -> Dispatch IO RouteResponse
dispatchStatic resp@(RouteResponse hs r (Object o)) = 
    case HMS.lookup "<Allow>" o of
        Nothing -> return resp
        Just v  -> return $ RouteResponse (inject v) r (Object $ HMS.delete "<Allow>" o)
  where 
    inject v = hs ++ [("Allow", encodeUtf8 $ escVal' v)]
dispatchStatic resp = return resp

