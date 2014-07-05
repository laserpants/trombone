{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Static where

import Data.Aeson
import Data.Text.Encoding                              ( encodeUtf8 )
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db

import qualified Data.HashMap.Strict                   as HMS

dispatchStatic :: RouteResponse -> Dispatch RouteResponse
dispatchStatic resp@(RouteResponse hs r (Object o)) = 
    case HMS.lookup "<Allow>" o of
        Nothing -> return resp
        Just v  -> return $ RouteResponse (inject v) r (Object o')
  where o'       = HMS.delete "<Allow>" o
        inject v = hs ++ [("Allow", encodeUtf8 $ escVal' v)]
dispatchStatic resp = return resp

