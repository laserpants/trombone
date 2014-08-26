{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch 
    ( module Trombone.Dispatch.Core
    , dispatch
    ) where

import Control.Applicative                             ( (<$>) )
import Data.Conduit
import Data.Monoid                                     ( mconcat )
import Data.Text                                       ( Text )
import Trombone.Dispatch.Core
import Trombone.Dispatch.Db
import Trombone.Dispatch.NodeJs
import Trombone.Dispatch.Pipeline
import Trombone.Dispatch.Static
import Trombone.Hmac
import Trombone.Response

import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy.Char8            as L8
import qualified Data.ByteString.Lazy.Internal         as LI
import qualified Data.Conduit.List                     as CL
import qualified Data.Text                             as Text

strictRequestBody_ :: Request -> IO BS.ByteString
strictRequestBody_ req = loop id
  where 
    loop front = do
      bs <- requestBody req
      if BS.null bs
          then return $ L8.toStrict $ front LI.Empty
          else loop (front . LI.Chunk bs)

dispatch :: RouteAction -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatch route ps = do
    Context{ dispatchRequest = r, dispatchMesh = table } <- ask
    body <- liftIO $ strictRequestBody_ r 
    auth <- authenticate body
    case auth of
        Left resp -> return resp
        Right _   -> 
            case requestObj body of
                Nothing  -> return $ errorResponse ErrorBadRequest "Malformed JSON." 
                Just obj -> 
                    case route of
                      RouteSql query -> dispatchDbAction query ps obj
                      RoutePipes pipe -> 
                          case lookup pipe table of
                              Nothing -> return $ errorResponse ErrorServerConfiguration
                                  $ Text.concat ["Unknown pipeline: '", pipe , "'."]
                              Just s -> dispatchPipeline s ps obj
                      RouteInline p -> dispatchPipeline p ps obj
                      RouteNodeJs js -> dispatchNodeJs js body
                      RouteStatic resp -> dispatchStatic resp

