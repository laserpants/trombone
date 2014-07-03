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
import Trombone.Hmac

import qualified Data.Conduit.List                     as CL
import qualified Data.Text                             as Text

dispatch :: RouteAction -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatch route ps = do
    Context{ dispatchRequest = r, dispatchMesh = table } <- ask
    body <- liftIO $ mconcat <$> (requestBody r $$ CL.consume)
    auth <- authenticate body
    case auth of
        Left resp -> return resp
        Right _   -> 
            let obj = requestObj body in
            case route of
                RouteSql query -> dispatchDbAction query ps obj
                RoutePipes pipe -> 
                    case lookup pipe table of
                        Nothing -> return $ errorResponse ErrorServerConfiguration
                            $ Text.concat ["Unknown pipeline: '", pipe , "'."]
                        Just s -> dispatchPipeline s ps obj
                RouteNodeJs js -> dispatchNodeJs js body

