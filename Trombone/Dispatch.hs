{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch 
    ( module Trombone.Dispatch.Core
    , dispatch
    ) where

import Trombone.Dispatch.Core
import Trombone.Dispatch.Db
import Trombone.Dispatch.Mesh
import Data.Text                                       ( Text )

import qualified Data.Text                             as Text

dispatchNodeJsAction :: Text -> Dispatch RouteResponse
dispatchNodeJsAction js = undefined

dispatch :: RouteAction -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatch (RouteSql query) ps = dispatchDbAction query ps
dispatch (RouteMesh mesh) ps = do
    Context _ _ _ table <- ask
    case lookup mesh table of
        Nothing -> return $ errorResponse ErrorServerConfiguration
            $ Text.concat ["Unknown mesh system '", mesh, "'."]
        Just s  -> dispatchMeshAction s ps
dispatch (RouteNodeJs js) _  = dispatchNodeJsAction js

