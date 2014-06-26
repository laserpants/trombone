module Trombone.Dispatch 
    ( module Trombone.Dispatch.Core
    , dispatch
    ) where

import Trombone.Dispatch.Core
import Trombone.Dispatch.Db
import Data.Text                                       ( Text )

dispatchMeshAction :: Text -> Dispatch RouteResponse
dispatchMeshAction mesh = undefined

dispatchNodeJsAction :: Text -> Dispatch RouteResponse
dispatchNodeJsAction js = undefined

dispatch :: RouteAction -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatch (RouteSql query) ps = dispatchDbAction query ps
dispatch (RouteMesh mesh) _  = dispatchMeshAction mesh
dispatch (RouteNodeJs js) _  = dispatchNodeJsAction js

