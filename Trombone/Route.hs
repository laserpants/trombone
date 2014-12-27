module Trombone.Route 
    ( RouteAction(..)
    , RouteResult(..)
    , Route(..)
    ) where

import Data.Text                                       ( Text )
import Network.HTTP.Types.Method                       ( Method )
import Network.Wai.Internal                            ( Request(..) )
import Trombone.Db.Template
import Trombone.Pipeline
import Trombone.Response
import Trombone.RoutePattern

-- | A target action associated with a route.
data RouteAction = RouteSql    DbQuery
                 | RoutePipes  Text
                 | RouteInline Pipeline
                 | RouteNodeJs Text
                 | RouteStatic RouteResponse
    deriving (Show)

-- | A request route specification: 
-- viz. (1) the request method (2) a route pattern, and (3) an action.
data Route = Route Method RoutePattern RouteAction
    deriving (Show)
 
-- | The result of matching a request against a list of routes. A RouteAction
-- value which denotes the action, and a list of key-value pairs with the uri 
-- parameter names and extracted values.
data RouteResult = RouteNoResult | RouteResult RouteAction [(Text, Text)]
    deriving (Show)

