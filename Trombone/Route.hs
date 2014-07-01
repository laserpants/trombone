module Trombone.Route 
    ( RouteAction(..)
    , Route(..)
    ) where

import Data.Text                                       ( Text )
import Network.HTTP.Types.Method                       ( Method )
import Trombone.Db.Template                            ( DbQuery )
import Trombone.RoutePattern

-- | A target action associated with a route.
data RouteAction = RouteSql    DbQuery
                 | RoutePipes  Text
                 | RouteNodeJs Text
    deriving (Show)

-- | A request route specification: 
-- viz. (1) the request method (2) a route pattern, and (3) an action.
data Route = Route Method RoutePattern RouteAction
    deriving (Show)
 
