module Trombone.Encryption 
    ( encrypted 
    ) where

import Network.Wai                                     ( Response, responseLBS )
import Trombone.Response

encrypted :: RouteResponse -> Response
encrypted resp = undefined

