{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Arrow ( second )
import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Data.Aeson
import Data.Maybe                                      ( fromMaybe, mapMaybe )
import Data.Text                                       ( Text )
import Database.Persist.Postgresql  hiding ( Filter, Connection, In )
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import Data.Conduit
import Data.Scientific
import Data.ByteString                                 ( ByteString )
import Network.HTTP.Types                              
import Network.Wai                                     ( Application, Middleware, Response, responseLBS )
import Network.Wai.Internal
import Network.Wai.Handler.Warp                        ( run )
import Trombone.Db.Template
import Trombone.Dispatch
import Trombone.Middleware.Amqp     hiding ( Message, Connection )
import Trombone.Middleware.Cors
import Trombone.Middleware.Logger
import Trombone.RoutePattern
import Trombone.Router
import Trombone.Route
import Trombone.Mesh
import Trombone.Server
import Trombone.Mesh.Json
import Trombone.Db.Execute
import Trombone.Tests.Bootstrap

import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy.Char8            as L8
import qualified Text.Show.ByteString                  as Show

myQuery :: DbQuery
myQuery = DbQuery (Collection [ "id"
                              , "name"
                              , "latitude"
                              , "longitude"
                              , "tin"
                              , "phone"
                              , "isActive"
                              , "priceCategoryName"
                              ]) 
                  (DbTemplate [ DbSqlStatic 
                    "select customer.id                                                                    \
                  \       , customer.name                                                                  \
                  \       , customer.latitude                                                              \
                  \       , customer.longitude                                                             \
                  \       , customer.tin                                                                   \
                  \       , customer.phone                                                                 \
                  \       , customer.is_active                                                             \
                  \       , product_price_category.name                                                    \
                  \      from customer                                                                     \
                  \      join product_price_category on product_price_category.id = customer.price_cat_id  \
                  \      order by id" 
                              ])

myQuery2 :: DbQuery
myQuery2 = DbQuery (Item ["id", "name", "phone", "isActive"]) 
                   (DbTemplate 
                        [ DbSqlStatic "select id, name, phone, is_active from customer where id = "
                        , DbSqlUriParam "id" 
                        ])

myQuery3 :: DbQuery
myQuery3 = DbQuery (LastInsert "order_object" "id")
                   (DbTemplate 
                        [ DbSqlStatic "insert into order_object (created, customer_id, status, last_change, user_id) values ('now()', "
                        , DbSqlJsonValue "customerId" 
                        , DbSqlStatic ", "
                        , DbSqlJsonValue "status" 
                        , DbSqlStatic ", 'now()', "
                        , DbSqlJsonValue "userId" 
                        , DbSqlStatic ")" 
                        ])

myRoutes = [ Route "GET"  (decompose "customer")                       (RouteSql myQuery) 
           , Route "GET"  (decompose "customer/:id")                   (RouteSql myQuery2)
           , Route "POST" (decompose "customrr/:customer-id/:status")  (RouteSql myQuery3)
           , Route "POST" (decompose "customer")                       (RouteSql myQuery3)
           , Route "POST" (decompose "border")                         (RouteMesh "createorder")
           , Route "POST" (decompose "order")                          (RouteSql myQuery4)
           , Route "POST" (decompose "order-product")                  (RouteSql myQuery5)
           ]

myQuery4 :: DbQuery
myQuery4 = DbQuery (LastInsert "order_object" "id")
                (DbTemplate 
                    [ DbSqlStatic "insert into order_object (created, customer_id, status, last_change, user_id) values ('now()', "
                    , DbSqlJsonValue "customerId"
                    , DbSqlStatic ", 'queued', 'now()', "
                    , DbSqlJsonValue "userId"
                    , DbSqlStatic ")"
                    ])

myQuery5 :: DbQuery
myQuery5 = DbQuery (LastInsert "order_product" "id")
                (DbTemplate 
                    [ DbSqlStatic "insert into order_product (order_id, product_id, quantity, price) values ("
                    , DbSqlJsonValue "orderId" 
                    , DbSqlStatic ", "
                    , DbSqlJsonValue "productId" 
                    , DbSqlStatic ", "
                    , DbSqlJsonValue "quantity"
                    , DbSqlStatic ", (select product_price.price from customer join product_price on product_price.price_cat_id = customer.price_cat_id where customer.id = 1 and product_price.product_id = 5))"
                    ])

main :: IO ()
main = do

    (_, channel) <- connectAmqp "guest" "guest"
    logger <- buildLogger defaultBufSize "trombone.log"
    systems <- parseMeshFromFile "mesh.conf"

    print systems

    let conf = DbConf 
            { dbHost = "localhost"
            , dbPort = 5432
            , dbUser = "postgres" 
            , dbPass = "postgres" 
            , dbName = "sdrp5"
            }

    let hmac = buildHmacConf [("generic", "14ad0ef86bc392b39bad6009113c2a5a8a1d993a")] True

    runWithMiddleware 10 3010 conf [cors, logger, amqp channel] myRoutes hmac systems

--    withPostgresqlPool conn 10 $ \pool -> 
--        run 3010 
--            $ cors
--            $ logger 
--            $ amqp channel
--            $ \request -> liftM sendJsonResponseOr404 $ 
--                runReaderT runRoutes (Context pool request myRoutes systems)

