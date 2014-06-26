{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Maybe                                      ( fromMaybe )
import Database.Persist.Postgresql
import Network.HTTP.Types                              
import Network.Wai                                     ( Application, Response, responseLBS )
import Network.Wai.Handler.Warp                        ( run )
import Trombone.Db.Template
import Trombone.Dispatch
import Trombone.RoutePattern
import Trombone.Router
import Trombone.Tests.Bootstrap

myQuery :: DbQuery
myQuery = DbQuery (Collection [ "id"
                              , "name"
                              , "latitude"
                              , "longitude"
                              , "tin"
                              , "phone"
                              , "is_active"
                              , "price_category_name"
                              ]) 
                  (DbTemplate [ DbSqlStatic 
                    "select customer.id as id                                                              \
                  \       , customer.name as name                                                          \
                  \       , customer.latitude as latitude                                                  \
                  \       , customer.longitude as longitude                                                \
                  \       , customer.tin as tin                                                            \
                  \       , customer.phone as phone                                                        \
                  \       , customer.is_active as is_active                                                \
                  \       , product_price_category.name as price_category_name                             \
                  \      from customer                                                                     \
                  \      join product_price_category on product_price_category.id = customer.price_cat_id  \
                  \      order by id" 
                              ])

myQuery2 :: DbQuery
myQuery2 = DbQuery (Item ["id", "name", "phone", "is_active"]) 
                   (DbTemplate 
                        [ DbSqlStatic "select id, name, phone, is_active from customer where id = "
                        , DbSqlUriParam "id" 
                        ])

myQuery3 :: DbQuery
myQuery3 = DbQuery Count
                   (DbTemplate 
                        [ DbSqlStatic "insert into order_object (created, customer_id, status, last_change, user_id) values ('now()', "
                        , DbSqlJsonValue "customerId" 
                        , DbSqlStatic ", "
                        , DbSqlJsonValue "status" 
                        , DbSqlStatic ", 'now()', "
                        , DbSqlJsonValue "userId" 
                        , DbSqlStatic ")" 
                        ])

myRoutes = [ 
             Route "GET"  (decompose "customer")                       (RouteSql myQuery) 
           , Route "GET"  (decompose "customer/:id")                   (RouteSql myQuery2)
           , Route "POST" (decompose "customrr/:customer-id/:status")  (RouteSql myQuery3)
           , Route "POST" (decompose "customer")                       (RouteSql myQuery3)
           ]

conn :: ConnectionString
conn = "host=localhost port=5432 user=postgres password=postgres dbname=sdrp5"

app :: ConnectionPool -> Application
app pool request = do
    resp <- runReaderT (runRoutes myRoutes) (Context pool request)
    return $ sendJsonResponse $ fromMaybe (errorResponse ErrorNotFound "Resource not found.") resp

main :: IO ()
main = do
    runTests
    withPostgresqlPool conn 10 $ run 3010 . app 

