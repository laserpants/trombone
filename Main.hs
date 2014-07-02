{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe                                      ( catMaybes )
import Data.Text                                       ( Text, pack, unpack )
import Database.HsSqlPpp.Ast
import Database.HsSqlPpp.Parser
import Network.HTTP.Types.Method
import Text.ParserCombinators.Parsec
import Trombone.Db.Execute
import Trombone.Db.Template
import Trombone.Dispatch
import Trombone.Middleware.Amqp                        hiding ( Message, Connection )
import Trombone.Middleware.Cors
import Trombone.Middleware.Logger
import Trombone.Parse
import Trombone.Pipeline
import Trombone.Pipeline.Json
import Trombone.Route
import Trombone.RoutePattern
import Trombone.Router
import Trombone.Server
import Trombone.Tests.Bootstrap
 
import qualified Data.Text                             as Text

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
           , Route "POST" (decompose "border")                         (RoutePipes "createorder")
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

    testParseMethod
    testParseUri

    (_, channel) <- connectAmqp "guest" "guest"
    logger <- buildLogger defaultBufSize "trombone.log"
    systems <- parsePipesFromFile "pipelines.conf"

    print systems

    let conf = DbConf 
            { dbHost = "localhost"
            , dbPort = 5432
            , dbUser = "postgres" 
            , dbPass = "postgres" 
            , dbName = "sdrp5"
            }

    let hmac = buildHmacConf [("generic", "14ad0ef86bc392b39bad6009113c2a5a8a1d993a")] True

    r <- readFile "routes.conf"
    let (Right xs) = parse (many line) "" r
        routes = catMaybes xs

    print routes

    runWithMiddleware 10 3010 conf [cors, logger, amqp channel] routes hmac systems

--    withPostgresqlPool (buildConnectionString conf) 10 $ \pool -> 
--        run 3010 
--            $ cors
--            $ logger 
--            $ amqp channel
--            $ \request -> liftM sendJsonResponseOr404 $ 
--                runReaderT runRoutes (Context pool request myRoutes hmac systems)

-- {{xyz}} hello
-- 

-- hello {{a}} b {{c}}
--
-- ["hello", "a", "b", "c"]

-- insert into user (name, address) values {{name}}, {{address}};
-- insert into user (name, address) values ${name}, ${address};

-- |/| Reflection / probe |/|

------------------------------------------


-- GET  /user      --  insert into ...
-- GET  /user      >>  select * from user
-- GET  /user/:id  ~>  select * from user where id = {{:id}}
-- GET  /user/:id  ->  select * from user where id = {{:id}}
-- POST /user/     <>  (user, id) insert into user ...
-- GET  /user/:id  ><  update ...
--
-- POST /order     ||  order-create
-- POST /x        <js> abc


testParseMethod = do
    let (Right a) = parse method "" "GET"
    let (Right b) = parse method "" "POST"
    let (Right c) = parse method "" "PUT"
    let (Right d) = parse method "" "PATCH"
    let (Right e) = parse method "" "DELETE"
    let (Left  f) = parse method "" "PASTA"
    print (a, b, c, d, e, f)

testParseUri = do
    let (Right a) = parse uri "" "/a/:bcd/efg/"
    let (Right b) = parse uri "" "hello"
    let (Right c) = parse uri "" "/:one/:two"
    let (Right d) = parse uri "" "abcd/x"
    let (Right e) = parse uri "" "/x/"
    let (Right f) = parse uri "" ":what"
--    let (Left  g) = parse uri "" "ab//def"
--    let (Left  h) = parse uri "" "@@hello"
--    let (Left  i) = parse uri "" "/what?"
    print (a, b, c, d, e, f)
 
