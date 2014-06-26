{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Maybe                                      ( fromMaybe )
import Database.Persist.Postgresql  hiding ( Filter )
import Network.HTTP.Types                              
import Network.Wai                                     ( Application, Middleware, Response, responseLBS )
import Network.Wai.Handler.Warp                        ( run )
import Trombone.Db.Template
import Trombone.Dispatch
import Trombone.Middleware.Amqp     hiding ( Message )
import Trombone.Middleware.Logger
import Trombone.Middleware.Cors
import Trombone.RoutePattern
import Trombone.Router
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
myQuery2 = DbQuery (Item ["id", "name", "phone", "isActive"]) 
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
    resp <- runReaderT runRoutes (Context pool request myRoutes)
    return $ sendJsonResponseOr404 resp

main :: IO ()
main = do
    runTests
    withPostgresqlPool conn 10 $ \pool -> do
        (conn, channel) <- connectAmqp "guest" "guest"
        logger <- buildLogger defaultBufSize "trombone.log"
        run 3010 
            $ cors
            $ logger 
            $ amqp channel
            $ app pool

-----------------------

data TransType = TransExclude
               | TransInclude
               | TransBind
               | TransRename
    deriving (Eq, Ord, Show)

data Transformer = Transformer TransType [Value]
    deriving (Eq, Show)

data Predicate = PredEqualTo
               | PredNotEqualTo
               | PredGreaterThan
               | PredGreaterThanOrEqual
               | PredLessThan
               | PredLessThanOrEqual
    deriving (Show)

data Filter = Filter
    { property  :: String
    , predicate :: Predicate
    , value     :: Value
    } deriving (Show)

data Processor = Processor
    { processorId     :: Int              -- ^ A unique identifier
    , processorMethod :: Method           -- ^ Any valid HTTP method
    , processorUri    :: String           -- ^ The resource identifier 
    , processorAggr   :: Maybe Text.Text  -- ^ An optional aggregator
    } deriving (Show)

data Connection = Connection
    { source       :: ProcessorId
    , destination  :: ProcessorId
    , transformers :: [Transformer]
    , filters      :: [Filter]
    } deriving (Show)

data ProcessorId = Id Int | In | Out
    deriving (Eq, Show)

data Message = Message ProcessorId Object
    deriving (Show)

type MessageQueue = [Message]

