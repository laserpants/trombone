{-# LANGUAGE OverloadedStrings, RecordWildCards, PackageImports #-}
module Main where

import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans                       ( lift )

import Control.Applicative
import Control.Arrow                                   ( (***), second )
import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Control.Monad                                   ( liftM )
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString                                 ( ByteString )
import Data.ByteString.Lazy                            ( fromStrict )
import Data.Char                                       ( isAlphaNum )
import Data.Conduit
import Data.Maybe                                      ( listToMaybe, maybeToList, fromMaybe, mapMaybe )
import Data.Monoid                                     ( mconcat )
import Data.Scientific
import Data.Text                                       ( Text )
import Data.Text.Lazy                                  ( toStrict )
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat
import Database.Persist
import Database.Persist.Postgresql                     hiding ( Sql )
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import Network.HTTP.Types                              ( status200 )
import Network.HTTP.Types.Method
import Network.Wai                                     ( Application, Middleware, responseLBS, requestBody )
import Network.Wai.Handler.Warp                        ( run )
import Network.Wai.Internal                            ( Request(..) )
import Trombone.Db.Execute
import Trombone.Db.Template
import Trombone.Dispatch
import Trombone.Response
import Trombone.Route
import Trombone.RoutePattern
import Trombone.Tests.Bootstrap

import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as L8
import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

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
                        ]
                   )

myQuery3 :: DbQuery
myQuery3 = DbQuery Count
                   (DbTemplate 
                        [ DbSqlStatic "insert into order_object (created, customer_id, status, last_change, user_id) values ('now()', "
                        , DbSqlUriParam "customer-id" 
                        , DbSqlStatic ", "
                        , DbSqlUriParam "status" 
                        , DbSqlStatic ", 'now()', "
                        , DbSqlUriParam "user-id" 
                        , DbSqlStatic ")" 
                        ]
                   )


x = [ 
        Route "GET"  (decompose "customer")                               (RouteSql myQuery) 
    ,   Route "GET"  (decompose "customer/:id")                           (RouteSql myQuery2)
    ,   Route "POST" (decompose "customrr/:customer-id/:status")          (RouteSql myQuery3)
    ,   Route "POST" (decompose "customer/:customer-id/:status/:user-id") (RouteSql myQuery3)
    ]

app :: ConnectionPool -> Application
app pool request = do
    -- runReaderT (dispatch $ RouteSql myQuery) $ Context pool request 
    runReaderT (runRoutes x) (Context pool request)

--    case instantiateQ myQuery [] of
--        Left  e -> undefined                             -- Bad request
--        Right t -> do
--            x <- runDb (getResult t) pool
--            print x

    return $ responseLBS status200 [] "-\n"

conn :: ConnectionString
conn = "host=localhost port=5432 user=postgres password=postgres dbname=sdrp5"

main :: IO ()
main = do
    runTests
    withPostgresqlPool conn 10 $ run 3010 . app 

-- {-# LANGUAGE OverloadedStrings #-}
-- module Main where
-- 
-- import Control.Monad                                   ( void )
-- import Control.Monad.Logger
-- import Control.Monad.Trans.Resource
-- import Data.Conduit
-- import Data.Text                                       ( Text, append )
-- import Database.Persist
-- import Database.Persist.Postgresql
-- import Trombone.Db.Template
-- import Trombone.RoutePattern
-- import Trombone.Tests.Bootstrap
-- 
-- import qualified Data.Conduit.List                     as CL
-- import qualified Data.Text                             as Text
-- 
-- -------------------------- 
 
catchException :: SomeException -> RouteResponse
catchException e = 
    case fromException e of
      Just e1 -> catchDbErrors e1
      Nothing -> error "Uncaught exception."
 
catchDbErrors SqlError{ sqlState = sqls } = 
    case sqls of
      "23503" -> errorResponse ErrorSqlConstraintViolation "Foreign key constraint violation."
      "23505" -> errorResponse ErrorSqlUniqueViolation     "Unique constraint violation."
      "42P01" -> errorResponse ErrorSqlGeneric             "Undefined table."
      -- @todo: Add more error types here!
      _       -> errorResponse ErrorSqlGeneric             "Unknown SQL error."

mood :: DbQuery -> [(Text, EscapedText)] -> Dispatch RouteResponse
mood (DbQuery ret tpl) ps = 
    case instantiate tpl ps of
        Left e -> -- 400 Bad request: Request parameters did not match template
            return $ errorResponse ErrorBadRequest 
                "Invalid route: Template parameter list undersaturated."
        Right q -> do
            res <- try $ getResponse ret q 
            return $ case res of   
                       Left  e -> catchDbErrors e -- An SQL exception occured
                       Right r -> r

banan :: DbQuery -> [(Text, EscapedText)] -> Value -> Dispatch RouteResponse
banan q ps (Array  a) = undefined
banan q ps (Object o) = undefined
banan q ps Null       = mood q ps
banan q ps _          = undefined

xx :: Object -> [(Text, EscapedText)]
xx = map (second escVal) . HMS.toList 

escVal :: Value -> EscapedText
escVal (Number n) = 
    case floatingOrInteger n of
      Left  r -> EscapedText $ toStrict $ toLazyText $ realFloat r
      Right i -> EscapedText $ toStrict $ toLazyText $ decimal i

dispatchDbAction :: DbQuery -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatchDbAction q ps = do
    Context _ Request{..} <- ask

    body <- lift $ requestBody $$ CL.consume
    banan q ps $ requestObj body 
    --banan q ps Null -- requestObj body 

-- | Translate the raw request body to a JSON value.
requestObj :: [ByteString] -> Value
requestObj body | null body = Null
                | otherwise = fromMaybe Null (f body)
  where f = decode . fromStrict . mconcat

dispatchMeshAction :: Text -> IO RouteResponse
dispatchMeshAction mesh = undefined

dispatchNodeJsAction :: Text -> IO RouteResponse
dispatchNodeJsAction js = undefined

dispatch :: RouteAction -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatch (RouteSql query) ps = do
    res <- dispatchDbAction query ps
    liftIO $ print res
    return res
dispatch (RouteMesh mesh) ps = lift $ dispatchMeshAction mesh
dispatch (RouteNodeJs js) ps = lift $ dispatchNodeJsAction js

runRoutes :: [Route] -> Dispatch (Maybe RouteResponse)
runRoutes routes = do
    Context pool Request{..} <- ask
    let info = filterNot Text.null pathInfo
    liftIO $ print info
    run routes requestMethod info 
  where run [] _ _ = return Nothing -- List exhausted: No match!
        run (Route method pattern action:rs) mtd info 
                -- First check if the request methods match 
                | mtd /= method = run rs mtd info
                | otherwise    =
            case match pattern info of
                Params ps -> liftM Just (dispatch action $ params ps)
                _         -> run rs mtd info

params :: [(Text, Text)] -> [(Text, EscapedText)]
params = map (prefix *** escape) 
  where escape = EscapedText . quoute . sanitize

prefix :: Text -> Text
prefix = Text.cons ':' 

quoute :: Text -> Text
quoute = flip Text.snoc '\'' . Text.cons '\'' 

sanitize :: Text -> Text
sanitize = Text.filter pred 
  where pred  :: Char -> Bool
        pred  c | isAlphaNum c = True
                | otherwise    = c `elem` "-_!"

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

-- myQuery :: DbQuery
-- myQuery = DbQuery (Collection ["id", "name", "latitude", "longitude", "tin", "phone", "is_active", "price_category_name"]) (DbTemplate [ DbSqlStatic "select customer.id as id, customer.name as name, customer.latitude as latitude, customer.longitude as longitude, customer.tin as tin, customer.phone as phone, customer.is_active as is_active, product_price_category.name as price_category_name from customer join product_price_category on product_price_category.id = customer.price_cat_id order by id" ])
-- 
-- -- runQ :: DbQuery -> IO ()
-- runQ pool (DbQuery res tpl) = 
--     case instantiate tpl [] of
--         Left e -> undefined
--         Right q -> 
--             case res of
--                 NoResult -> void $ rawExecuteCount q []
--                 _        -> undefined
-- 
-- connStr = "host=localhost port=5432 user=postgres password=postgres dbname=sdrp5"
-- 
-- banan :: ConnectionPool -> IO [[PersistValue]]
-- banan = runNoLoggingT . runResourceT . runSqlPool q
-- 
-- q :: SqlPersistT (ResourceT (NoLoggingT IO)) [[PersistValue]]
-- q = rawQuery "" [] $$ CL.consume
-- 
-- main :: IO ()
-- main = void $ withPostgresqlPool connStr 10 banan
-- 
-- template = DbTemplate [ DbSqlStatic "select id from dispatch where id = "
--                       , DbSqlUriParam "id" ]
-- 
-- -- import Data.Aeson                                      ( ToJSON, toJSON, object, (.=) )
-- -- import Data.Text                                       ( Text, splitOn, isPrefixOf )
-- -- import Network.HTTP.Types                              ( status200 )
-- -- import Network.Wai                                     ( Application, Middleware, responseLBS )
-- -- import Network.Wai.Handler.Warp                        ( run )
-- -- import Network.Wai.Internal                            ( Request(..), Response(..) )
-- -- 
-- -- import qualified Data.Text                             as Text
-- -- 
-- -- data User = User String
-- -- 
-- -- instance ToJSON User where
-- --     toJSON (User name) 
-- --         = object [ "name" .= name
-- --                  ]
-- -- 
-- -- dispatch :: Middleware
-- -- dispatch app req = undefined
-- -- 
-- -- app :: Application
-- -- app req = return $ responseLBS status200 [] ""
-- -- 
-- -- -- app :: Application
-- -- -- app Request{ requestMethod = method, pathInfo = info } = do
-- -- --     let someObject = User "hello"
-- -- --     return $ responseLBS status200 [] $ encode someObject
-- -- 
-- -- main :: IO ()
-- -- main = run 3010 $ dispatch app
-- -- 
-- 
