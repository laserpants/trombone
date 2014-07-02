{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text                                       ( Text, pack, unpack )
import Network.HTTP.Types.Method
import Text.ParserCombinators.Parsec
import Trombone.Db.Execute
import Trombone.Dispatch
import Trombone.Middleware.Amqp                        hiding ( Message, Connection )
import Trombone.Middleware.Cors
import Trombone.Middleware.Logger
import Trombone.Pipeline
import Trombone.Pipeline.Json
import Trombone.Route
import Trombone.RoutePattern
import Trombone.Router
import Trombone.Server
import Trombone.Db.Template
import Trombone.Tests.Bootstrap
import Database.HsSqlPpp.Parser
import Database.HsSqlPpp.Ast
 
import qualified Data.Text                             as Text

-- import qualified Data.Conduit.List                     as CL
-- import qualified Data.HashMap.Strict                   as HMS
-- import qualified Data.Text                             as Text
-- import qualified Data.Vector                           as Vect
-- import qualified Data.ByteString                       as BS
-- import qualified Data.ByteString.Char8                 as C8
-- import qualified Data.ByteString.Lazy.Char8            as L8
-- import qualified Text.Show.ByteString                  as Show

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

    runWithMiddleware 10 3010 conf [cors, logger, amqp channel] myRoutes hmac systems

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

tokenize :: Text -> [Text]
tokenize = concatMap (Text.splitOn "}}") . Text.splitOn "{{" 

consume :: [Text] -> [DbSqlSegment]
consume []        = []
consume [x]       | "" == x    = []
                  | otherwise = [DbSqlStatic x]
consume (x:x':xs) | "" == x    = param x':consume xs
                  | otherwise = DbSqlStatic x:param x':consume xs
  where param ""  = error "Empty JSON variable placeholder in a query template."
        param ":" = error "Empty uri parameter value in a query template."
        param x | Text.head x == ':' = DbSqlUriParam $ Text.tail x
                | otherwise         = DbSqlJsonValue x

parseDbTemplate :: Text -> DbTemplate
parseDbTemplate = DbTemplate . consume . tokenize

-- Create an "arbitrary" template instance (for the purpose of extracting
-- various meta-data from the statement using the reflection utilities).
arbitrary :: DbTemplate -> String
arbitrary (DbTemplate segms) = foldr f "" segms
  where f (DbSqlStatic    t) b = unpack t ++ b
        f (DbSqlUriParam  t) b = "'?'" ++ b
        f (DbSqlJsonValue t) b = "'?'" ++ b

-- |/| Reflection / probe |/|

probeTemplate :: DbTemplate -> (Maybe Text, Maybe [Text])
probeTemplate = probe . arbitrary

probe :: String                      -- ^ A "raw" SQL SELECT or INSERT statement
      -> (Maybe Text, Maybe [Text])  -- ^ Table name and list of columns
probe x = case parseStatements "" x of
            Right [i@Insert{}]         -> (statmTable i, statmCols i)
            Right [QueryStatement _ s] -> (queryTable s, queryCols s)
            _                          -> (Nothing, Nothing)

-- | Probe and extract the table name from a standard SELECT query.
queryTable :: QueryExpr -> Maybe Text
queryTable ( Select _ _ _ t _ _ _ _ _ _ ) = extractTref t
queryTable   _                            = Nothing

-- | Probe and extract a list of column names from a standard SELECT query.
queryCols :: QueryExpr -> Maybe [Text]
queryCols ( Select _ _ s _ _ _ _ _ _ _ ) = Just $ extractFromList s
queryCols   _                            = Nothing

extractTref :: [TableRef] -> Maybe Text
extractTref [Tref _ (Name _ [Nmc n]) _] = Just $ pack n
extractTref _                           = Nothing 

extractFromList :: SelectList -> [Text]
extractFromList (SelectList _ xs) = concatMap extract xs

-- | Extract the name components from a SELECT item.
extract :: SelectItem -> [Text]
extract ( SelExp     _ s          ) = f s
  where f (Identifier  _ (Nmc n)  ) = [pack n]
        f (QIdentifier _ xs       ) = map (pack . ncStr) xs
        f _                         = []
extract ( SelectItem _ _ (Nmc  a) ) = [pack a]
extract ( SelectItem _ _ (QNmc a) ) = [pack a]

-- | Probe and extract the table name from an INSERT statement.
statmTable :: Statement -> Maybe Text
statmTable ( Insert _ (Name _ [Nmc n]) _ _ _ ) = Just $ pack n
statmTable _                                   = Nothing

-- | Extract a list of column names from an INSERT statement.
statmCols :: Statement -> Maybe [Text]
statmCols ( Insert _ _ xs _ _ ) = Just $ map (pack . ncStr) xs
statmCols _                     = Nothing

------------------------------------------

-- | Parse a HTTP method.
method :: GenParser Char st Method
method = try ( string "GET"    >> return "GET"    )
     <|> try ( string "POST"   >> return "POST"   )
     <|> try ( string "PUT"    >> return "PUT"    )
     <|> try ( string "PATCH"  >> return "PATCH"  )
     <|>     ( string "DELETE" >> return "DELETE" )

-- | Parse a route pattern.
uri :: GenParser Char st RoutePattern
uri = do
    optional $ char '/'
    liftM RoutePattern $ sepEndBy1 (variable <|> atom) $ char '/'

-- | Parse a uri variable segment.
variable :: GenParser Char st RouteSegment
variable = char ':' >> liftM Variable literal 

-- | Parse a text uri segment.
atom :: GenParser Char st RouteSegment
atom = liftM Atom literal

-- | Parse a string consisting strictly of alphanumeric characters, dashes, 
-- underscores or exclamation marks.
literal :: GenParser Char st Text
literal = liftM pack $ many1 (alphaNum <|> oneOf "-_!")

-- | Parse a single line of input, which may be a comment, a blank line, or 
-- a valid route description.
line :: GenParser Char st (Maybe Route)
line = do
    blankspaces
    r <- optionMaybe route
    optional comment
    eol
    return r

-- | A comment may appear at the end of any line, and starts with a '#'.
comment :: GenParser Char st ()
comment = char '#' >> skipMany (noneOf "\n\r") 

-- | Parse a route (i.e., method, uri, and action).
route :: GenParser Char st Route
route = do
    m <- method
    blankspaces
    u <- uri
    blankspaces
    a <- action
    return $ Route m u a

-- | Any of the valid route action types.
action :: GenParser Char st RouteAction
action = try sqlRoute
     <|> try pipelineRoute
     <|> nodeJsRoute

-- | A database query route.
sqlRoute :: GenParser Char st RouteAction
sqlRoute = try sqlNoResult 
       <|> try sqlItem
       <|> try sqlItemOk
       <|> try sqlCollection
       <|> try sqlLastInsert
       <|> sqlCount

-- | An optional list of field names used for db routes.
hints :: GenParser Char st [Text]
hints = do
    char '('
    r <- elements
    char ')'
    return $ map pack r

-- | A comma-separated list of items.
elements :: GenParser Char st [String]
elements = sepBy cell $ char ',' 

-- | A list item.
cell :: GenParser Char st String
cell = spaces >> many (noneOf ",\n\r)")

result :: DbResult -> GenParser Char st RouteAction
result res = liftM (RouteSql . mkQuery res) (many $ noneOf "\n\r") 

-- | A PostgreSQL route of type that returns no result.
sqlNoResult :: GenParser Char st RouteAction
sqlNoResult = do
    symbolSqlNoResult
    blankspaces
    result NoResult

-- | A PostgreSQL route of type that returns a single item.
sqlItem :: GenParser Char st RouteAction
sqlItem = do
    symbolSqlItem
    blankspaces
    h <- optionMaybe hints
    case h of
        Nothing -> undefined
        Just hs -> result $ Item hs

-- | A PostgreSQL route of type that returns a single item with an 'Ok' status 
-- message.
sqlItemOk :: GenParser Char st RouteAction
sqlItemOk = do
    symbolSqlItemOk
    blankspaces
    h <- optionMaybe hints
    case h of
        Nothing -> undefined
        Just hs -> result $ ItemOk hs

-- | A PostgreSQL route of type that returns a collection.
sqlCollection :: GenParser Char st RouteAction
sqlCollection = do
    symbolSqlCollection
    blankspaces
    h <- optionMaybe hints
    case h of
        Nothing -> undefined
        Just hs -> result $ Collection hs

-- | A PostgreSQL route of type that returns the last inserted id.
sqlLastInsert :: GenParser Char st RouteAction
sqlLastInsert = do
    symbolSqlLastInsert
    blankspaces
    h <- optionMaybe hints
    case h of
        Just [table, seq] -> result $ LastInsert table seq
        _                 -> undefined

-- | A PostgreSQL route of type that returns a row count.
sqlCount :: GenParser Char st RouteAction
sqlCount = symbolSqlCount >> blankspaces >> result Count

-- | Parse a pipeline route.
pipelineRoute :: GenParser Char st RouteAction
pipelineRoute = do
    symbolPipeline
    blankspaces
    liftM (RoutePipes . pack) $ many (noneOf ",\n\r)")

-- | Parse a nodejs route.
nodeJsRoute :: GenParser Char st RouteAction
nodeJsRoute = do
    symbolNodeJs
    blankspaces
    liftM (RouteNodeJs . pack) $ many (noneOf ",\n\r)")

mkQuery :: DbResult -> String -> DbQuery
mkQuery res = DbQuery res . parseDbTemplate . pack 

skip1 :: GenParser Char st a -> GenParser Char st ()
skip1 = liftM $ const ()

-- | Symbol to indicate that the route is a PostgreSQL query template of type 
-- that returns no result.
symbolSqlNoResult :: GenParser Char st ()
symbolSqlNoResult = skip1 $ string "--" 

-- | Symbol for PostgreSQL query of type that returns a single item.
symbolSqlItem :: GenParser Char st ()
symbolSqlItem = skip1 $ string "~>" 

-- | Symbol for PostgreSQL query of type that returns a single item with
-- an 'Ok' status message.
symbolSqlItemOk :: GenParser Char st ()
symbolSqlItemOk = skip1 $ string "->" 

-- | Symbol for PostgreSQL query of type that returns a collection.
symbolSqlCollection :: GenParser Char st ()
symbolSqlCollection = skip1 $ string ">>" 

-- | Symbol for PostgreSQL query of type that returns the last inserted id.
symbolSqlLastInsert :: GenParser Char st ()
symbolSqlLastInsert = skip1 $ string "<>" 

-- | Symbol for PostgreSQL query of type that returns a row count result.
symbolSqlCount :: GenParser Char st ()
symbolSqlCount = skip1 $ string "><" 

-- | Symbol which indicates that the route is a nodejs script.
-- e.g., GET /resource  <js> myscript
symbolNodeJs :: GenParser Char st ()
symbolNodeJs = skip1 $ string "<js>" 

-- | Symbol which indicates that the route is a pipeline.
-- e.g., GET /resource  ||  some-system
symbolPipeline :: GenParser Char st ()
symbolPipeline = skip1 $ string "||" 

-- | Zero or more blank spaces (unlike spaces, this combinator accepts only
-- "true" spaces).
blankspaces :: GenParser Char st ()
blankspaces = skipMany (char ' ')

eol :: GenParser Char st String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"


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
 
