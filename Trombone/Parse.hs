{-# LANGUAGE OverloadedStrings #-}
module Trombone.Parse where

import Control.Monad
import Data.Text                                       ( Text, pack, unpack )
import Network.HTTP.Types.Method
import Text.ParserCombinators.Parsec
import Trombone.Db.Parse
import Trombone.Db.Reflection
import Trombone.Db.Template
import Trombone.Route
import Trombone.RoutePattern

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

resultFromTemplate :: DbResult -> DbTemplate -> GenParser Char st RouteAction
resultFromTemplate res = return . RouteSql . DbQuery res 

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
        Just hs -> result $ Item hs
        Nothing -> inspect Item

-- | A PostgreSQL route of type that returns a single item with an 'Ok' status 
-- message.
sqlItemOk :: GenParser Char st RouteAction
sqlItemOk = do
    symbolSqlItemOk
    blankspaces
    h <- optionMaybe hints
    case h of
        Just hs -> result $ ItemOk hs
        Nothing -> inspect ItemOk

-- | A PostgreSQL route of type that returns a collection.
sqlCollection :: GenParser Char st RouteAction
sqlCollection = do
    symbolSqlCollection
    blankspaces
    h <- optionMaybe hints
    case h of
        Just hs -> result $ Collection hs
        Nothing -> inspect Collection

inspect :: ([Text] -> DbResult) -> GenParser Char st RouteAction
inspect res = do
    q <- many $ noneOf "\n\r"
    let tpl = parseDbTemplate $ pack q
    case probeTemplate tpl of
        (_, Just cs) -> resultFromTemplate (res cs) tpl
        _            -> error 
                "Unable to extract column names from SQL statement. \
                \Add parameter hints to configuration."

-- | A PostgreSQL route of type that returns the last inserted id.
sqlLastInsert :: GenParser Char st RouteAction
sqlLastInsert = do
    symbolSqlLastInsert
    blankspaces
    h <- optionMaybe hints
    case h of
        Just [table, seq] -> result $ LastInsert table seq
        _                 -> do
            q <- many $ noneOf "\n\r"
            let tpl = parseDbTemplate $ pack q
            case probeTemplate tpl of
                (Just tbl, _) -> resultFromTemplate (LastInsert tbl "id") tpl
                _             -> error 
                        "Unable to infer table name from SQL statement."

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

