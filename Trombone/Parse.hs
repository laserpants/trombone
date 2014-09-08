{-# LANGUAGE OverloadedStrings #-}
module Trombone.Parse 
    ( lines
    , uri
    , method
    , parseRoutesFromFile
    ) where

import Control.Monad
import Data.Aeson                                      ( decode, eitherDecode )
import Data.List                                       ( foldl' )
import Data.List.Utils                                 ( split, replace )
import Data.Maybe                                      ( maybeToList, catMaybes, mapMaybe )
import Data.Text                                       ( Text, pack, unpack )
import Data.Text.Encoding                              ( encodeUtf8 )
import Network.HTTP.Types.Method
import Text.ParserCombinators.Parsec
import Trombone.Db.Parse
import Trombone.Db.Reflection
import Trombone.Db.Template
import Trombone.Pipeline
import Trombone.Pipeline.Json
import Trombone.Response
import Trombone.Route
import Trombone.RoutePattern

import qualified Data.ByteString.Lazy.Char8            as L8

-- | Parse an HTTP method.
method :: GenParser Char st Method
method = try ( string "GET"     >> return "GET"     )
     <|> try ( string "POST"    >> return "POST"    )
     <|> try ( string "PUT"     >> return "PUT"     )
     <|> try ( string "PATCH"   >> return "PATCH"   )
     <|> try ( string "DELETE"  >> return "DELETE"  )
     <|>     ( string "OPTIONS" >> return "OPTIONS" )

-- | Parse a route pattern.
uri :: GenParser Char st RoutePattern
uri = do
    optional $ char '/'
    liftM RoutePattern $ sepEndBy (variable <|> atom) $ char '/'

-- | Parse a uri variable segment.
variable :: GenParser Char st RouteSegment
variable = char ':' >> liftM Variable literal 

-- | Parse a text uri segment.
atom :: GenParser Char st RouteSegment
atom = liftM Atom literal

-- | Parse a string made up strictly of alphanumeric characters together
-- with a small subset of the special ascii characters.
literal :: GenParser Char st Text
literal = liftM pack $ many1 (alphaNum <|> oneOf "-_!~")

-- | Parse a single line of input, which may be a comment, a blank line, or 
-- a valid route description.
line :: GenParser Char st [Route]
line = do
    blankspaces
    r <- optionMaybe routeOrBlock
    optional comment
    eol
    return $ concat $ maybeToList r

-- | A comment may appear at the end of any line, and starts with a '#'.
comment :: GenParser Char st ()
comment = char '#' >> skipMany (noneOf "\n\r") 

routeOrBlock :: GenParser Char st [Route]
routeOrBlock = try route <|> dryBlock

-- | Parse a route (i.e., method, uri, and action).
route :: GenParser Char st [Route]
route = do
    m <- method
    blankspaces
    u <- uri
    blankspaces
    a <- action
    return [Route m u a]

dryBlock :: GenParser Char st [Route]
dryBlock = do
    string "DRY"
    blankspaces
    s <- many (noneOf "\n\r") 
    eol
    openingBracket
    xs <- sepEndBy (item s) (char ';')
    eol
    closingBracket
    return $ concatMap f xs
  where 
    f r = case parse route "" r of
            Left   _ -> []
            Right rs -> rs

alone :: Char -> GenParser Char st ()
alone c = do
    skip1 (char c)
    blankspaces
    skip1 eol

openingBracket :: GenParser Char st ()
openingBracket = alone '{'

closingBracket :: GenParser Char st ()
closingBracket = alone '}'

item :: String -> GenParser Char st String
item s = do
    m <- segm
    u <- segm
    a <- segm
    blankspaces
    t <- many (noneOf ";\n\r")
    return $ concat 
        [ m , " "
        , u , " "
        , a , " "
        , replace "{{..}}" t s
        , ";" ] 
  where 
    segm = blankspaces >> many (noneOf " \n\r")

-- | Any of the valid route action types.
action :: GenParser Char st RouteAction
action = try sqlRoute
     <|> try pipelineRoute
     <|> try inlineRoute
     <|> try staticRoute
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
cell = do
    spaces 
    s <- many (noneOf ",\n\r) ") 
    spaces
    return s

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
        (Just tbl, Just ["*"]) -> resultFromTemplate (res ["*", tbl]) tpl
        (_, Just cs)           -> resultFromTemplate (res cs) tpl
        _                      -> error 
                "Unable to extract column names from SQL template. \
                \An explicit parameter list is required in the route configuration."

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
pipelineRoute = symbolPipeline >> arg RoutePipes

-- | Parse an inline route.
inlineRoute :: GenParser Char st RouteAction
inlineRoute = do
    symbolInline >> blankspaces >> eol >> firstline
    liftM (route . eitherDecode . L8.pack . wrap . concat) lines
  where route :: Either String Pipeline -> RouteAction
        route (Left  e) = error  $ "Error parsing pipeline : " ++ e
        route (Right p) = RouteInline p
        lines = many jsonLine >>= \p -> lastline >> return p
        firstline = char '{' >> blankspaces >> eol
        lastline  = char '}' >> blankspaces >> eol >> blankspaces
        wrap x = '{':x ++ "}"

jsonLine :: GenParser Char st String
jsonLine = do
    x  <- noneOf "}"
    xs <- many (noneOf "\n\r")
    eol
    return (x:xs)

-- | Parse a static route.
staticRoute :: GenParser Char st RouteAction
staticRoute = do
    symbolStatic 
    blankspaces
    liftM f $ many (noneOf "\n\r") 
   where f :: String -> RouteAction
         f x = case decode $ L8.pack x of
                 Just v -> RouteStatic $ RouteResponse [] 200 v
                 Nothing -> error "Failed to parse JSON data in static route pattern."

-- | Parse a nodejs route.
nodeJsRoute :: GenParser Char st RouteAction
nodeJsRoute = symbolNodeJs >> arg RouteNodeJs

arg :: (Text -> RouteAction) -> GenParser Char st RouteAction
arg t = do
    blankspaces
    r <- many (noneOf ",\n\r) ")
    blankspaces
    return $ t $ pack r

mkQuery :: DbResult -> String -> DbQuery
mkQuery res = DbQuery res . parseDbTemplate . pack 

skip1 :: GenParser Char st a -> GenParser Char st ()
skip1 = liftM $ const ()

symbol :: String -> GenParser Char st ()
symbol = skip1 . string 

-- | Symbol to indicate that the route is a PostgreSQL query template of type 
-- that returns no result.
symbolSqlNoResult :: GenParser Char st ()
symbolSqlNoResult = symbol "--" 

-- | Symbol for PostgreSQL query of type that returns a single item.
symbolSqlItem :: GenParser Char st ()
symbolSqlItem = symbol "~>" 

-- | Symbol for PostgreSQL query of type that returns a single item with
-- an 'Ok' status message.
symbolSqlItemOk :: GenParser Char st ()
symbolSqlItemOk = symbol "->" 

-- | Symbol for PostgreSQL query of type that returns a collection.
symbolSqlCollection :: GenParser Char st ()
symbolSqlCollection = symbol ">>" 

-- | Symbol for PostgreSQL query of type that returns the last inserted id.
symbolSqlLastInsert :: GenParser Char st ()
symbolSqlLastInsert = symbol "<>" 

-- | Symbol for PostgreSQL query of type that returns a row count result.
symbolSqlCount :: GenParser Char st ()
symbolSqlCount = symbol "><" 

-- | Symbol which indicates that the route is a nodejs script.
-- e.g., GET /resource  <js> myscript
symbolNodeJs :: GenParser Char st ()
symbolNodeJs = symbol "<js>" 

-- | Symbol which indicates that the route is a pipeline.
-- e.g., GET /resource  ||  some-system
symbolPipeline :: GenParser Char st ()
symbolPipeline = symbol "||" 

-- | Symbol which indicates that the route is an inline pipeline.
-- e.g., GET /resource  |>  {"processors":[...],"connections":[...]}
symbolInline :: GenParser Char st ()
symbolInline = symbol "|>" 

-- | Symbol to denote a static route.
-- e.g., GET /resource {..} {"hello":"is it me you're looking for?"}
symbolStatic :: GenParser Char st ()
symbolStatic = symbol "{..}" 

-- | Zero or more blank spaces (unlike the default "spaces", this combinator 
-- accepts only "true" spaces).
blankspaces :: GenParser Char st ()
blankspaces = skipMany (char ' ')

-- | End of line.
eol :: GenParser Char st String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"

-- | Read and parse routes from a configuration file with a progress bar
-- being printed to stdout.
parseRoutesFromFile :: FilePath -> IO [Route]
parseRoutesFromFile file = do
    putStr "Reading configuration\n|"
    chars 80 ' ' >> putStr "|" >> chars 81 '\b'
    r <- readFile file
    let ls = preprocess r
    x <- liftM concat $ mapM go $ zip (dots $ length ls) ls
    putChar '\n'
    return x
  where go :: (String, String) -> IO [Route]
        go (dots,x) = 
            case parse line "" (x ++ "\n") of
                Left e   -> error $ show e ++ '\n':x
                Right xs -> putStr dots >> return xs
        chars n = putStr . replicate n 
        dots n = f 0 0 [] ""
          where f i y xs d | i == 81 = xs
                f i y xs d = let y' = div (i*n) 80 
                                 f' = flip f y' $ succ i in 
                    if y' == y then f' xs ('.':d) 
                              else f' (d:fill y y' ++ xs) "."
                fill x x' = replicate (x' - x - 1) ""

preprocess :: String -> [String]
preprocess xs = 
    let (_, ys) = foldl' f ("", []) $ lines xs ++ ["\n"] 
    in filter notNull $ reverse ys 
  where 
    f :: (String, [String]) -> String -> (String, [String])
    f (x, xs) y | null y       = (x' ++ y       , xs)
                | null x       = (x  ++ y'      , xs)
                | '{' == head y = (x' ++ "\n{\n" , xs)
                | '}' == head y = (x' ++ "\n}\n" , xs)
                | ind y        = (x' ++ y'      , xs)
                | otherwise    = (y', trimLine x:xs)
      where
        x' = trimLine x 
        y' = ' ':trimLine (rc y) 
        ind y = ' ' == head y || '#' == head y
        rc "" = ""
        rc x  = head (split "#" x)

notNull :: String -> Bool
notNull = not . null

trimLeft :: String -> String
trimLeft "" = ""
trimLeft (x:xs) | ' ' == x   = trimLeft xs
                | otherwise = x:xs

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse

trimLine :: String -> String
trimLine = trimLeft . trimRight

