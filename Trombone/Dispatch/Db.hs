{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Db 
    ( dispatchDbAction
    ) where

import Control.Arrow                                   ( second )
import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Data.Aeson
import Data.Conduit
import Data.List                                       ( intersperse )
import Data.Maybe                                      ( listToMaybe, maybeToList, fromMaybe, mapMaybe )
import Data.Scientific
import Data.Text                                       ( Text, pack, empty )
import Data.Text.Encoding
import Data.Text.Lazy                                  ( toStrict )
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy.Builder.RealFloat
import Data.Vector                                     ( Vector, fromList, toList )
import Database.Persist
import Database.Persist.Postgresql              hiding ( Sql )
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import Network.Wai.Internal                            ( Request(..) )
import Trombone.Db.Execute
import Trombone.Db.Template
import Trombone.Dispatch.Core
import Trombone.Response

import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

-- | Run a database query and return the result in the Dispatch monad stack.
dispatchDbAction :: DbQuery -> [(Text, EscapedText)] -> Dispatch RouteResponse
dispatchDbAction q ps = do
    Context _ r <- ask
    body <- lift $ requestBody r $$ CL.consume
    r <- run $ requestObj body 
    liftIO $ print r
    return r
  where run (Array a) = 
            -- Run a sequence of actions and collect the results
            liftM resp $ mapM run (Vect.toList a)
          where val (RouteResponse _ x) = x
                -- Using response code 202 is to indicate that the result
                -- of each individual request must be considered separately
                -- and that no claim is made as to the state of success 
                -- w.r.t these.
                resp = RouteResponse 202 . Array . Vect.fromList . map val
        run (Object o) = _run q $ ps ++ map (second escVal) (HMS.toList o)
        run _          = _run q ps

_run :: DbQuery -> [(Text, EscapedText)] -> Dispatch RouteResponse
_run (DbQuery ret tpl) ps = 
    case instantiate tpl ps of
        Left e -> do
            liftIO $ print e
            -- 400 Bad request: Request parameters did not match template
            return $ errorResponse ErrorBadRequest $ Text.concat
                [ "Invalid route: Template parameter list undersaturated, \
                  \with the following arguments missing: "
                , Text.concat $ intersperse ", " $ map arg e ]
        Right q -> do
            liftIO $ print q
            res <- try $ getDbResponse ret q 
            return $ case res of   
                       Left  e -> catchDbErrors e -- An SQL exception occured
                       Right r -> r
  where arg :: Text -> Text
        arg x | Text.null x       = x
              | ':' == Text.head x = quoute $ Text.tail x
              | otherwise         = quoute x

-- | Run a database action in the Dispatch monad.
runDbDispatch :: Sql a -> Dispatch a
runDbDispatch sql = ask >>= \(Context pool _) -> lift $ runDb sql pool

-- | Run a database query and respond according to the specified result type.
getDbResponse :: DbResult -> Text -> Dispatch RouteResponse
-- A simple 200 OK response is sufficient.
getDbResponse NoResult q = runDbDispatch (noResult q) >> return (okResponse [])
-- Respond with a single item, or a 404 error.
getDbResponse (Item ns) q = do 
    r <- runDbDispatch (getOne q) 
    return $ case row ns $ concat $ maybeToList r of
               Nothing -> errorResponse ErrorNotFound "Resource not found."
               Just v  -> RouteResponse 200 v
-- Respond with a collection.
getDbResponse (Collection ns) q = liftM f $ runDbDispatch (getResult q)
  where f = RouteResponse 200 . Array . fromList . mapMaybe (row ns)
-- Respond with the row count in a result.
getDbResponse Count q = do
    r <- runDbDispatch (getCount q) 
    return $ okResponse [("rows", Number $ fromIntegral r)]

row :: [Text] -> [PersistValue] -> Maybe Value
row ns xs | length xs /= length ns = Nothing
          | otherwise = Just $ toObj $ zip ns xs

toObj :: [(Text, PersistValue)] -> Value
toObj = Object . HMS.fromList . map (second persistValToJsonVal)

-- | Translate a JSON value to an equivalent PersistValue.
jsonValToPersistVal :: Value -> PersistValue
jsonValToPersistVal (String t) = PersistText t
jsonValToPersistVal (Bool   b) = PersistBool b
jsonValToPersistVal (Number n) = scientificToPersistVal n
jsonValToPersistVal (Array  a) = valuesToPersistList a
jsonValToPersistVal (Object _) = PersistText "[object]"
jsonValToPersistVal  Null      = PersistNull

-- | Translate a PersistValue to a JSON value.
persistValToJsonVal :: PersistValue -> Value
persistValToJsonVal (PersistText       t) = String t
persistValToJsonVal (PersistBool       b) = Bool b
persistValToJsonVal (PersistByteString b) = String $ decodeUtf8 b
persistValToJsonVal (PersistInt64      n) = Number $ fromIntegral n
persistValToJsonVal (PersistDouble     d) = Number $ fromFloatDigits d
persistValToJsonVal (PersistMap        m) = toObj m
persistValToJsonVal (PersistUTCTime    u) = showV u
persistValToJsonVal (PersistTimeOfDay  t) = showV t
persistValToJsonVal (PersistDay        d) = showV d
persistValToJsonVal (PersistList      xs) = persistValsToJsonArray xs
persistValToJsonVal  PersistNull          = Null
persistValToJsonVal  _                    = String "[unsupported SQL type]"

-- | Translate a JSON value to a format ready to be inserted into an SQL query
-- template. Special care must be taken w.r.t. string values. 
escVal :: Value -> EscapedText
-- Numeric values are inserted 'as they are'.
escVal (Number n) = 
    case floatingOrInteger n of
      Left  r -> f (realFloat r)
      Right i -> f (decimal i)
  where f = EscapedText . toStrict . toLazyText 
-- String values need to be properly escaped and enclosed in quoute marks.
escVal (String s) = EscapedText $ quoute $ foldr f s escapeChars
  where f (from, to) = Text.replace from to
escVal (Bool True)  = EscapedText "'true'"
escVal (Bool False) = EscapedText "'false'"
-- Comma-separate array elements and surround the output with parentheses.
escVal (Array a) = listify a
  where f v = let (EscapedText t) = escVal v in t
        listify = EscapedText . quoute . Text.concat . intersperse "," 
                              . map f  . Vect.toList 
escVal _ = EscapedText empty

-- | A list of from-and-to character sequences used to escape SQL parameters.
escapeChars :: [(Text, Text)]
escapeChars = [("\"", "\\\""), ("'", "\\'")]

-------------------------------------------------------------------------------
-- Type conversion helper functions
-------------------------------------------------------------------------------

persistValsToJsonArray :: [PersistValue] -> Value
persistValsToJsonArray = Array . fromList . map persistValToJsonVal

valuesToPersistList :: Vector Value -> PersistValue
valuesToPersistList = PersistList . map jsonValToPersistVal . toList

scientificToPersistVal :: Scientific -> PersistValue
scientificToPersistVal s = 
    case floatingOrInteger s of
      Left  r -> PersistDouble r
      Right i -> PersistInt64 i

-- | Piggy-back on the underlying Show instance (as the last option).
showV :: (Show a) => a -> Value
showV = String . pack . show

-------------------------------------------------------------------------------
-- Exception handling
-------------------------------------------------------------------------------

catchException :: SomeException -> RouteResponse
catchException e = 
    case fromException e of
      Just e1 -> catchDbErrors e1
      Nothing -> error "Uncaught exception."
 
catchDbErrors SqlError{ sqlState = sqls } = 
    case sqls of
      "23503" -> errorResponse ErrorSqlConstraintViolation 
                    "Foreign key constraint violation."
      "23505" -> errorResponse ErrorSqlUniqueViolation     
                    "Unique constraint violation."
      "42P01" -> errorResponse ErrorSqlGeneric             
                    "Undefined table."
      -- @todo: Add more error types here!
      _       -> errorResponse ErrorSqlGeneric             
                    "Unknown SQL error."
