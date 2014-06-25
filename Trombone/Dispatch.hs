{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Trombone.Dispatch where

import "mtl" Control.Monad.Reader

import Control.Arrow                                   ( second )
import Data.Aeson
import Data.Maybe                                      ( listToMaybe, maybeToList, fromMaybe, mapMaybe )
import Data.Scientific
import Data.Text                                       ( Text, pack )
import Data.Text.Encoding
import Data.Vector                                     ( Vector, fromList, toList )
import Database.Persist
import Database.Persist.Postgresql                     hiding ( Sql )
import Network.Wai.Internal                            ( Request(..) )
import Trombone.Db.Execute
import Trombone.Db.Template
import Trombone.Response

import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Vector                           as Vect

-- | Various state required to process a request.
data Context = Context ConnectionPool Request

-- | Monad transformer in which requests are dispatched.
type Dispatch = ReaderT Context IO

-- | Run a database action in the Dispatch monad.
runDbDispatch :: Sql a -> Dispatch a
runDbDispatch sql = ask >>= \(Context pool _) -> lift $ runDb sql pool

-- | Run a database query and respond according to the specified result type.
getResponse :: DbResult -> Text -> Dispatch RouteResponse
-- A simple 200 OK response is sufficient.
getResponse NoResult q = runDbDispatch (noResult q) >> return (okResponse [])
-- Respond with a single item, or a 404 error.
getResponse (Item ns) q = do 
    r <- runDbDispatch (getOne q) 
    return $ case row ns $ concat $ maybeToList r of
               Nothing -> errorResponse ErrorNotFound "Resource not found."
               Just v  -> RouteResponse 200 v
-- Respond with a collection.
getResponse (Collection ns) q = liftM f $ runDbDispatch (getResult q)
  where f = RouteResponse 200 . Array . fromList . mapMaybe (row ns)
-- Respond with the number of rows in a result.
getResponse Count q = do
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
                                         -- ^ Anything we might have left out

-- | Fallback using the underlying Show instance.
showV :: (Show a) => a -> Value
showV = String . pack . show

persistValsToJsonArray :: [PersistValue] -> Value
persistValsToJsonArray = Array . fromList . map persistValToJsonVal

valuesToPersistList :: Vector Value -> PersistValue
valuesToPersistList = PersistList . map jsonValToPersistVal . toList

scientificToPersistVal :: Scientific -> PersistValue
scientificToPersistVal s = 
    case floatingOrInteger s of
      Left  r -> PersistDouble r
      Right i -> PersistInt64 i

