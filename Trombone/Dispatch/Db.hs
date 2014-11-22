{-# LANGUAGE OverloadedStrings #-}
module Trombone.Dispatch.Db 
    ( dispatchDbAction
    , escVal
    ) where

import Control.Applicative
import Control.Arrow                                   ( second )
import Control.Exception.Lifted                        ( SomeException, try, fromException )
import Control.Monad                                   ( liftM, when )
import Control.Monad.IO.Class                          ( liftIO )
import Control.Monad.Trans                             ( lift )
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Foldable                                   ( forM_ )
import Data.List                                       ( intersperse )
import Data.Maybe                                      ( fromMaybe, mapMaybe )
import Data.Monoid                                     ( (<>) )
import Data.Text                                       ( Text, pack, unpack, stripStart )
import Data.Vector                                     ( Vector, fromList, toList )
import Database.Persist
import Database.PostgreSQL.Simple                      ( SqlError(..) )
import System.Log.FastLogger
import Trombone.Db.Execute
import Trombone.Db.Template 
import Trombone.Dispatch.Core
import Trombone.Response

import qualified Data.HashMap.Strict                   as HMS
import qualified Data.Text                             as Text
import qualified Data.Vector                           as Vect

dispatchDbAction :: DbQuery 
                 -> [(Text, EscapedText)] 
                 -> Value 
                 -> Dispatch IO RouteResponse
dispatchDbAction q ps (Array a) = 
    -- Run a sequence of actions and collect their results
    liftM resp $ mapM (dispatchDbAction q ps) (Vect.toList a)
  where val (RouteResponse _ _ x) = x
        -- Using response code 202 is to indicate that the result
        -- of each individual request must be considered separately
        -- and that no claim is made as to the state of success 
        -- w.r.t. these.
        resp = RouteResponse [] 202 . Array . Vect.fromList . map val
dispatchDbAction q ps (Object o) = 
    run q $ ps ++ map (second escVal) (HMS.toList o)
dispatchDbAction q ps _ = run q ps

run :: DbQuery -> [(Text, EscapedText)] -> Dispatch IO RouteResponse
run (DbQuery ret tpl) ps = do
    Context pool _ _ _ loud _ <- ask
    case instantiate tpl ps of
        Left e -> 
            -- 400 Bad request: Request parameters do not match the template
            return $ errorResponse ErrorBadRequest $ Text.concat
                [ "Error: Template parameter list undersaturated. \
                  \Arguments missing: "
                , Text.concat $ intersperse ", " $ map arg e, "." ]
        Right q -> do
            let q' = stripStart q
            printS (unpack q') -- Verbose output
            logSql $ q' <> "\n"
            getDbResponse ret q
  where 
    arg :: Text -> Text
    arg x | Text.null x       = x
          | ':' == Text.head x = quoute $ Text.tail x
          | otherwise         = quoute x

-- | Run a database action in the Dispatch monad.
runDbDispatch :: SqlT a -> Dispatch IO a
runDbDispatch sql = ask >>= \Context{ dispatchPool = pool } -> lift $ runDb sql pool

-- | Run a database query and respond with the appropriate type of result.
getDbResponse :: DbResult -> Text -> Dispatch IO RouteResponse
-- A simple 200 OK response is sufficient.
getDbResponse NoResult q = runDbDispatch (void q) >> return (okResponse [])
-- Respond with a single item, or a 404 error.
getDbResponse (Item ns) q = liftM f $ runDbDispatch (item q ns) 
  where 
    f Nothing   = errorResponse ErrorNotFound "Resource not found."
    f (Just ok) = RouteResponse [] 200 ok
-- Ok response with default message and status properties
getDbResponse (ItemOk ns) q = getDbResponse (Item ns) q >>= \r -> 
    return $ case r of
               RouteResponse hs 200 (Object o) -> okResponse $ HMS.toList o 
               _                               -> r
-- Respond with a collection.
getDbResponse (Collection ns) q = liftM f $ runDbDispatch (collection q ns)
  where f = RouteResponse [] 200 . Array . fromList 
-- Respond with "last insert id"
getDbResponse (LastInsert table seq) q = do
    runDbDispatch (void q) 
    getDbResponse (ItemOk ["id"]) last
  where 
    last = Text.concat ["SELECT currval(pg_get_serial_sequence('"
                       , table, "', '", seq, "'))" ]
-- Respond with the row count in a result.
getDbResponse Count q = do
    r <- runDbDispatch (executeCount q) 
    return $ okResponse [("rows", Number $ fromIntegral r)]

