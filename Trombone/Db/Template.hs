{-# LANGUAGE OverloadedStrings #-}
module Trombone.Db.Template 
    ( DbResult(..)
    , DbSqlSegment(..)
    , DbTemplate(..)
    , DbQuery(..)
    , EscapedText(..)
    , instantiate
    , instantiateQ
    ) where

import Data.Aeson
import Data.Text                                       ( Text, append, pack )

import qualified Data.Text                             as Text

-- | The type of result produced by an SQL statement.
data DbResult = NoResult 
              | Item       [Text]
              -- ^ Return a single item, or a 404 error message
              | ItemOk     [Text]
              -- ^ Same as Item, except with added 'Ok' status message 
              | Collection [Text]
              -- ^ Return a collection of items
              | LastInsert  Text    -- ^ Table name
                            Text    -- ^ Sequence
              -- ^ Return the last inserted id (for INSERT statements)
              | Count
              -- ^ Return a row count result

-- | A part of a query template.
data DbSqlSegment 
    = DbSqlStatic    Text -- ^ A normal text segment 
    | DbSqlUriParam  Text -- ^ A uri variable placeholder, e.g., "id"
    | DbSqlJsonValue Text -- ^ A parameterized JSON value 
    deriving (Show)

-- | A complete SQL query template.
--
-- e.g., 
--     "select id from dispatch where id = {{id}}" 
-- has the form
--     DbTemplate [ DbSqlStatic "select id from dispatch where id = "
--                , DbSqlUriParam "id" ]
newtype DbTemplate = DbTemplate [DbSqlSegment]
    deriving (Show)

-- | A database query.
--
-- e.g.,
--     "select name, phone, address from customer"
-- would be represented as
--     DbQuery 
--         (DbResult Collection ["name", "phone", "address"])
--         (DbTemplate [DbSqlStatic "select name, phone, address from customer"])
data DbQuery = DbQuery DbResult DbTemplate

-- | Represents a properly escaped and quouted placeholder value.
newtype EscapedText = EscapedText Text
    deriving (Show)

-- | Instantiate a template using the given lookup table of uri parameters and 
-- JSON data. The result is either a list of placeholder values that failed to 
-- match any key in the association lists, or the final query instance. Note 
-- that all uri parameters should have their name prefixed by a colon in the 
-- lookup list to avoid namespace collisions.
instantiate :: DbTemplate          
            -- ^ The query template
            -> [(Text, EscapedText)]      
            -- ^ uri variable and JSON key-value pairs
            -> Either [Text] Text  
            -- ^ A list of non-matching keys, or the query
instantiate (DbTemplate segms) table = subst segms "" []
  where subst [] txt es = if null es then Right txt else Left es
        -- A static string segment is appended to the query, just as it is
        subst (DbSqlStatic t:ts) txt [] = subst ts (append txt t) []
        -- If errors are present, we need not worry about the query
        subst (DbSqlStatic _:ts) _ es = subst ts "" es
        subst (t:ts) txt es = 
            let key = f t in
            case lookup key table of
                 Nothing -> subst ts "" (key:es)
                 Just (EscapedText t') -> subst ts (if null es then append txt t' else "") es
        f (DbSqlUriParam  txt) = Text.cons ':' txt
        f (DbSqlJsonValue txt) = txt

instantiateQ :: DbQuery -> [(Text, EscapedText)] -> Either [Text] Text
{-# INLINE instantiateQ #-}
instantiateQ (DbQuery _ tpl) = instantiate tpl

