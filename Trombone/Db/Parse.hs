{-# LANGUAGE OverloadedStrings #-}
module Trombone.Db.Parse 
    ( parseDbTemplate
    , arbitrary
    ) where

import Data.Text                                       ( Text, pack, unpack )
import Trombone.Db.Template

import qualified Data.Text                             as Text

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
parseDbTemplate = DbTemplate . consume . tokenize . endlchar

-- | Append a semicolon at the end of the string, unless one is already present.
endlchar :: Text -> Text
endlchar x | ';' == Text.last x = x
           | otherwise         = Text.append x ";"

-- | Create an "arbitrary" template instance (for the purpose of extracting
-- various meta-data from the statement using the reflection utilities).
arbitrary :: DbTemplate -> String
arbitrary (DbTemplate segms) = foldr f "" segms
  where f (DbSqlStatic    t) b = unpack t ++ b
        f (DbSqlUriParam  t) b = "'?'" ++ b
        f (DbSqlJsonValue t) b = "'?'" ++ b

