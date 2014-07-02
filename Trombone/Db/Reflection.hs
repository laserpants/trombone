module Trombone.Db.Reflection where

import Data.Text                                       ( Text, pack, unpack )
import Database.HsSqlPpp.Ast
import Database.HsSqlPpp.Parser
import Trombone.Db.Parse
import Trombone.Db.Template

probeTemplate :: DbTemplate -> (Maybe Text, Maybe [Text])
probeTemplate = probe . arbitrary

probe :: String                      -- ^ A "raw" SQL SELECT or INSERT statement
      -> (Maybe Text, Maybe [Text])  -- ^ Table name and list of columns
probe x = case parseStatements "" x of
            Right [i@Insert{}]         -> (statmTable i, statmCols i)
            Right [QueryStatement _ s] -> (queryTable s, queryCols s)
            Right _                    -> (Nothing, Nothing)
            Left e                     -> error $ show e

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

