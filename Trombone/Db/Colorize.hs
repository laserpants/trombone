{-# LANGUAGE OverloadedStrings #-}
module Trombone.Db.Colorize 
    ( colorize 
    ) where

import Data.Text                           ( Text, pack, unpack, stripStart, toLower )
import Data.List                           ( intersperse )
import System.Console.ANSI

import qualified Data.Text as Text

data SqlString = SqlItem    Text
               | StringItem Text
  deriving (Show)

data Label = Keyword   Text
           | Delim     Text
           | Default   Text
           | StringLit Text
  deriving (Show)

colorCode :: ColorIntensity -> Color -> Text
{-# INLINE colorCode #-}
colorCode i c = pack $ setSGRCode [SetColor Foreground i c]

encode :: [Label] -> Text
encode ls = stripStart $ Text.concat $ map decorate ls 
          ++ [colorCode Vivid White]

decorate :: Label -> Text
decorate (Keyword   t) = Text.concat [" ", colorCode Vivid Cyan, t] 
decorate (Delim   ",") = Text.concat [colorCode Vivid Red, ","]
decorate (Delim   ";") = Text.concat [colorCode Vivid Red, ";"]
decorate (Delim     d) = Text.concat [" ", colorCode Vivid Red, d] 
decorate (Default   t) = 
    case Text.split (== '.') t of
        [a, b] -> Text.concat [ " "
                             , colorCode Vivid Green, a
                             , colorCode Vivid White
                             , "."
                             , b ] 
        xs -> Text.concat $ [" ", colorCode Vivid White] ++ xs
decorate (StringLit t) = Text.concat [ colorCode Vivid White  , " '"
                                     , colorCode Vivid Yellow , t
                                     , colorCode Vivid White  , "'" ]

tokens :: [SqlString] -> [Label]
tokens = concatMap f 
  where 
    f :: SqlString -> [Label]
    f (StringItem t) = [StringLit t]
    f (SqlItem    t) = map label $ concatMap Text.words 
                                 $ concatMap (bup ';') 
                                 $ bup ',' t
    bup :: Char -> Text -> [Text]
    bup c = filter (/= "") . intersperse (pack [c]) . Text.split (== c) 

label :: Text -> Label
{-# INLINE label #-}
label t | isKeyword (toLower t) = Keyword t
        | isDelimOp t           = Delim   t
        | otherwise             = Default t

isDelimOp :: Text -> Bool
isDelimOp t = t `elem`
    [ "="
    , ","
    , ";"
    , "("
    , ")"
    , "+"
    , "-"
    , "<>"
    , ">"
    , ">="
    , "<"
    , "<=" ]

isKeyword :: Text -> Bool
isKeyword t = t `elem` 
     [ "select"
     , "insert"
     , "update"
     , "delete"
     , "from"
     , "into"
     , "values"
     , "where"
     , "and"
     , "set"
     , "order"
     , "by"
     , "in"
     , "join"
     , "on"
     , "not"
     , "limit" ]

takeApart :: Text -> [SqlString]
takeApart s = filter nonempty $ reverse $ f s' [] SqlItem StringItem
  where s' = Text.split (=='\'') s
        f [] ys _ _ = ys
        f (x:xs) ys c d = f xs (c x:ys) d c
        nonempty (SqlItem    "") = False
        nonempty (StringItem "") = False
        nonempty _               = True

colorize :: Text -> Text
{-# INLINE colorize #-}
colorize = encode . tokens . takeApart

