{-# LANGUAGE OverloadedStrings #-}
module Trombone.RoutePattern where

import Data.Text                                       ( Text )

import qualified Data.Text                             as Text

-- | A string literal or a placeholder variable name.
data RouteSegment = Atom Text | Variable Text
    deriving (Show, Eq)

-- | A route pattern is a list of segments.
newtype RoutePattern = RoutePattern [RouteSegment]
    deriving (Show, Eq)

-- | The result from matching a uri with a route pattern.
data RouteMatch = NoMatch | Params [(Text, Text)]
    deriving (Show, Eq)

-- | Match a route pattern against a list of uri path segments and return 
-- either a NoMatch or the list of key-value pairs with the extracted uri 
-- parameter values.
match :: RoutePattern -> [Text] -> RouteMatch
match (RoutePattern segms) ts | length ts /= length segms = NoMatch
match (RoutePattern segms) ts = foldr f (Params []) $ zip segms ts 
  where 
    f _ NoMatch = NoMatch
    f (Atom     a, t) (Params ps) | a == t     = Params ps
                                  | otherwise = NoMatch
    f (Variable v, t) (Params ps) = Params ((v, t):ps)

-- | Translate the uri to component form.
decompose :: Text -> RoutePattern
decompose = RoutePattern . map f . filter blanks . Text.splitOn "/" 
  where 
    f p | ':' == Text.head p = Variable $ Text.tail p
        | otherwise         = Atom p

-- | Predicate to filter out blank uri segments.
blanks :: Text -> Bool
{-# INLINE blanks #-}
blanks ""  = False
blanks ":" = False
blanks _   = True

