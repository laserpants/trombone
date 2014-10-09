{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Trombone.Pipeline
    ( module Pipe
    , Connection(..)
    , Filter(..)
    , Message(..)
    , Pipeline(..)
    , Predicate(..)
    , Processor(..)
    , ProcessorId(..)
    , TransType(..)
    , Transformer(..)
    , broadcast
    , buildJsonRequest
    , connections
    , expand
    , outgoingConns
    , processorMsgs 
    ) where

import Control.Applicative
import Data.Aeson                            as Pipe
import Data.Maybe                                      ( fromMaybe, mapMaybe )
import Data.Text                             as Pipe   ( Text, cons )
import Network.HTTP.Types                              
import Trombone.Db.Template                  as Pipe
import Trombone.RoutePattern

import qualified Data.HashMap.Strict         as HMS
import qualified Data.Vector                 as Vect

data TransType = TransExclude
               | TransInclude
               | TransBind
               | TransRename
               | TransCopy
               | TransAggregate
    deriving (Eq, Ord, Show)

data Transformer = Transformer TransType [Value]
    deriving (Eq, Show)

data Predicate = PredEqualTo
               | PredNotEqualTo
               | PredGreaterThan
               | PredGreaterThanOrEqual
               | PredLessThan
               | PredLessThanOrEqual
    deriving (Show)

data Filter = Filter
    { property  :: Text
    , predicate :: Predicate
    , value     :: Value
    } deriving (Show)

data Processor = Processor 
    { processorId     :: Int            -- ^ A unique identifier
    , processorFields :: [Text]         -- ^ List of input fields
    , processorMethod :: Method         -- ^ Any valid HTTP method
    , processorUri    :: RoutePattern   -- ^ A route pattern
    , processorExpand :: Maybe Text     -- ^ An optional "expansion" property
    } deriving (Show)

data Connection = Connection
    { source       :: ProcessorId
    , destination  :: ProcessorId
    , transformers :: [Transformer]
    , filters      :: [Filter]
    } deriving (Show)

data ProcessorId = Id Int | In | Out
    deriving (Eq, Show)

data Message = Message ProcessorId Object
    deriving (Eq, Show)

type MessageQueue = [Message]

data Pipeline = Pipeline [Processor]
                         [Connection]
                         MessageQueue
    deriving (Show)

broadcast :: [Connection] -> Value -> [Message]
broadcast conns (Object o) = mapMaybe f conns
  where f :: Connection -> Maybe Message
        f (Connection _ dest funs filters) = 
            Message <$> Just dest 
                    <*> (foldr runTransformer <$> applyFilters o filters 
                                              <*> Just funs)
broadcast conns (Array a) = concatMap (broadcast conns) (Vect.toList a)
broadcast conns Null      = broadcast conns (Object HMS.empty)
broadcast _ _             = []

applyFilters :: Object -> [Filter] -> Maybe Object
applyFilters = foldr runFilter . Just 

runFilter :: Filter -> Maybe Object -> Maybe Object
runFilter Filter{..} (Just o) = 
    case HMS.lookup property o of
        Nothing -> Nothing
        Just v  -> if compare v value then Just o else Nothing
  where compare :: Value -> Value -> Bool
        compare (Number v1) (Number v2) = 
            case predicate of
                PredEqualTo            -> v1 == v2
                PredNotEqualTo         -> v1 /= v2
                PredGreaterThan        -> v1 > v2
                PredGreaterThanOrEqual -> v1 >= v2
                PredLessThan           -> v1 < v2
                PredLessThanOrEqual    -> v1 <= v2
        compare v1 v2 = 
            case predicate of
                PredEqualTo            -> v1 == v2
                PredNotEqualTo         -> v1 /= v2
                _                      -> False
runFilter _ _ = Nothing

runTransformer :: Transformer -> Object -> Object
runTransformer (Transformer TransExclude args) o = HMS.filterWithKey pred o
  where pred k _ = String k `notElem` args
runTransformer (Transformer TransInclude args) o = HMS.filterWithKey pred o
  where pred k _ = String k `elem` args
runTransformer (Transformer TransRename (String from:String to:_)) o =
    case HMS.lookup from o of
      Nothing -> o
      Just v  -> HMS.insert to v $ HMS.delete from o
runTransformer (Transformer TransCopy (String from:String to:_)) o =
    case HMS.lookup from o of
      Nothing -> o
      Just v  -> HMS.insert to v o
runTransformer (Transformer TransBind (String key:val:_)) o = HMS.insert key val o
runTransformer (Transformer TransAggregate (String key:_)) o = HMS.fromList [(cons '*' key, Object o)]
runTransformer _ o = o

expand :: Maybe Text -> Object -> Value
expand Nothing  obj = Object obj
expand (Just v) obj = 
    case HMS.lookup v obj of
        Just x  -> f x
        Nothing -> Object obj
  where f (Object o) = Object $ HMS.union o obj
        f (Array  a) = Array  $ Vect.map f a
        f x          = x

buildJsonRequest :: [Message] -> Object
buildJsonRequest = foldr f HMS.empty 
  where f (Message _ o1) = HMS.union o1 

-- | Return the subset of the message queue relevant to a specific processor.
processorMsgs :: ProcessorId -> MessageQueue -> MessageQueue
processorMsgs p = filter $ \(Message pid _) -> pid == p

-- | Count the number of source or destination endpoints connected to a specific
-- processor in the given list of connections.
connections :: ProcessorId -> [Connection] -> (Connection -> ProcessorId) -> Int
connections _ [] _ = 0
connections p (x:xs) f | p == f x   = 1 + connections p xs f
                       | otherwise =     connections p xs f

-- | Return all connections for which the source is the specified processor.
outgoingConns :: ProcessorId -> [Connection] -> [Connection]
outgoingConns p = filter $ (==) p . source 

