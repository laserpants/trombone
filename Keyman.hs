{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Exception
import Control.Monad                       ( liftM )
import Data.ByteString                     ( ByteString )
import Data.ByteString.Base16
import Data.List.Utils                     ( replace )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import GHC.Int
import System.Entropy
import System.Environment
import System.IO

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8

getKeys :: Connection -> IO [(String, String)]
getKeys conn = runQ $ query_ conn "SELECT client, key FROM trombone_keys;"

maxlen :: [String] -> Int
maxlen = foldr f 0 
  where
    f x n | n' > n    = n'
          | otherwise = n
      where n' = length x

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '

listKeys :: Connection -> IO ()
listKeys conn = do
    ys <- getKeys conn

    let fs = map fst ys
        n  = maxlen fs
        xs = map (pad $ n + 1) fs

    mapM_ (f n) $ zipWith (\a b -> (a, snd b)) xs ys 
  where
    f n (k, v) = putStrLn $ k ++ ": " ++ v 

isHex :: ByteString -> Bool
isHex = C8.all (flip elem "0123456789abcdefABCDEF") 

isValid :: ByteString -> Either String ByteString
isValid key | BS.length key /= 40 = Left "Invalid key length: Please specify a 40-character hexadecimal string."
            | not (isHex key)      = Left "Invalid key format: Please specify a 40-character hexadecimal string."
            | otherwise          = Right key

insertKey :: Connection -> ByteString -> ByteString -> IO ()
insertKey conn client key = f >>= \r ->
    case r of
        1 -> do
            putStrLn "Client registered:"
            putStrLn $ C8.unpack client ++ ": " ++ C8.unpack key
        _ -> error "Error registering client."
  where f = case isValid key of
              (Left  e) -> error e
              (Right k) -> runQ $ execute_ conn $ Query $ BS.concat
                  [ "INSERT INTO trombone_keys (client, key) VALUES ('"
                  , client , "', '"
                  , k      , "');" ]

renewKey :: Connection -> ByteString -> ByteString -> IO ()
renewKey conn client key = f >>= \r ->
    case r of
        1 -> do
            putStrLn "Client key renewed:"
            putStrLn $ C8.unpack client ++ ": " ++ C8.unpack key
        _ -> error "No such client."
  where f = case isValid key of
              (Left  e) -> error e
              (Right k) -> runQ $ execute_ conn $ Query $ BS.concat
                  [ "UPDATE trombone_keys SET key = '"
                  , k      , "' WHERE client = '"
                  , client , "';" ]

revokeKey :: Connection -> ByteString -> IO ()
revokeKey conn client = f >>= \r ->
    case r of
        1 -> putStrLn $ "Client key revoked: " ++ C8.unpack client 
        _ -> error "No such client."
  where 
    f = runQ $ execute_ conn $ Query $ BS.concat 
        [ "DELETE FROM trombone_keys WHERE client = '", client, "';" ]

runQ :: IO a -> IO a
runQ q = try q >>= \r ->
    case r of
        Left (SqlError s _ m d h) -> 
            case C8.unpack s of
                "42P01" -> error 
                    "Table 'trombone_keys' could not be found. Please run \
                    \./trombone with HMAC authentication enabled (default \
                    \mode) to set up tables. Alternatively, confirm that \
                    \your configuration file contains the correct database \
                    \name (dbname)."
                _ -> error $ C8.unpack m
        Right r -> return r

runWithArgs :: [String] -> Connection -> IO ()
runWithArgs args conn = do
    hash <- liftM encode (getEntropy 20)
    case args of
        [ "list"                   ] -> listKeys  conn
        [ "register" , client      ] -> insertKey conn (C8.pack client) hash
        [ "register" , client, key ] -> insertKey conn (C8.pack client) (C8.pack key)
        [ "register"               ] -> usage "Usage: %1 register [client] | register [client key]" 
        [ "renew"    , client      ] -> renewKey  conn (C8.pack client) hash
        [ "renew"    , client, key ] -> renewKey  conn (C8.pack client) (C8.pack key)
        [ "renew"                  ] -> usage "Usage: %1 renew [client] | renew [client key]" 
        [ "revoke"   , client      ] -> revokeKey conn (C8.pack client)
        [ "revoke"                 ] -> usage "Usage: %1 revoke [client]" 
        _                            -> usage "Usage: %1 {list|register|renew|revoke} [client [key]]" 
  where
    usage s = getProgName >>= putStrLn . flip (replace "%1") s 

main :: IO ()
main = do
    args <- getArgs
    ( BS.readFile "keyman.conf" 
         >>= connectPostgreSQL 
         >>= runWithArgs args ) 

