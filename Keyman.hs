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
import System.Console.GetOpt
import System.Directory
import System.Entropy
import System.Environment
import System.IO

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C8

getKeys :: Connection -> IO [(String, String)]
getKeys conn = runQ $ query_ conn "SELECT client, key FROM trombone_keys ORDER BY client;"

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
isHex = C8.all (`elem` "0123456789abcdefABCDEF") 

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
                    "Table 'trombone_keys' does not exist. Please run the \
                    \server with HMAC authentication enabled (default mode) \
                    \to set up the schema. Alternatively, confirm that your \
                    \configuration file contains the correct database name \
                    \(dbname)."
                "23505" -> error
                    "A client is already registered with the specified name." 
                _ -> error $ C8.unpack m ++ " (" ++ C8.unpack s ++ ")"
        Right r -> return r

data Flag = Config String | Help
    deriving (Eq)

run :: [OptDescr Flag] -> [String] -> IO ()
run options = f . getOpt Permute options 
  where
    f (a,n,[]) | Help `elem` a  = putStrLn (usageInfo usage [])
               | otherwise = getHomeDirectory >>= invokeCmd n . getConfig a
    f (_,_,e) = ioError $ userError (concat e ++ "\n" ++ usageInfo usage [])

invokeCmd :: [String] -> FilePath -> IO ()
invokeCmd xs file = do
    file <- BS.readFile file
    conn <- connectPostgreSQL file
    hash <- liftM encode (getEntropy 20)
    case xs of
        [ "list"                   ] -> listKeys  conn
        [ "register" , client      ] -> insertKey conn (C8.pack client) hash
        [ "register" , client, key ] -> insertKey conn (C8.pack client) (C8.pack key)
        [ "register"               ] -> err [header, regRenewUsage, "\n", options]
        [ "renew"    , client      ] -> renewKey  conn (C8.pack client) hash
        [ "renew"    , client, key ] -> renewKey  conn (C8.pack client) (C8.pack key)
        [ "renew"                  ] -> err [header, regRenewUsage, "\n", options]
        [ "revoke"   , client      ] -> revokeKey conn (C8.pack client)
        [ "revoke"                 ] -> err [header, revokeUsage, "\n", options]
        _                            -> err [usage]
  where
    err info = ioError $ userError $ usageInfo ("\n" ++ concat info) []

getConfig :: [Flag] -> FilePath -> String
getConfig [] hd             = hd ++ "/.config/trombone/keyman.conf"
getConfig (Config file:_) _ = file
getConfig (_:xs) hd         = getConfig xs hd

listUsage, regRenewUsage, revokeUsage, helpUsage, options, header, usage :: String

listUsage     = "  keyman list [--config=<file>]\n"
regRenewUsage = "  keyman (register|renew) <client> [<key>] [--config=<file>]\n"
revokeUsage   = "  keyman revoke <client> [--config=<file>]\n"
helpUsage     = "  keyman --help\n"
options       = "Options:\n\
                \  -c --config=<file>  Path to database connection file.\n\
                \  -? --help           Display this help."
header        = "Usage:\n"

usage         = header
              ++ listUsage
              ++ regRenewUsage
              ++ revokeUsage
              ++ helpUsage ++ "\n"
              ++ options

main :: IO ()
main = do
    argv <- getArgs
    run [ Option "c" ["config"] (ReqArg Config "FILE") "" 
        , Option "?" ["help"]   (NoArg Help)           "" 
        ] argv

