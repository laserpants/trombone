{-# LANGUAGE OverloadedStrings #-}
module Trombone.Encryption 
    ( encrypted 
    ) where

import Control.Monad                                   ( join )
import Crypto.Cipher.AES                               ( AES(..), encryptECB, initAES )
import Data.Aeson                                      ( ToJSON, encode )
import Data.ByteString                                 ( ByteString )
import Data.ByteString.Builder                                 
import Data.ByteString.Lazy.Char8                      ( fromStrict, toStrict )
import Data.List.Utils                                 ( addToAL )
import Network.HTTP.Types                              
import Network.Wai                                     ( Response, responseLBS )
import Network.Wai.Internal
import Trombone.Dispatch.Core
import Trombone.Hmac                                   ( generateHmac )
import Trombone.Response
import Trombone.Server.Config                          ( lookupKey, versionH )

import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy.Char8  as L8

encrypted :: RouteResponse -> Dispatch Response
encrypted r@(RouteResponse _ 401 _) = return $ sendJsonResponse r
encrypted resp = do
    Context{ dispatchRequest = req } <- ask
    case clientInfo $ requestHeaders req of
      (Just client, Just user) -> userEncrypt resp client user
      (Just client, Nothing  ) -> clientEncrypt resp client
      _                        -> return $ sendJsonResponse unauthorized

clientInfo :: [Header] -> (Maybe ByteString, Maybe ByteString)
clientInfo headers = (cname client, user)
  where client  = lookup "API-Access" headers
        user    = lookup "API-User" headers
        cname (Just n) = case C8.split ':' n of
                           [client, _] -> Just client
                           _           -> Nothing
        cname Nothing = Nothing

-- | Encrypt the response using the user's hashed password as key.
userEncrypt :: RouteResponse -> ByteString -> ByteString -> Dispatch Response
userEncrypt resp _ user = 
    return $ case C8.split ':' user of
               [uname, pass] -> cbResponse pass resp
               _             -> sendJsonResponse unauthorized

-- | Encrypt the response using a MAC generated from the client name and
-- stored encryption key.
clientEncrypt :: RouteResponse -> ByteString -> Dispatch Response
clientEncrypt resp client = do
    Context{ dispatchKeys = keys } <- ask
    return $ case join $ fmap (lookupKey client) keys of
               Nothing  -> sendJsonResponse unauthorized
               Just key -> cbResponse (generateHmac key client) resp

cbResponse :: ByteString -> RouteResponse -> Response
cbResponse key (RouteResponse hs st val) = 
    if C8.length key < 32
        then sendJsonResponse unauthorized
        else let aes = initAES $ C8.take 32 key in
                responseLBS ok200 (headers hs) 
                    $ toLazyByteString $ byteStringHex $ codeBook aes val

headers :: [(HeaderName, ByteString)] -> [(HeaderName, ByteString)]
headers = foldr f [("Content-Type", "text/plain"), versionH] 
  where f (k, v) a = addToAL a k v
 
codeBook :: ToJSON a => AES -> a -> ByteString
codeBook aes = encryptECB aes . pad . toStrict . encode 
  where bits   = flip mod 16 . negate 
        pad bs = C8.concat $ bs:replicate (bits $ C8.length bs) space
        space  = C8.singleton ' '
 
