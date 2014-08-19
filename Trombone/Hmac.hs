{-# LANGUAGE OverloadedStrings #-}
module Trombone.Hmac 
    ( authenticate
    , generateHmac
    ) where

import Crypto.Hash                 
import Data.ByteString                                 ( ByteString, unpack )
import Data.ByteString.Lazy                            ( fromStrict, toStrict )
import Data.Byteable
import Data.Monoid                                     ( mconcat )
import Data.Text                                       ( Text )
import Data.Text.Encoding                              ( decodeUtf8 )
import Network.Socket
import Trombone.Dispatch.Core
import Trombone.Response
import Trombone.Server.Config

import qualified Data.ByteString.Char8       as C8
import qualified Data.HashMap                as Map

-- | Client identification stub.
data ClientId = Anonymous | Local | Client Text

-- | Perform a cryptographic message integrity check based on the MAC code 
-- attached to the request in the form of an "API-Access" header. A subsequent 
-- code is computed from the request object using a stored key associated with 
-- the client application. The result is then compared to the MAC attached to 
-- the request in order to establish its authenticity.
authenticate :: ByteString -> Dispatch (Either RouteResponse ClientId)
authenticate body = do
    Context{ dispatchRequest = req, dispatchKeys = keys } <- ask
    case keys of
        Nothing   -> return $ Right Anonymous  -- HMAC is disabled
        Just conf -> inspectHeaders req conf
  where inspectHeaders req conf = 
          if isLocalhost (remoteHost req) && allowLocal conf
              then return $ Right Local
              else case lookup "API-Access" $ requestHeaders req of
                     Nothing -> return $ if isPing req
                                            -- Allow "ping" requests to pass through
                                            then Right Anonymous
                                            else Left unauthorized 
                     Just h  -> case C8.split ':' h of
                                [client, code] -> 
                                    case lookupKey client conf of
                                        Just key -> authorize client code key
                                        Nothing  -> return $ Left unauthorized 
                                _ -> return $ Left unauthorized
        authorize client code key = 
            return $ if generateHmac key body /= code
                         then Left unauthorized
                         else Right $ Client (decodeUtf8 client)
  
-- | Compute a MAC using the SHA1 cryptographic algorithm.
generateHmac :: ByteString -> ByteString -> ByteString
generateHmac key = digestToHexByteString . hmacGetDigest . sha1Hmac key 
  where sha1Hmac :: ByteString -> ByteString -> HMAC SHA1
        sha1Hmac = hmac
 
isPing :: Request -> Bool
{-# INLINE isPing #-}
isPing req = not (null info) && "ping" == head info
  where info = pathInfo req

isLocalhost :: SockAddr -> Bool
{-# INLINE isLocalhost #-}
isLocalhost (SockAddrInet  _   ha   ) = 16777343  == ha
isLocalhost (SockAddrInet6 _ _ ha6 _) = (0,0,0,1) == ha6
isLocalhost _                         = True

