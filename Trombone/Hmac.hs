{-# LANGUAGE OverloadedStrings #-}
module Trombone.Hmac where

import Data.ByteString                                 ( ByteString )
import Data.ByteString.Lazy                            ( fromStrict )
import Data.Digest.Pure.SHA
import Data.Monoid                                     ( mconcat )
import Data.Text                                       ( Text )
import Data.Text.Encoding                              ( decodeUtf8 )
import Network.Socket
import Trombone.Dispatch.Core
import Trombone.Response

import qualified Data.ByteString.Char8       as C8
import qualified Data.HashMap                as Map

-- | Client identification stub.
data ClientId = Anonymous | Local | Client Text

-- | Perform a cryptographic message integrity check based on the MAC code 
-- attached to the request in the form of an "API-Access" header. A subsequent 
-- code is computed from the request object using a stored key associated with 
-- the client application. The result is then compared to the MAC attached to 
-- the request in order to establish the authenticity of the request.
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
                     Nothing -> if isPing req
                                   -- Allow "ping" requests to pass through
                                   then return $ Right Anonymous
                                   else liftM Left unauthorized 
                     Just h  -> case C8.split ':' h of
                                [client, code] -> 
                                    case lookupKey client conf of
                                        Just key -> authorize client code key
                                        Nothing  -> liftM Left unauthorized 
                                _ -> liftM Left unauthorized
        authorize client code key = 
            let secret = fromStrict key
                msg    = fromStrict body in
            if showDigest (hmacSha1 secret msg) /= C8.unpack code
                then liftM Left unauthorized
                else return $ Right $ Client (decodeUtf8 client)
  
isPing :: Request -> Bool
isPing req = not (null info) && "ping" == head info
  where info = pathInfo req

unauthorized :: Dispatch RouteResponse
unauthorized = return $ errorResponse ErrorUnauthorized "Unauthorized."

isLocalhost :: SockAddr -> Bool
isLocalhost (SockAddrInet  _   ha   ) = 16777343  == ha
isLocalhost (SockAddrInet6 _ _ ha6 _) = (0,0,0,1) == ha6
isLocalhost _                         = True

