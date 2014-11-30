{-# LANGUAGE OverloadedStrings #-}
module Trombone.Hmac 
    ( Keys
    , ClientIdentity(..)
    , generateHmac 
    , authenticate
    ) where

import Control.Applicative                             ( (<$>), (<*>) )
import Control.Monad                                   ( msum, guard )
import Crypto.Hash                 
import Data.ByteString                                 ( ByteString, unpack )
import Data.HashMap                                    ( Map )
import Data.Maybe                                      ( fromMaybe )
import Data.Text                                       ( Text )
import Data.Text.Encoding                              ( decodeUtf8 )
import Network.Socket
import Network.Wai

import qualified Data.ByteString.Char8                 as C8
import qualified Data.HashMap                          as Map

-- | Compute a MAC using the SHA1 cryptographic algorithm.
generateHmac :: ByteString -> ByteString -> ByteString
generateHmac key = digestToHexByteString . hmacGetDigest . sha1Hmac key 
  where 
    sha1Hmac :: ByteString -> ByteString -> HMAC SHA1
    sha1Hmac = hmac
 
-- | User agent identity.
data ClientIdentity = 
    Anonymous    -- ^ Unknown user agent
  | Local        -- ^ Application running on the local server
  | Client Text  -- ^ Authenticated and trusted user agent
  | Untrusted    -- ^ A request which is invalid, fraudulent, or originates 
                 --   from an untrusted source
    deriving (Show)

type Keys = Map ByteString ByteString

-- | Establish the identity of the user agent from which the request originates.
authenticate :: Request         -- ^ Request object
             -> ByteString      -- ^ The raw request body
             -> Bool            -- ^ Is HMAC authentication enabled?
             -> Bool            -- ^ Always trust requests from localhost?
             -> Bool            -- ^ Allow ping requests to "pass through"?
             -> Keys            -- ^ Client keys
             -> ClientIdentity

authenticate req body hmacEnabled trustLocal allowPing keys =
    fromMaybe Untrusted $ msum 
        [ authHmac'
        -- ^ Validate the request's API-Access header, if one is present
        , authLocal 
        -- ^ Authenticate requests from applications running on the same server
        , authPing
        -- ^ Authenticate anonymous ping requests 
        , authAnon
        -- ^ Finally, if HMAC is disabled, all requests are honored
        ]
  where 
    authHmac', authLocal, authPing, authAnon :: Maybe ClientIdentity
    authHmac' | not hmacEnabled            = Nothing
              | otherwise                = authHmac req body keys
    authLocal | isLocal && trustLocal     = Just Local
              | otherwise                = Nothing
    authPing  | isPing req && allowPing   = Just Anonymous
              | otherwise                = Nothing
    authAnon  | not hmacEnabled            = Just Anonymous
              | otherwise                = Nothing
    isLocal = isLocalhost $ remoteHost req 

-- | Perform a cryptographic message integrity check using the HMAC encapsulated 
-- in the request's "API-Access" header. A MAC is computed from the request body 
-- using a stored key associated with the client application from which the 
-- request claims to originate. The result is then compared to the code found in
-- the header field in order to establish the authenticity of the request.
authHmac :: Request -> ByteString -> Keys -> Maybe ClientIdentity
authHmac req body keys = macCheck <$> lookup "API-Access" (requestHeaders req)
  where 
    macCheck :: ByteString -> ClientIdentity
    macCheck h = maybe Untrusted Client $ do
        (client, code) <- extractClientInfo h
        key <- Map.lookup client keys
        guard (generateHmac key body == code) >> Just (decodeUtf8 client)

extractClientInfo :: ByteString -> Maybe (ByteString, ByteString)
extractClientInfo = ext . C8.split ':' 
  where
    ext [client, code] = Just (client, code)
    ext _              = Nothing

isPing :: Request -> Bool
{-# INLINE isPing #-}
isPing req | null info = False
           | otherwise = "ping" == head info
  where
    info = pathInfo req

isLocalhost :: SockAddr -> Bool
{-# INLINE isLocalhost #-}
isLocalhost (SockAddrInet  _   ha   ) = 16777343  == ha
isLocalhost (SockAddrInet6 _ _ ha6 _) = (0,0,0,1) == ha6
isLocalhost _                         = True

