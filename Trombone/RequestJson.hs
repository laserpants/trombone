{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Trombone.RequestJson 
    ( RequestJson(..)
    , RequestInfo(..)
    , AuthFlags(..)
    , requestJson
    , authRequest
    ) where

import Control.Applicative                             ( (<$>), (<*>) )
import Control.Exception.Lifted                        
import Control.Monad                                   ( join, liftM )
import Control.Monad.IO.Class                          ( liftIO ) 
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Parser                               ( json' )
import Data.ByteString                                 ( ByteString )
import Data.Conduit
import Data.Conduit.Attoparsec                         ( Position, sinkParser, errorMessage, errorPosition )
import Data.Conduit.Internal                           ( zipSinks )
import Data.Maybe                                      ( isJust )
import Network.Wai
import Network.Wai.Conduit                             ( sourceRequestBody )
import Trombone.Dispatch.Core
import Trombone.Hmac
import Trombone.Route

import qualified Data.ByteString                       as BS
import qualified Data.Conduit.List                     as CL
import qualified Data.HashMap                          as Map

data RequestJson = EmptyBody 
                 -- ^ Returned if the request body was found to be empty
                 | RequestBodyError String Position
                 -- ^ Return error description and position if the JSON object 
                 --   failed to parse
                 | JsonBody Value ByteString
                 -- ^ A valid JSON-formatted request object was detected. The 
                 --   raw body is returned together with the parsed Aeson value.
    deriving (Show)

-- | Parse the request body to JSON and return a RequestJson value to indicate. 
-- the result.
requestJson :: Request -> IO RequestJson
requestJson req = 
    case requestBodyLength req of
      KnownLength 0 -> return EmptyBody
      _             -> parse 
  where
    parse :: IO RequestJson
    parse = do
        r <- try $ sourceRequestBody req $$ zipSinks CL.consume (sinkParser json') 
        return $ case r of
            Left e -> RequestBodyError (errorMessage e) (errorPosition e)
            Right (bs, v) -> JsonBody v (BS.concat bs)

-- | Authentication flags.
data AuthFlags = AuthFlags
    { authHmacEnabled :: Bool
    -- ^ Is HMAC authentication enabled? Highly recommended in a production
    --   environment.
    , authTrustLocal  :: Bool
    -- ^ If this flag is enabled, requests from applications running on the same
    --   server are always trusted.
    , authAllowPing   :: Bool 
    -- ^ Ping requests may be allowed to "pass through" without authentication.
    }

-- | JSON-formatted request object and user agent identity.
data RequestInfo = RequestInfo RequestJson ClientIdentity
    deriving (Show)

-- | Extract and parse the JSON object from the request body and return the 
-- result together with the identity of the participating user agent.
authRequest :: Request -> Dispatch IO RequestInfo
authRequest req = ask >>= liftIO . auth req

auth :: Request -> Context -> IO RequestInfo
auth req Context{..} = 
    liftM (flip RequestInfo <$> authId . takeBody 
                            <*> id) (requestJson req)
  where
    authId :: ByteString -> ClientIdentity
    authId body = authenticate req body 
                               (isJust dispatchKeys)
                               (maybe True allowLocal dispatchKeys) 
                               True (keys dispatchKeys)

keys :: Maybe HmacKeyConf -> Map.Map ByteString ByteString
keys (Just (HmacKeyConf m _)) = m
keys _                        = Map.empty

takeBody :: RequestJson -> ByteString
{-# INLINE takeBody #-}
takeBody (JsonBody _ raw)     = raw
takeBody _                    = BS.empty

allowLocal :: HmacKeyConf -> Bool
{-# INLINE allowLocal #-}
allowLocal (HmacKeyConf _ a) = a

