{-# OPTIONS_GHC -fno-warn-orphans #-}

module NStack.CLI.Auth (
  signRequest,
  addHeader,
  allowSelfSigned
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hAuthorization)

import NStack.Auth
import NStack.Prelude.Applicative ((<&>))
import NStack.Settings (AuthSettings(..))

signRequest :: AuthSettings -> Request -> IO Request
signRequest (NStackHMAC user key) req = nstackHmacSha256 user key req <&> (\auth -> req `addHeader` ( hAuthorization, auth ))
signRequest (Trust key)           req = nstackTrust key req <&> (\auth -> req `addHeader` ( hAuthorization, auth ))

addHeader :: Request -> Header -> Request
addHeader req header = req { requestHeaders = header : requestHeaders req }

nstackHmacSha256 :: UserId -> SecretKey -> Request -> IO ByteString
nstackHmacSha256 user k req = format <$> sign k req
  where format s = encodeUtf8 $ "NSTACK-HMAC-SHA256 user=" <> formatUserId user <> ",signature=" <> s

nstackTrust :: SecretKey -> Request -> IO ByteString
nstackTrust k req = format <$> sign k req
  where format s = encodeUtf8 $ "NSTACK-TRUST signature=" <> s

instance GetPayload Request IO where
  getPath = return . path
  getBody = get . requestBody
    where get (RequestBodyLBS b)  = return $ toStrict b
          get (RequestBodyBS  b)  = return b
          get (RequestBodyIO  b)  = b >>= get
          get _                   = return "NOT YET IMPLEMENTED" -- We don't support signing streamed bodies right now

-- TODO - we should not accept self-signed certificates; this is a temporary fix until we
-- can embed an NStack root cert in the CLI such that we can sign the server certificates
-- ourselves
allowSelfSigned :: TLSSettings
allowSelfSigned = TLSSettingsSimple
  { settingDisableCertificateValidation = True
  , settingDisableSession = True
  , settingUseServerName = False
  }
