module NStack.Http (
  Method,
  getMethod,
  Path,
  httpOK,
  http400,
  http404,
  http500,
  httpData,
  httpUnauthorized,
  httpMethodNotAllowed,
  contentPlaintext
) where

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy            -- from: bytestring
import qualified Network.Wai as Web                      -- from: wai
import qualified Network.HTTP.Types as Http              -- from: http-types

-- | HTTP Request Method ADT
type Method = Http.StdMethod
-- | HTTP Request Path
type Path = ByteString

getMethod :: MonadError String m => Web.Request -> m Method
getMethod = eitherToExcept . Http.parseMethod . Web.requestMethod
  where eitherToExcept = either (const $ throwError "Bad Request: Invalid HTTP Method") return

contentPlaintext :: Http.ResponseHeaders
contentPlaintext = [("Content-Type", "text/plain")]

contentOctetStream :: Http.ResponseHeaders
contentOctetStream = [("Content-Type", "application/octet-stream")]

http400 :: Lazy.ByteString -> Web.Response
http400 = Web.responseLBS Http.status400 contentPlaintext

httpOK :: Web.Response
httpOK = Web.responseLBS Http.status200 contentPlaintext "Msg Accepted"

http404 :: Web.Response
http404 = Web.responseLBS Http.status404 contentPlaintext "Not Found"

http500 :: Lazy.ByteString -> Web.Response
http500 = Web.responseLBS Http.status500 contentPlaintext

httpData :: ByteString -> Web.Response
httpData = Web.responseLBS Http.status200 contentOctetStream . Lazy.fromStrict

httpUnauthorized :: Web.Response
httpUnauthorized = Web.responseLBS Http.status401 contentPlaintext "Unauthorized"

httpMethodNotAllowed :: Web.Response
httpMethodNotAllowed = Web.responseLBS Http.status405 contentPlaintext "405 Method Not Allowed"
