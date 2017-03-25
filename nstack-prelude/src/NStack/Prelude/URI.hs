module NStack.Prelude.URI (
  readPort,
  queryParams,
  queryValue,
  mkQueryString
) where

import Control.Monad (join)
import Control.Monad.Except (MonadError)
import Data.ByteString.Char8 (pack)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)
import Data.Word (Word16)
import Network.HTTP.Types (parseQueryText, urlDecode)
import Text.Parsec (digit, string, many)
import Text.Read (readEither)

import NStack.Prelude.Parsec (parse)
import NStack.Prelude.Monad (castError)

type Port = Word16

readPort :: MonadError String m => String -> m Port
readPort s = do
  p <- parse (string ":" *> many digit) "" s `castError` (err . show)
  readEither p `castError` (err . show)
    where err reason = "Cannot parse Port from string: "<>s<>"("<>reason<>")"

queryParams :: String -> Map Text (Maybe Text)
queryParams = Map.fromList . parseQueryText . urlDecode False . pack

queryValue :: Text -> Map Text (Maybe Text) -> Maybe Text
queryValue t m = join $ Map.lookup t m

mkQueryString :: Map Text Text -> Text
mkQueryString m = "?" <> (intercalate "&" $ queryPair <$> (Map.toList m))
  where queryPair (a,b) = a <> "=" <> b
