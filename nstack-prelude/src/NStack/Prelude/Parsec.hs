-- | Utils functions around Parsec
module NStack.Prelude.Parsec where

import Control.Monad.Except (MonadError)
import Data.Functor.Identity (Identity)
import Data.Bifunctor (first)
import Data.Text (Text)
import Text.Parsec.Text                                        -- from: parsec
import qualified Text.Parsec as P                                        -- from: parsec
import Text.Parsec (Parsec, SourceName, ParseError, spaces, eof, Stream) -- from: parsec

import NStack.Prelude.Monad (eitherToExcept)

-- | Run a parser on a string
parse :: (Stream s Identity Char) => Parsec s () a -> SourceName -> s -> Either ParseError a
parse p = P.parse (p <* spaces <* eof)

-- | Run a pure parser over a text element
inlineParser :: MonadError String m => Parser a -> Text -> m a
inlineParser p  = eitherToExcept . first show . parse p ""

