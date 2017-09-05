module NStack.Module.Parser where

import Data.Foldable (asum)
import Control.Applicative (optional, some)
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError, catchError)

import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAlpha)
import Data.Semigroup
import Data.Text (Text)                        -- from: text
import Data.Text (pack, unpack)                -- from: text
import Data.Void (Void)
import Text.Megaparsec (Parsec, MonadParsec, parse, parseErrorPretty, many, parseMaybe, (<|>), skipMany, eof, sepBy1, try)    -- from: megaparsec
import Text.Megaparsec.Char (spaceChar, alphaNumChar, char, anyChar, string, upperChar)    -- from: megaparsec
import Text.Regex.Applicative (sym, psym, RE, match)
import Text.Regex.Applicative.Common (decimal)
import qualified Text.Megaparsec.Lexer as L    -- from: megaparsec

import NStack.Auth (UserName(..), nstackUserName)
import NStack.Module.Types
import NStack.Prelude.Monad (maybeToExcept, eitherToExcept, orError)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "/") (L.skipBlockCommentNested "/*" "*/")

symbol :: String -> Parser String
symbol    = L.symbol spaceConsumer

pLanguage :: Parser Language
pLanguage = ((string "python" <|> string "Python") *> ((Python2 <$ (string "27" <|> string "2"))
                                                    <|> pure Python))
         <|> (NodeJS <$ (string "nodejs" <|> string "NodeJS"))

inlineParser :: MonadError String m => Parser a -> Text -> m a
inlineParser p = eitherToExcept . first parseErrorPretty . parse (spaces *> p <* spaces <* eof) ""
  where
    spaces = skipMany spaceChar

parseModuleName :: MonadError String m => Text -> m ModuleName
parseModuleName n = (do
  (ModuleIdentifier ident v1 v2 v3 r) <- match modNameRegex (unpack n) `orError` "Not a valid modulename"
  parseModuleName' Nothing ident v1 v2 v3 r) `catchError` fmtError
  where fmtError err = throwError $
          "Could not parse module name " <> unpack n <> ".\n"
          <> err

data ModuleIdentifier = ModuleIdentifier Text Integer Integer Integer Release

-- A Canonical Module Name Lexer we can re-use
modNameRegex :: RE Char ModuleIdentifier
modNameRegex = ModuleIdentifier <$> ident' <*> dec' ':' <*> dec' '.' <*> dec' '.' <*> snap
  where
    ident' = fmap pack $
      ((:) <$> psym (\c -> isAlpha c || c == '_')
           <*> many (psym $ \c -> isAlphaNum c || c == '_' || c == '.' || c == '/')
      ) <|> "[Byte]" -- for backwards compatibility, see #701
    dec' a = sym a *> decimal
    snap = maybe Release (const Snapshot) <$> optional "-SNAPSHOT"

pRawModuleName :: Parser String
pRawModuleName = many . asum $ alphaNumChar : map char ".:/-"

-- | Parse and extract the raw modulename string from a DSL
dslModuleName :: Parser Text
dslModuleName = pack <$> (spaceConsumer *> (symbol "module") *> pRawModuleName <* (skipMany anyChar) <* eof)

-- | Parse and return the module name from a DSL
getDslName :: MonadError String m => Text -> m ModuleName
getDslName src = (maybeToExcept "Can't find DSL ModuleName" . parseMaybe dslModuleName $ src) >>= parseModuleName

-- | Parse a module name as a single identifier
parseModuleName' :: MonadError String m => Maybe UserName -> Text -> Integer -> Integer -> Integer -> Release -> m ModuleName
parseModuleName' user ident v1 v2 v3 r = inlineParser (pModuleName user) ident <*> pure (Version v1 v2 v3 r)

pModuleName :: forall m . MonadParsec Void Text m => Maybe UserName -> m (Version -> ModuleName)
pModuleName user = asum $ fmap try [
    ModuleName <$> registry   <*> author    <*> name,
    ModuleName nStackRegistry <$> author    <*> name,
    ModuleName nStackRegistry defaultAuthor <$> name
  ]
  where
    registry = fmap NSUri $ ((:) <$> (pathElem <* char '.') <*> (pathElem `sepBy1` char '.')) <* char '/' -- registry must be at least 2 elems
    author = UserName <$> pathElem <* char '/'
    pathElem = pack <$> some (alphaNumChar <|> char '_')
    name = NSUri <$> nameElem `sepBy1` char '.'
    nameElem = fmap pack $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')
    defaultAuthor = maybe nstackUserName id user

