module NStack.Module.Parser where

import Data.Foldable (asum)
import Control.Applicative (some)
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
import qualified Text.Megaparsec.Char.Lexer as L    -- from: megaparsec

import NStack.Auth (UserName(..), nstackUserName)
import NStack.Module.Name (ModuleName, ModuleRef, ModuleURI(..), nStackRegistry, NSUri(..))
import NStack.Module.Version (ExactRelease(..), FuzzyRelease(..), SemVer(..), parseSnapshotHash)
import NStack.Module.Types (Language(..))
import NStack.Prelude.Monad (maybeToExcept, eitherToExcept, orError)

type (||) = Either

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "/") (L.skipBlockCommentNested "/*" "*/")

symbol :: Text -> Parser Text
symbol    = L.symbol spaceConsumer

pLanguage :: Parser Language
pLanguage = ((string "python" <|> string "Python") *> ((Python2 <$ (string "27" <|> string "2"))
                                                    <|> pure Python))
            <|> (R <$ (string "R" <|> string "r"))
            <|> (NodeJS <$ (string "nodejs" <|> string "NodeJS"))

inlineParser :: MonadError String m => Parser a -> Text -> m a
inlineParser p = eitherToExcept . first parseErrorPretty . parse (spaces *> p <* spaces <* eof) ""
  where
    spaces = skipMany spaceChar

parseModuleName :: MonadError String m => Text -> m ModuleName
parseModuleName = parseModuleUri fuzzyRelease

parseModuleRef :: MonadError String m => Text -> m ModuleRef
parseModuleRef = parseModuleUri exactRelease

parseModuleUri :: MonadError String m => (RawRelease -> m r) -> Text -> m (ModuleURI (SemVer r))
parseModuleUri parseR n = (do
  (ModuleIdentifier ident v1 v2 v3 rawR) <- match modNameRegex (unpack n) `orError` "Not a valid modulename"
  r <- parseR rawR
  parseModuleName' Nothing ident v1 v2 v3 r) `catchError` fmtError
  where fmtError err = throwError $ "Could not parse module name " <> unpack n <> ": " <> err <> "\n"

data ModuleIdentifier = ModuleIdentifier Text Integer Integer Integer RawRelease

-- A Canonical Module Name Lexer we can re-use
modNameRegex :: RE Char ModuleIdentifier
modNameRegex = ModuleIdentifier <$> ident' <*> dec' ':' <*> dec' '.' <*> dec' '.' <*> releaseRegex
  where
    ident' = fmap pack $
      ((:) <$> psym (\c -> isAlpha c || c == '_')
           <*> many (psym $ \c -> isAlphaNum c || c == '_' || c == '.' || c == '/')
      ) <|> "[Byte]" -- for backwards compatibility, see #701
    dec' a = sym a *> decimal

fuzzyRelease :: MonadError String m => RawRelease -> m FuzzyRelease
fuzzyRelease RawRelease    = return FRelease
fuzzyRelease LatestSnap    = return Snapshot
fuzzyRelease (ExactSnap t) = FSnap <$> parseSnapshotHash t

exactRelease :: MonadError String m => RawRelease -> m ExactRelease
exactRelease RawRelease    = return Release
exactRelease LatestSnap    = throwError "Expected an exact version, found a floating snapshot"
exactRelease (ExactSnap t) = Snap <$> parseSnapshotHash t

releaseRegex :: RE Char RawRelease
releaseRegex = asum [
    "-SNAPSHOT-" *> (ExactSnap . pack <$> some(psym (\c -> isAlphaNum c || c == '-'))),
    LatestSnap <$ "-SNAPSHOT",
    RawRelease <$ ""
  ]

data RawRelease = RawRelease
                | LatestSnap
                | ExactSnap Text
                deriving (Eq, Ord, Show)

pRawModuleName :: Parser String
pRawModuleName = many . asum $ alphaNumChar : map char ".:/-"

-- | Parse and extract the raw modulename string from a DSL
dslModuleName :: Parser Text
dslModuleName = pack <$> (spaceConsumer *> (symbol "module") *> pRawModuleName <* (skipMany anyChar) <* eof)

-- | Parse and return the module name from a DSL
getDslName :: MonadError String m => Text -> m ModuleName
getDslName src = (maybeToExcept "Can't find DSL ModuleName" . parseMaybe dslModuleName $ src) >>= parseModuleName

-- | Parse a module name as a single identifier
parseModuleName' :: MonadError String m => Maybe UserName -> Text -> Integer -> Integer -> Integer -> r -> m (ModuleURI (SemVer r))
parseModuleName' user ident v1 v2 v3 r = inlineParser (pModuleName user) ident <*> pure (SemVer v1 v2 v3 r)

pModuleName :: forall m . MonadParsec Void Text m => Maybe UserName -> forall v. m (v -> ModuleURI v)
pModuleName user = asum $ fmap try [
    ModuleURI <$> registry   <*> author    <*> name,
    ModuleURI nStackRegistry <$> author    <*> name,
    ModuleURI nStackRegistry defaultAuthor <$> name
  ]
  where
    registry = fmap NSUri $ ((:) <$> (pathElem <* char '.') <*> (pathElem `sepBy1` char '.')) <* char '/' -- registry must be at least 2 elems
    author = UserName <$> pathElem <* char '/'
    pathElem = pack <$> some (alphaNumChar <|> char '_')
    name = NSUri <$> nameElem `sepBy1` char '.'
    nameElem = fmap pack $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')
    defaultAuthor = maybe nstackUserName id user

