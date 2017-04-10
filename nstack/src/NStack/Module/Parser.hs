{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module NStack.Module.Parser where

import Data.Foldable (asum, for_)
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError, catchError)

import Data.Bifunctor (first)
import Data.Char (isUpper, isAlphaNum)
import Data.Semigroup
import Data.Maybe (isNothing)
import Data.Text (Text)                        -- from: text
import qualified Data.Text as T                -- from: text
import qualified Data.Text.Read as T           -- from: text
import qualified Text.Megaparsec.Text as P    -- from: megaparsec
import Text.Megaparsec (spaceChar, alphaNumChar, char, parse, parseErrorPretty, many, parseMaybe, (<|>), anyChar, skipMany, eof, string)    -- from: megaparsec
import qualified Text.Megaparsec.Lexer as L    -- from: megaparsec

import NStack.Auth (UserName(..))
import NStack.Module.Types
import NStack.Prelude.Monad (maybeToExcept, eitherToExcept)

spaceConsumer :: P.Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "/") (L.skipBlockCommentNested "/*" "*/")

symbol :: String -> P.Parser String
symbol    = L.symbol spaceConsumer

pStack :: P.Parser Stack
pStack = (Python <$ (string "python" <|> string "Python"))
         <|> (NodeJS <$ (string "nodejs" <|> string "NodeJS"))

inlineParser :: MonadError String m => P.Parser a -> Text -> m a
inlineParser p = eitherToExcept . first parseErrorPretty . parse (spaces *> p <* spaces <* eof) ""
  where
    spaces = skipMany spaceChar

parseModuleName :: MonadError String m => Text -> m ModuleName
parseModuleName n = (do
  (ident, v1, v2, v3, r) <- splitModName n
  parseModuleName' Nothing ident v1 v2 v3 r) `catchError` fmtError
  where fmtError err = throwError $
          "Could not parse module name " <> T.unpack n <> ".\n"
          <> err

-- | Split a module into its elements
splitModName :: MonadError String m => Text -> m (Text, Integer, Integer, Integer, Release)
splitModName modname = do
  (name, ver_snapshot) <-
    case T.splitOn ":" modname of
      [name, ver_snapshot] -> return (name, ver_snapshot)
      _ -> noVersion
  for_ (T.find (not . validChar) name) $ \c ->
    invalidChar c
  (ver, snapshot) <-
    case T.splitOn "-" ver_snapshot of
      [ver] -> return (ver, Release)
      [ver, snapshot]
        | snapshot == "SNAPSHOT" -> return (ver, Snapshot)
        | otherwise -> badSnapshot snapshot
      _ -> manyDashes
  let ver_components_text = T.splitOn "." ver
  ver_components <- mapM parseVerNumber ver_components_text
  (v1, v2, v3) <-
    case ver_components of
      [v1, v2, v3] -> return (v1, v2, v3)
      _ -> wrongNumberOfComponents
  return (name, v1, v2, v3, snapshot)
  where
    parseVerNumber txt =
      case T.decimal txt of
        Left {} -> badVersionComponent txt
        Right (n, rest) ->
          if T.null rest
            then return n
            else badVersionComponent txt
    validChar c = isAlphaNum c || c `elem` ("./" :: [Char])

    noVersion = throwError "Module must have a version, e.g. Foo:1.2.3 or Foo:1.2.3-SNAPSHOT"
    wrongNumberOfComponents = throwError "Version must have exactly three components (e.g. 1.2.3)"
    badSnapshot snapshot = throwError $
      "Unexpected part of module version: " ++ T.unpack snapshot ++
      "\nDid you mean 'SNAPSHOT'?"
    manyDashes = throwError "A dash ('-') is only allowed as part of '-SNAPSHOT'"
    badVersionComponent v = throwError $ "Version component " ++ show v ++ " is not an integer"
    invalidChar c = throwError $ "Invalid character in module name: " ++ show c ++
      "\nValid characters are letters, numbers, '.', and '/'"

pRawModuleName :: P.Parser String
pRawModuleName = many . asum $ alphaNumChar : map char ".:/-"

-- | Parse and extract the raw modulename string from a DSL
dslModuleName :: P.Parser Text
dslModuleName = T.pack <$> (spaceConsumer *> (symbol "module") *> pRawModuleName <* (skipMany anyChar) <* eof)

-- | Parse and return the module name from a DSL
getDslName :: MonadError String m => Text -> m ModuleName
getDslName src = (maybeToExcept "Can't find DSL ModuleName" . parseMaybe dslModuleName $ src) >>= parseModuleName

-- | Parse a module name as a single identifier
-- TODO - this needs improvement
parseModuleName' :: (Monad m) => Maybe UserName -> Text -> Integer -> Integer -> Integer -> Release -> m ModuleName
parseModuleName' mUser ident v1 v2 v3 r = do
  modNameF <- case T.splitOn "/" ident of
    [] -> error "moduleName': empty identifier (shouldn't happen)"
    [modName] -> ModuleName nStackRegistry defaultAuthor <$> mkModName modName
    [regOrAuthor, modName] -> if isAuthor regOrAuthor
      then ModuleName nStackRegistry (Author regOrAuthor) <$> mkModName modName
      else ModuleName <$> mkReg regOrAuthor <*> pure defaultAuthor <*> mkModName modName
    [reg, author, modName] -> ModuleName <$> mkReg reg <*> pure (Author author) <*> mkModName modName
    _ -> fail'
  return . modNameF $ Version v1 v2 v3 r
    where
      -- check all elements begin with upper char
      mkModName modName = if validModName modName then return $ NSUri (T.splitOn "." modName) else fail'
      validModName modName = all (\x -> Just True == (isUpper . fst <$> T.uncons x)) $ T.splitOn "." modName
      isAuthor = isNothing . T.find (== '.')
      mkReg x = let reg = T.splitOn "." x in
        if length reg > 1 then return (NSUri reg) else fail "Invalid registry length"
      fail' = fail "Invalid module name, must be capitalised and separated with '.'"
      defaultAuthor = maybe nStackAuthor (Author . _username) mUser


