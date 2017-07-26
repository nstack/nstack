module NStack.Settings.Parser(runSettingsParser) where

import Control.Monad (join, when)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Coerce
import qualified Data.UUID as UUID
import Data.Yaml.Combinators

import NStack.Auth (readUserId, readKey)
import NStack.Module.Types (DebugOpt(..))
import NStack.Settings.Types (AnalyticsSettings(..), HostName(..), ServerDetails(..), AuthSettings(..), Settings(..), InstallID(..))

valueWhenMatching :: a -> Parser () -> Parser a
valueWhenMatching v p = const v <$> p

valueWhenMatchingString :: a -> T.Text -> Parser a
valueWhenMatchingString a = valueWhenMatching a . theString

analyticsSettingsParser :: Parser AnalyticsSettings
analyticsSettingsParser =
  AnalyticsEnabled `valueWhenMatchingString` "enabled" <>
  AnalyticsDisabled `valueWhenMatchingString` "disabled"
                                                               
hostNameParser :: Parser HostName
hostNameParser = (HostName . coerce) <$> string

frontendHostParser :: Parser HostName
frontendHostParser = validate string $ \s -> do
  when (T.last s == '/') (Left "frontend-host url not to contain a trailing slash")
  return $ (HostName . coerce) s

serverDetailsParser :: Parser ServerDetails
serverDetailsParser = object $
  ServerDetails <$>
    optFieldOrNull "hostname" hostNameParser <*>
    optFieldOrNull "port" integer

authSettingsParser :: Parser AuthSettings
authSettingsParser = nstackAuthParser <> trustAuthParser
  where
    nstackAuthParser = object $
      theField "scheme" "nstack" *> (NStackHMAC <$>
                                      field "user-id" userIdParser <*>
                                      field "secret-key" secretKeyParser)

    trustAuthParser = object $
      theField "scheme" "trust" *> (Trust <$>
                                     field "secret-key" secretKeyParser)
                 
    userIdParser = validate string $
      maybe (Left "the user-id to be a lowercase hexstring with even length") Right . readUserId

    secretKeyParser = validate string $
      maybe (Left "the secret-key to be a lowercase hexstring with even length") Right . readKey

debugParser :: Parser DebugOpt
debugParser = trueToDebug <$> bool
  where
    trueToDebug True = Debug
    trueToDebug False = NoDebug

settingsParser :: Parser Settings
settingsParser = object $ Settings <$>
  optFieldOrNull "install-id" (InstallID <$> uuidParser) <*>
  optFieldOrNull "analytics" analyticsSettingsParser <*>
  optFieldOrNull "authentication" authSettingsParser <*>
  optFieldOrNull "auth-server" hostNameParser <*>
  optFieldOrNull "server" serverDetailsParser <*>
  optFieldOrNull "frontend-host" frontendHostParser <*>
  optFieldOrNull "service-limits" bool <*>
  optFieldOrNull "cli-timeout" integer <*>
  optFieldOrNull "debug" debugParser
  where
    uuidParser = validate string $
      maybe (Left "install-id to be a valid UUID") Right . UUID.fromText

optFieldOrNull :: T.Text -> Parser a -> FieldParser (Maybe a)
optFieldOrNull fieldName parser = fmap join parserOfMaybe
  where
    parserOfMaybe = optField fieldName (liftedParser <> liftedNull)
    liftedParser = Just <$> parser
    liftedNull = Nothing `valueWhenMatching` null_

runSettingsParser :: BS.ByteString -> Either String Settings
runSettingsParser = parse settingsParser
