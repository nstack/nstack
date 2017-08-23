import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.List (isInfixOf)
import Data.Aeson

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (defaultMainWithIngredients)
import Test.Tasty.Runners.AntXML

import NStack.Module.Types
import NStack.Settings
import NStack.Auth (readKey, readUserId)


main :: IO ()
main = defaultMainWithIngredients (antXMLRunner:defaultIngredients) $ testGroup "Tests" [
  settingsParserTests
  ]

settingsParserTests :: TestTree
settingsParserTests = testGroup "Unit Tests"
  [ testCase "Display a proper error message when invalid analytics settings" $
      checkErrorMessages "test/res/invalid-analytics-settings.conf" "Expected \"disabled\", \"enabled\" instead of",
    testCase "Display a proper error message when invalid install-id" $
      checkErrorMessages "test/res/invalid-install-id.conf" "Expected install-id to be a valid UUID instead of",
    testCase "Display a proper error message when invalid server" $
      checkErrorMessages "test/res/invalid-server.conf" "Expected Object, Null instead of",

    testCase "Display a proper error message when trust scheme is given more arguments than needed" $
      checkErrorMessages "test/res/invalid-authentication1.conf" "Unexpected \n\nuser-id: abc123",

    testCase "Display a proper error message when more arguments for nstack scheme are needed" $
      checkErrorMessages "test/res/invalid-authentication2.conf" "Expected field \"user-id\" as part of:",

    testCase "Display a proper error message when invalid authentication scheme is used" $
      checkErrorMessages "test/res/invalid-authentication3.conf" "Expected \"nstack\", \"trust\" instead of:",
    testCase "Display a proper error message when invalid secret-key is used" $
      checkErrorMessages "test/res/invalid-authentication4.conf" "Expected the secret-key to be a lowercase hexstring with even length instead of:",

    testCase "Display a proper error message if frontend-host contains a trailing slash" $
      checkErrorMessages "test/res/invalid-frontend-host.conf" "Expected frontend-host url not to contain a trailing slash",

    testCase "Parse a settings file"
      parseSettingsFileTest,

    testCase "Serializing and then deserializing is the identity" $
      serializeDeserialize completeSettingsSample
  ]

checkErrorMessages :: FilePath -> String -> IO ()
checkErrorMessages filepath errorMessage = do
  dat <- BS.readFile filepath
  let parseResult = runSettingsParser dat
  case parseResult of
    Left s -> unless (errorMessage `isInfixOf` s) (errorFailure s)
    _ -> assertFailure "Parsing should have failed"

  where
    errorFailure actualError =
      assertFailure $ "Expected to find '" ++ errorMessage ++ "' inside actual error: " ++
        actualError

parseSettingsFileTest :: IO ()
parseSettingsFileTest = do
  dat <- BS.readFile "test/res/valid-settings-file.conf"
  let parseResult = runSettingsParser dat
  case parseResult of
    Left errorMessage ->
      assertFailure $ "Parsing should not have failed: " ++ errorMessage
    Right actualSettings ->
      assertEqual "Settings should be equal" expectedSettings actualSettings
  where
    expectedSettings =
      Settings
        (Nothing :: Maybe InstallID)
        (Just AnalyticsEnabled)
        (NStackHMAC <$> readUserId "1a2b3c" <*> readKey "abc123")
        (Just (HostName "https://demo-register.nstack.com:8443"))
        (Just (ServerDetails (Just $ HostName "examplehost.com") (Just 3000)))
        (Just (HostName "http://localhost:8000"))
        Nothing
        Nothing
        Nothing

completeSettingsSample :: Settings
completeSettingsSample = Settings
  (Nothing :: Maybe InstallID)
  (Just AnalyticsEnabled)
  (NStackHMAC <$> readUserId "1a2b3c" <*> readKey "abc123")
  (Just (HostName "https://demo-register.nstack.com:8443"))
  (Just (ServerDetails (Just $ HostName "adfadfa") (Just 3000)))
  (Just $ HostName "http://localhost:8000")
  (Just True)
  (Just 23)
  (Just Debug)

serializeDeserialize :: Settings -> IO ()
serializeDeserialize s = do
  let bs = toStrict (encode s)
  either
    (const $ assertFailure "Parsing should not have failed")
    (assertEqual "Original and serialize-deserialized should be equal" s)
    (runSettingsParser bs)
