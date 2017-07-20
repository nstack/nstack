module NStack.Settings (SettingsT,
                        runSettingsT,
                        MonadSettings(..),
                        Settings(..),
                        InstallID(..),
                        installId,
                        analytics,
                        installUUID,
                        AnalyticsSettings(..),
                        analyticsSettings,
                        authSettings,
                        AuthSettings(..),
                        authLogin,
                        authKey,
                        authServer,
                        HostName(..),
                        defaultAuthServer,
                        frontendHost,
                        serviceLimits,
                        cliTimeout,
                        defaultFrontendHost,
                        runSettingsParser,
                        serverConn,
                        serverHostname,
                        serverPort,
                        ServerDetails(..)
                       ) where
import Control.Exception
import Control.Lens                        -- from: lens
import Control.Monad (when, unless, guard)
import Control.Monad.Trans                 -- from: mtl
import qualified Data.ByteString as BS     -- from: bytestring
import Data.List (isPrefixOf)
import Data.UUID (UUID)                    -- from: uuid
import Data.UUID.V1 (nextUUID)             -- from: uuid
import System.Directory (getXdgDirectory,
                         XdgDirectory(..),
                         createDirectoryIfMissing) -- from: directory
import System.IO.Error (isDoesNotExistError)

import NStack.Prelude.Exception (throwPermanentError)
import NStack.Prelude.FilePath (directory)
import NStack.Settings.Types hiding (runSettingsT)
import NStack.Settings.Parser
import qualified NStack.Settings.Types as Types
import qualified Data.Yaml as Yaml


settingsFileName :: FilePath
settingsFileName = "nstack.conf"

settingsFilePath :: IO FilePath
settingsFilePath = configDir <&> \fp ->
                      -- If we're root then prefer /etc/<filename>
                      -- as the server only ever run the server on linux
                      -- the check below works.
                      -- unix-compat provides a get user function that is
                      -- portable except on windows it just makes up an
                      -- arbitrary user number so i'm loath to use it without
                      -- wrapping in a CPP block so it isn't used on windows,
                      -- but in that case the below method works just as well
                      if "/root/" `isPrefixOf` fp
                        then etcloc
                        else fp
                                 
  where configDir = getXdgDirectory XdgConfig settingsFileName `catch` fallback
        etcloc = "/etc/nstack/" ++ settingsFileName
        -- getHomeDirectory on unix is simply an env var lookup
        -- for $HOME. If this doesn't exist (for instance when run
        -- in a non-standard environment -- eg init/cron) then
        -- we will get an error. In that case we fallback to /etc/<filename>
        -- eventually we will probably want a cli argument to override too.
        fallback :: IOException -> IO FilePath
        fallback _ = return etcloc

data BadConfigException = BadConfigException FilePath String
  deriving Eq

instance Show BadConfigException where
  show (BadConfigException path parseError) = "Config file " ++ path ++ " is not valid. Parse error: " ++ parseError

instance Exception BadConfigException

loadSettings :: IO Settings
loadSettings =
  catchJust (guard . isDoesNotExistError)
    (do path <- settingsFilePath
        dat <- BS.readFile path
        either (throwIO . BadConfigException path) return (runSettingsParser dat)
    )
    (const defWithWrite)
               where defWithWrite = do 
                       s <- defaultSettingsWithInstallId
                       settingsPath <- settingsFilePath

                       unless ("/etc/nstack" `isPrefixOf` settingsPath) (writeSettings s)

                       return s

defaultSettingsWithInstallId :: IO Settings
defaultSettingsWithInstallId = setInstallId <$> getUUID
  where setInstallId x = defaultSettings & installId ?~ InstallID x

getUUID :: IO UUID
getUUID = nextUUID >>= maybe getUUID pure

writeSettings :: Settings -> IO ()
writeSettings s = do
  path <- settingsFilePath

  when ("/etc/nstack" `isPrefixOf` path) (throwPermanentError "Setting were tried to be written to /etc/nstack/")
  -- create parent directories of the settings file if they don't exist
  createDirectoryIfMissing True (directory path)

  BS.writeFile path (Yaml.encode s)


runSettingsT :: MonadIO m => SettingsT m m a -> m a
runSettingsT m = Types.runSettingsT m $ buildSettingOp loadSettings write' liftIO
  where write' orig new = unless (orig == new) $ writeSettings new

