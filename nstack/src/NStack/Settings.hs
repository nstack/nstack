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
                        serverConn,
                        serverHostname,
                        serverPort,
                        ServerDetails(..)
                       ) where
import Control.Exception
import Control.Lens                        -- from: lens
import Control.Monad (unless, guard)
import Control.Monad.Trans                 -- from: mtl
import qualified Data.ByteString as BS     -- from: bytestring
import Data.List (isPrefixOf)
import Data.UUID (UUID)                    -- from: uuid
import Data.UUID.V1 (nextUUID)             -- from: uuid
import System.Directory (getXdgDirectory,
                         XdgDirectory(..)) -- from: directory
import System.IO.Error (isDoesNotExistError)

import NStack.Settings.Internal.Lens
import NStack.Settings.Types hiding (runSettingsT)
import qualified NStack.Settings.Types as Types

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

data BadConfigException = BadConfigException FilePath
  deriving Eq

instance Show BadConfigException where
  show (BadConfigException path) = "Config file " ++ path ++ " is not valid"

instance Exception BadConfigException

loadSettings :: IO Settings
loadSettings =
  catchJust (guard . isDoesNotExistError)
    (do path <- settingsFilePath
        dat <- BS.readFile path
        maybe (throwIO $ BadConfigException path) return $ dat ^? _YAML
    )
    (const defWithWrite)
               where defWithWrite = do s <- defaultSettingsWithInstallId
                                       writeSettings s
                                       return s

defaultSettingsWithInstallId :: IO Settings
defaultSettingsWithInstallId = setInstallId <$> getUUID
  where setInstallId x = defaultSettings & installId ?~ InstallID x

getUUID :: IO UUID
getUUID = nextUUID >>= maybe getUUID pure

writeSettings :: Settings -> IO ()
writeSettings s = settingsFilePath >>= \f -> BS.writeFile f (s ^. re _YAML)

runSettingsT :: MonadIO m => SettingsT m m a -> m a
runSettingsT m = Types.runSettingsT m $ buildSettingOp loadSettings write' liftIO
  where write' orig new = unless (orig == new) $ writeSettings new
