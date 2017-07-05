module NStack.Settings.Types where
import Control.Applicative (empty, (<|>))
import Control.DeepSeq
import Control.Lens hiding ((.=))               -- from: lens
import Control.Monad (mfilter)
import Control.Monad.Reader (ReaderT(..))       -- from: mtl
import Data.Aeson                               -- from: aeson
import Data.Aeson.Types (Parser)                -- from: aeson
import Data.Coerce
import Data.String
import Data.Text (Text, toLower)                -- from: text
import Data.UUID (UUID)                         -- from: uuid
import GHC.Generics

import NStack.Auth (SecretKey, UserId)

newtype InstallID = InstallID UUID deriving (Eq, Show, ToJSON, FromJSON)

installUUID :: Iso' InstallID UUID
installUUID = iso coerce InstallID

data AnalyticsSettings = AnalyticsEnabled | AnalyticsDisabled
  deriving (Eq, Show)

analyticsSettings :: Prism' Text AnalyticsSettings
analyticsSettings = prism' tos (froms . toLower)
  where tos AnalyticsEnabled  = "enabled"
        tos AnalyticsDisabled = "disabled"
        froms "enabled"       = Just AnalyticsEnabled
        froms "disabled"      = Just AnalyticsDisabled
        froms _               = Nothing

instance FromJSON AnalyticsSettings where
  parseJSON (String x) = maybe empty pure $ x ^? analyticsSettings
  parseJSON _          = empty

instance ToJSON AnalyticsSettings where
  toJSON a = String $ a ^. re analyticsSettings

-- TODO - Use the existing URI type
newtype HostName = HostName Text deriving (Eq, Show, IsString)

instance ToJSON HostName where
  toJSON (HostName a) = coerce (toJSON a)

instance FromJSON HostName where
  parseJSON a = coerce (parseJSON a :: Parser Text)

-- TODO: version settings files?
data Settings = Settings { _installId :: Maybe InstallID,
                           _analytics :: Maybe AnalyticsSettings,
                           _authSettings :: Maybe AuthSettings,
                           _authServer :: Maybe HostName,
                           _server :: Maybe ServerDetails,
                           _frontendHost :: Maybe HostName }
  deriving (Eq, Show)

data AuthSettings = NStackHMAC UserId SecretKey
                  | Trust SecretKey
                  deriving (Eq, Show, Generic)

data ServerDetails = ServerDetails { _hostname :: Maybe HostName,
                                     _port :: Maybe Int } deriving (Eq, Show)

instance NFData AuthSettings

instance FromJSON ServerDetails where
  parseJSON (Object o) = ServerDetails <$> (o .:? "hostname") <*> (o .:? "port")
  parseJSON _ = empty

instance ToJSON ServerDetails where
  toJSON (ServerDetails h p) = object ["hostname" .= h,
                                       "port" .= p]

instance FromJSON AuthSettings where
  parseJSON (Object o) =  (mfilter (=="nstack") (o .: "scheme" :: Parser Text) *> (NStackHMAC <$> o .: "user-id" <*> o .: "secret-key")) <|>
                          (mfilter (=="trust") (o .: "scheme" :: Parser Text) *> (Trust <$> o .: "secret-key"))
  parseJSON _ = empty

instance ToJSON AuthSettings where
  toJSON (NStackHMAC user key) = object ["scheme" .= ("nstack" :: Text),
                                          "user-id" .= user,
                                          "secret-key" .= key]
  toJSON (Trust key) = object ["scheme" .= ("trust" :: Text),
                               "secret-key" .= key]

defaultSettings :: Settings
defaultSettings = Settings { _installId = Nothing, _analytics = Nothing, _authSettings = Nothing, _authServer = Nothing, _server = Nothing, _frontendHost = Nothing }

installId :: Lens' Settings (Maybe InstallID)
installId f s = (\r -> s { _installId = r }) <$> f (_installId s)

analytics :: Lens' Settings (Maybe AnalyticsSettings)
analytics f s = (\r -> s { _analytics = r }) <$> f (_analytics s)

authSettings :: Lens' Settings (Maybe AuthSettings)
authSettings f s = (\r -> s { _authSettings = r }) <$> f (_authSettings s)

authServer :: Lens' Settings (Maybe HostName)
authServer f s = (\r -> s { _authServer = r }) <$> f (_authServer s)

frontendHost :: Lens' Settings (Maybe HostName)
frontendHost f s = (\r -> s { _frontendHost = r }) <$> f (_frontendHost s)

authKey :: Lens' AuthSettings SecretKey
authKey f = \case
  NStackHMAC u k -> NStackHMAC u <$> f k
  Trust k -> Trust <$> f k

authLogin :: Prism' AuthSettings (UserId, SecretKey)
authLogin = prism' (uncurry NStackHMAC) $ \case
  NStackHMAC u k -> Just (u, k)
  _ -> Nothing

serverConn :: Lens' Settings (Maybe ServerDetails)
serverConn f s = (\r -> s { _server = r }) <$> f (_server s)

serverHostname :: Lens' ServerDetails (Maybe HostName)
serverHostname f s = (\r -> s { _hostname = r }) <$> f (_hostname s)

serverPort :: Lens' ServerDetails (Maybe Int)
serverPort f s = (\r -> s { _port = r }) <$> f (_port s)

defaultAuthServer :: HostName
defaultAuthServer = HostName "http://localhost:8443"

defaultFrontendHost :: HostName
defaultFrontendHost = HostName "http://localhost:8000"

defaultServerDetails :: ServerDetails
defaultServerDetails = ServerDetails Nothing Nothing

instance FromJSON Settings where
  parseJSON (Object v) = Settings <$> v .:? "install-id" <*> v .:? "analytics" <*> v .:? "authentication" <*> v .:? "auth-server" <*> v .:? "server" <*> v .:? "frontend-host"
  parseJSON _          = empty

instance ToJSON Settings where
  toJSON a = object ["analytics"  .= (a ^. analytics),
                     "install-id" .= (a ^. installId),
                     "authentication" .= (a ^. authSettings),
                     "server" .= (a ^. serverConn),
                     "auth-server" .= (a ^. authServer),
                     "frontend-host" .= (a ^. frontendHost)]

class MonadSettings m where
  settings :: m Settings
  modifySettings :: (Settings -> Settings) -> m ()


newtype SettingState m = SettingState { runSS :: forall r. (Settings -> (r, Settings)) -> m r }
type SettingsT n = ReaderT (SettingState n)

settingsT :: (SettingState n -> m a) -> SettingsT n m a
settingsT = ReaderT

runSettingsT :: SettingsT n m a -> (forall r. (Settings -> (r, Settings)) -> n r) -> m a
runSettingsT m f = runReaderT m (SettingState f)

instance MonadSettings (SettingsT m m) where
  settings = settingsT $ \opf -> runSS opf dup
    where dup a = (a, a)
  modifySettings f = settingsT $ \opf -> runSS opf (((), ) . f)

buildSettingOp :: Monad n => m Settings -> (Settings -> Settings -> m ())
               -> (forall a. m a -> n a) -> (forall r. (Settings -> (r, Settings)) -> n r)
buildSettingOp load dump lift' f = lift' load >>= \s -> let (r, s') = f s in lift' (dump s s') >> return r
