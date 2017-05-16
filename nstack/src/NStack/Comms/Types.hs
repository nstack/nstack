{-# LANGUAGE TemplateHaskell #-}

module NStack.Comms.Types where

import Data.ByteString (ByteString)  -- from: bytestring
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base, contain, SafeCopy(..), safePut, safeGet)
import Data.Serialize (Serialize(..), putTwoOf, getTwoOf, getEitherOf, putEitherOf)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme (UTCTime, formatTime, parseTime)
import Data.Thyme.Time (fromThyme, toThyme)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import DBus (makeRepresentable)
import GHC.Generics (Generic)
import System.Locale (defaultTimeLocale)
import Text.PrettyPrint.Mainland (Pretty(..), text, spaces, align, stack)

import NStack.Module.Types (DebugOpt, ModuleName, QFnName, Stack)
import NStack.Module.Types (Image(..))
import NStack.Prelude.Text (putText, getText)

processTimeFormat :: String
processTimeFormat = "%F %T"

-- general newtypes used from client/server comms

newtype StartTime = StartTime UTCTime
  deriving (Eq, Ord)

instance Show StartTime where
  show (StartTime t) = show t

instance Pretty StartTime where
  ppr = text . show

instance Serialize StartTime where
  put (StartTime t) = put $ formatTime defaultTimeLocale processTimeFormat t
  get = do
    s <- get
    maybe (fail "Could not parse date string") return $ timeM s
      where timeM = fmap StartTime . parseTime defaultTimeLocale processTimeFormat

instance SafeCopy StartTime where
  putCopy (StartTime t) = contain . safePut $ fromThyme t
  getCopy = contain $ StartTime . toThyme <$> safeGet

newtype ProcessId = ProcessId Text
  deriving (Eq, Pretty, Ord)

newtype GitRepo = GitRepo { _gitRepo :: Bool }
  deriving (Eq)

data ProcessInfo = ProcessInfo
  { _processId :: ProcessId
  , _timestamp :: StartTime
  , _command :: Text
  } deriving (Eq, Ord, Show)

instance Pretty ProcessInfo where
  ppr (ProcessInfo (ProcessId p) t c) =  ppr p <> spaces 5 <> ppr t <> spaces 2 <> align (stack $ ppr <$> T.lines c)

newtype ContainerId = ContainerId { _containerId :: Text } -- systemd dbus object path
newtype BuildTarball = BuildTarball { _buildTarball :: ByteString }
newtype WorkflowSrc = WorkflowSrc { _workflowSrc :: Text }
newtype LogsLine = LogsLine { _logLine :: Text }
  deriving (Pretty)
newtype TypeSignature = TypeSignature { _typeSignature :: Text }
  deriving (Pretty)
newtype DSLSource = DSLSource { _src :: Text }
  deriving (Eq, Ord, Typeable, IsString)

-- | Simplified nstack-server info for sending to the CLI
data ServerInfo = ServerInfo {
  _processes  :: [ProcessInfo],
  _methods    :: Map.Map QFnName TypeSignature,
  _modules    :: Map.Map ModuleName ModuleInfo }
  deriving (Generic)

-- | Simplified module info for sending to the CLI
data ModuleInfo = ModuleInfo {
  _miStack :: Maybe Stack,
  _miImage :: Image,
  _miParent :: Maybe ModuleName,
  _miIsFramework :: Bool
} deriving (Generic)


data MethodType = MethodType | SourceType | SinkType | WorkflowType
  deriving (Eq, Show, Generic)

instance Serialize MethodType

instance Show ProcessId where
  show = coerce $ show @Text

instance Show ContainerId where
  show = coerce $ show @Text

instance Show GitRepo where
  show = coerce $ show @Bool

instance Show WorkflowSrc where
  show = coerce $ show @Text

instance Show TypeSignature where
  show = coerce $ show @Text

instance Show DSLSource where
  show = coerce $ show @Text

instance Serialize ProcessId where
  put = coerce putText
  get = coerce getText

instance Serialize ProcessInfo where
  put (ProcessInfo pId t c) = putTwoOf put (putTwoOf put putText) (pId, (t, c))
  get = (\(p, (t, c)) -> ProcessInfo p t c) <$> getTwoOf get (getTwoOf get getText)

instance Serialize WorkflowSrc where
  put = coerce putText
  get = coerce getText

instance Serialize LogsLine where
  put = coerce putText
  get = coerce getText

instance Serialize TypeSignature where
  put = coerce putText
  get = coerce getText

instance Serialize DSLSource where
  put = coerce putText
  get = coerce getText

instance Serialize BuildTarball where
  put = coerce $ put @ByteString
  get = coerce $ get @ByteString

instance Serialize ServerInfo
instance Serialize ModuleInfo

makeRepresentable ''ProcessId
makeRepresentable ''ContainerId

deriveSafeCopy 0 'base ''ProcessId
deriveSafeCopy 0 'base ''ProcessInfo
deriveSafeCopy 0 'base ''ContainerId

-- NStack Toolkit/Server Communication
newtype ServerReturn a = ServerReturn { _serverReturn :: Either Text a } -- return type used internally for commands
  deriving (Functor)

instance (Serialize a) => Serialize (ServerReturn a) where
  put = putEitherOf putText put . _serverReturn
  get = ServerReturn <$> getEitherOf getText get

--------------------------------------------------------------------------------
-- Server API

data ApiCall a b where
  ApiCall :: (Serialize a, Serialize b) => Text -> ApiCall a b

startCommand :: ApiCall (DSLSource, DebugOpt) ProcessInfo
startCommand = ApiCall "StartCommand"

stopCommand :: ApiCall ProcessId ProcessId
stopCommand = ApiCall "StopCommand"

logsCommand :: ApiCall ProcessId [LogsLine]
logsCommand = ApiCall "LogsCommand"

serverLogsCommand :: ApiCall () [LogsLine]
serverLogsCommand = ApiCall "ServerLogsCommand"

infoCommand :: ApiCall Bool ServerInfo
infoCommand = ApiCall "InfoCommand"

listCommand :: ApiCall (Maybe MethodType, Bool) [(QFnName, TypeSignature)]
listCommand = ApiCall "ListCommand"

listModulesCommand :: ApiCall Bool [ModuleName]
listModulesCommand = ApiCall "ListModulesCommand"

deleteModuleCommand :: ApiCall ModuleName (Maybe UUID)
deleteModuleCommand = ApiCall "DeleteModuleCommand"

listProcessesCommand :: ApiCall () [ProcessInfo]
listProcessesCommand = ApiCall "ListProcessesCommand"

gcCommand :: ApiCall () [UUID]
gcCommand = ApiCall "GarbageCollectCommand"

buildCommand :: ApiCall BuildTarball ModuleName
buildCommand = ApiCall "BuildCommand"

buildWorkflowCommand :: ApiCall (WorkflowSrc, ModuleName) ModuleName
buildWorkflowCommand = ApiCall "BuildWorkflowCommand"

-- Not used for now but potentially all toolkit commands will send identifying info (e.g. token)
-- Client data sent upon each DBus request
-- type ClientState = (FilePath)
-- type ClientState' = (String)
