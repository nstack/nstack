{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NStack.Comms.Types (module NStack.Comms.Types) where

import Control.Lens (makeLenses, Lens', Iso')
import Data.ByteString (ByteString)  -- from: bytestring
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base, SafeCopy(..))
import Data.Serialize (Serialize(..))
import Data.Serialize.Put (Put)
import Data.Serialize.Get (Get)
import Data.Serialize.Text () -- Serialize Text instances
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme (UTCTime)
import Data.Typeable (Typeable)
import qualified Data.Loc as Loc
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Text.PrettyPrint.Mainland (Pretty(..), text, spaces, align, stack)
import Language.Haskell.TH (Name)
import Control.Exception(Exception(..))

import NStack.Comms.TypeRepresentation
import NStack.Module.Name (ModuleRef)
import NStack.Module.QMap (QMap)
import NStack.Module.Types (DebugOpt, FnName, TyName, QFnName, Stack, Image(..))
import NStack.Prelude.Text (putText, getText)
import NStack.Prelude.Time (timeToUnix, timeFromUnix)

-- general newtypes used from client/server comms

newtype BookendTime = BookendTime { _bookendTime :: UTCTime }
  deriving (Eq, Ord)

newtype StartTime = StartTime { _startBookend :: BookendTime }
  deriving (Eq, Ord, Serialize, Pretty)

newtype StopTime = StopTime { _stopBookend :: BookendTime }
  deriving (Eq, Ord, Serialize, Pretty)

newtype ScheduledTime = ScheduledTime { _unscheduledBookend :: UTCTime }
  deriving (Eq, Ord)

instance Show BookendTime where
  show (BookendTime t) = show t

instance Show StopTime where
  show (StopTime t) = show t

instance Show StartTime where
  show (StartTime t) = show t

instance Show ScheduledTime where
  show (ScheduledTime t) = show t

instance Pretty BookendTime where
  ppr = text . show

instance Pretty ScheduledTime where
  ppr = text . show

putTime :: UTCTime -> Put
putTime = put . timeToUnix

getTime :: Get UTCTime
getTime = do
  s <- get
  maybe (fail "Could not parse date string") return $ timeFromUnix s

instance Serialize BookendTime where
  put (BookendTime t) = putTime t
  get = BookendTime <$> getTime

instance Serialize ScheduledTime where
  put (ScheduledTime t) = putTime t
  get = ScheduledTime <$> getTime

instance SafeCopy StartTime

instance SafeCopy BookendTime

instance SafeCopy StopTime

newtype ProcessId = ProcessId Text
  deriving (Eq, Pretty, Ord, Generic)

newtype StoppedFrom = StoppedFrom Int
  deriving (Serialize, Num, Eq, Show, Ord)

newtype StoppedAmount = StoppedAmount Int
  deriving (Serialize, Num)

newtype FormId = FormId UUID
  deriving (Eq, Show, Ord, Serialize, Pretty)

processIdText :: Lens' ProcessId Text
processIdText f (ProcessId t) = ProcessId <$> f t

newtype GitRepo = GitRepo { _gitRepo :: Bool }
  deriving (Eq)

data ProcessInfo a = ProcessInfo
  { _processId :: ProcessId
  , _timestamp :: StartTime
  , _dslCommand :: Text
  , _stopTime :: a
  } deriving (Eq, Ord, Show, Functor, Generic, Foldable, Traversable)

instance Pretty (ProcessInfo ()) where
  ppr (ProcessInfo (ProcessId p) t c _) =  ppr p <> spaces 5 <> ppr t <> spaces 2 <> align (stack $ ppr <$> T.lines c)

instance Pretty (ProcessInfo StopTime) where
  ppr (ProcessInfo (ProcessId p) t c stop) =  ppr p <> spaces 5 <> ppr t <> spaces 2 <> ppr stop <> spaces 2 <> align (stack $ ppr <$> T.lines c)

newtype ContainerId = ContainerId { _containerId :: Text } -- systemd dbus object path
  deriving (Serialize)
newtype BuildTarball = BuildTarball { _buildTarball :: ByteString }
newtype WorkflowSrc = WorkflowSrc { _workflowSrc :: Text }
newtype LogsLine = LogsLine { _logLine :: Text }
  deriving (Pretty, Eq, Generic)

data TypeSignature
  = TypeSignature Text
    -- ^ the 'QFnName' is a function with this type signature
  | TypeDefinition Text
    -- ^ the 'QFnName' is a type with this definition
  deriving (Show, Generic, Eq)

-- Maybe acknowledges that not all nstack methods have a proper
-- 'MTypeRepresentation'. Only monomorphic methods do.
data MethodInfo = MethodInfo TypeSignature (Maybe MTypeRepresentation)
  deriving (Show, Eq, Generic)

newtype DSLSource = DSLSource { _src :: Text }
  deriving (Eq, Ord, Typeable, IsString)

-- | Simplified nstack-server info for sending to the CLI
data ServerInfo = ServerInfo {
  _siProcesses  :: [ProcessInfo ()],
  _siStopped    :: [ProcessInfo StopTime],
  _siMethods    :: QMap FnName MethodInfo,
  _siModules    :: Map.Map ModuleRef ModuleInfo }
  deriving (Eq, Show, Generic)

data ForFrontend = ForFrontend
  { _wuProcesses  :: [ProcessInfo ()]
  , _wuStopped    :: [ProcessInfo StopTime]
  , _wuMethods    :: QMap FnName MethodInfo
  , _wuModules    :: Map.Map ModuleRef ModuleInfo
  , _wuOldModules :: QMap FnName MethodInfo
  , _wuAvailLogs  :: Maybe [LogsLine]
  , _wuServerLogs :: [LogsLine]
  } deriving (Eq, Show, Generic)

-- | Simplified module info for sending to the CLI
data ModuleInfo = ModuleInfo {
  _miStack :: Maybe Stack,
  _miImage :: Image,
  _miParent :: Maybe ModuleRef,
  _miIsFramework :: Bool
} deriving (Generic, Eq, Show)


data TypeType = TypeType
  deriving (Eq, Show, Generic)

data MethodType = MethodType | SourceType | SinkType | WorkflowType
  deriving (Eq, Show, Generic)

instance Serialize MethodType
instance Serialize TypeType

data ContainerData = ContainerData {
  _containerTypeSigs :: Text,
  _containerCode :: Text,
  _containerYaml :: Text,
  _containerReqs :: Text
  } deriving (Generic, Eq, Show)

instance Serialize ContainerData

instance Show ProcessId where
  show = coerce $ show @Text

instance Show ContainerId where
  show = coerce $ show @Text

instance Show GitRepo where
  show = coerce $ show @Bool

instance Show WorkflowSrc where
  show = coerce $ show @Text

instance Show DSLSource where
  show = coerce $ show @Text

instance Show LogsLine where
  show = coerce $ show @Text

instance Serialize ProcessId where
  put = coerce putText
  get = coerce getText

instance Serialize a => Serialize (ProcessInfo a)

instance Serialize WorkflowSrc where
  put = coerce putText
  get = coerce getText

instance Serialize LogsLine where
  put = coerce putText
  get = coerce getText

instance Serialize TypeSignature
instance Serialize MethodInfo

instance Serialize DSLSource where
  put = coerce putText
  get = coerce getText

newtype HttpPath = HttpPath Text

instance Serialize HttpPath where
  put = coerce putText
  get = coerce getText

instance Serialize BuildTarball where
  put = coerce $ put @ByteString
  get = coerce $ get @ByteString

instance Serialize ServerInfo
instance Serialize ForFrontend
instance Serialize ModuleInfo

deriveSafeCopy 0 'base ''ProcessId
deriveSafeCopy 0 'base ''ProcessInfo
deriveSafeCopy 0 'base ''ContainerId

-- NStack Toolkit/Server Communication
newtype ServerReturn a = ServerReturn { _serverReturn :: Either StructuredError a } -- return type used internally for commands
  deriving (Functor, Serialize)

type SrcSpan = Loc.Loc

data SrcError = SrcError SrcSpan String
  deriving (Show,Typeable,Generic)

instance Exception SrcError where
  displayException = ppSrcError
  -- SrcError is a sub-exception of StructuredError
  fromException e0 = do
    SeSrcError e <- fromException e0
    return e
  toException = toException . SeSrcError

instance Serialize SrcError

ppSrcError :: SrcError -> String
ppSrcError (SrcError s msg) =
  Loc.displayLoc s ++ ":\n" ++ msg

data StructuredError
  = SeSrcError SrcError
  -- feel free to add new constructors
  -- and make them sub-exceptions of StructuredError
  | SeOther String
  deriving (Show,Typeable,Generic)

instance Exception StructuredError where
  displayException = \case
    SeSrcError e -> displayException e
    SeOther e -> e

instance Serialize StructuredError

-- orphan instances
deriving instance Generic Loc.Loc
deriving instance Generic Loc.Pos
instance Serialize Loc.Loc
instance Serialize Loc.Pos

--------------------------------------------------------------------------------
-- Server API

data ApiCall a b where
  ApiCall :: (Serialize a, Serialize b) => Text -> ApiCall a b

startCommand :: ApiCall (DSLSource, DebugOpt) (ProcessInfo ())
startCommand = ApiCall "StartCommand"

stopCommand :: ApiCall ProcessId (ProcessInfo StopTime)
stopCommand = ApiCall "StopCommand"

logsCommand :: ApiCall ProcessId [LogsLine]
logsCommand = ApiCall "LogsCommand"

serverLogsCommand :: ApiCall () [LogsLine]
serverLogsCommand = ApiCall "ServerLogsCommand"

infoCommand :: ApiCall Bool ServerInfo
infoCommand = ApiCall "InfoCommand"

frontendInfoCommand :: ApiCall (Maybe ProcessId, Maybe StoppedFrom) ForFrontend
frontendInfoCommand =  ApiCall "FrontendInfoCommand"

formRefreshCommand :: ApiCall () (Map.Map FormId (MTypeRepresentation, ProcessInfo (), QFnName))
formRefreshCommand = ApiCall "FormRefreshCommand"

startFormCommand :: ApiCall QFnName (FormId, ProcessInfo ())
startFormCommand = ApiCall "StartFormCommand"

formInfoCommand :: ApiCall FormId (MTypeRepresentation, ProcessInfo (), QFnName)
formInfoCommand = ApiCall "FormInfoCommand"

-- | The 'Text' in the output is a function or type name
listAllCommand :: ApiCall Bool (QMap FnName MethodInfo, QMap TyName TypeSignature)
listAllCommand = ApiCall "ListCommand"

listFnCommand :: ApiCall (Bool, Maybe MethodType) (QMap FnName MethodInfo)
listFnCommand = ApiCall "ListFnCommand"

listTypesCommand :: ApiCall Bool (QMap TyName TypeSignature)
listTypesCommand = ApiCall "ListTypesCommand"

listScheduledCommand :: ApiCall () [(ProcessInfo (), [ScheduledTime])]
listScheduledCommand = ApiCall "ListScheduledCommand"

listModulesCommand :: ApiCall Bool [ModuleRef]
listModulesCommand = ApiCall "ListModulesCommand"

deleteModuleCommand :: ApiCall ModuleRef ()
deleteModuleCommand = ApiCall "DeleteModuleCommand"

listProcessesCommand :: ApiCall () [ProcessInfo ()]
listProcessesCommand = ApiCall "ListProcessesCommand"

listStoppedCommand :: ApiCall (Maybe StoppedFrom, Maybe StoppedAmount) [ProcessInfo StopTime]
listStoppedCommand = ApiCall "ListStoppedCommand"

gcCommand :: ApiCall () [UUID]
gcCommand = ApiCall "GarbageCollectCommand"

buildCommand :: ApiCall BuildTarball ModuleRef
buildCommand = ApiCall "BuildCommand"

buildFrontendCommand :: ApiCall ContainerData ModuleRef
buildFrontendCommand = ApiCall "BuildFrontendCommand"

testCommand :: ApiCall (QFnName, HttpPath) (ProcessInfo ())
testCommand = ApiCall "TestCommand"

$(makeLenses ''ServerInfo)
$(makeLenses ''ForFrontend)
$(makeLenses ''ProcessInfo)
$(makeLenses ''BookendTime)
$(makeLenses ''StartTime)
$(makeLenses ''StopTime)

stopToTime :: Iso' StopTime UTCTime
stopToTime = stopBookend.bookendTime

startTime :: Iso' StartTime UTCTime
startTime = startBookend.bookendTime

-- | List all supported API calls.
--
-- This list needs to be kept up to date for correct versioning. See "NStack.Comms.ApiHash".
allApiCalls :: [Name]
allApiCalls =
  [ 'buildCommand
  -- should this even be in the version hash as it's not called on the CLI
  , 'buildFrontendCommand
  , 'deleteModuleCommand
  , 'gcCommand
  , 'infoCommand
  , 'frontendInfoCommand
  , 'listAllCommand
  , 'listTypesCommand
  , 'listModulesCommand
  , 'listProcessesCommand
  , 'listStoppedCommand
  , 'listScheduledCommand
  , 'logsCommand
  , 'serverLogsCommand
  , 'startCommand
  , 'stopCommand
  ]

-- Not used for now but potentially all toolkit commands will send identifying info (e.g. token)
-- Client data sent upon each DBus request
-- type ClientState = (FilePath)
-- type ClientState' = (String)
