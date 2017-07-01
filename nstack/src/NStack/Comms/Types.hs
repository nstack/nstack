{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NStack.Comms.Types where

import Data.ByteString (ByteString)  -- from: bytestring
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base, contain, SafeCopy(..), safePut, safeGet)
import Data.Serialize (Serialize(..), putTwoOf, getTwoOf)
import Data.Serialize.Text () -- Serialize Text instances
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme (UTCTime)
import Data.Thyme.Time (fromThyme, toThyme)
import Data.Typeable (Typeable)
import qualified Data.Loc as Loc
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Text.PrettyPrint.Mainland (Pretty(..), text, spaces, align, stack)
import Language.Haskell.TH (Name)
import Control.Exception(Exception(..))

import NStack.Module.Types (DebugOpt, ModuleName, QFnName, Stack, Image(..), Qualified)
import NStack.Prelude.Text (putText, getText)
import NStack.Prelude.Time (timeToUnix, timeFromUnix)

-- general newtypes used from client/server comms

newtype StartTime = StartTime UTCTime
  deriving (Eq, Ord)

instance Show StartTime where
  show (StartTime t) = show t

instance Pretty StartTime where
  ppr = text . show

instance Serialize StartTime where
  put (StartTime t) = put $ timeToUnix t
  get = do
    s <- get
    maybe (fail "Could not parse date string") return $ timeM s
      where timeM = fmap StartTime . timeFromUnix

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
  deriving (Pretty, Eq)
data TypeSignature
  = TypeSignature Text
    -- ^ the 'QFnName' is a function with this type signature
  | TypeDefinition Text
    -- ^ the 'QFnName' is a type with this definition
  deriving (Show, Generic)
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


data EntityType = MethodType | SourceType | SinkType | WorkflowType | TypeType
  deriving (Eq, Show, Generic)

instance Serialize EntityType

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

instance Serialize ProcessInfo where
  put (ProcessInfo pId t c) = putTwoOf put (putTwoOf put putText) (pId, (t, c))
  get = (\(p, (t, c)) -> ProcessInfo p t c) <$> getTwoOf get (getTwoOf get getText)

instance Serialize WorkflowSrc where
  put = coerce putText
  get = coerce getText

instance Serialize LogsLine where
  put = coerce putText
  get = coerce getText

instance Serialize TypeSignature

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

-- | The 'Text' in the output is a function or type name
listCommand :: ApiCall (Maybe EntityType, Bool) [(Qualified Text, TypeSignature)]
listCommand = ApiCall "ListCommand"

listModulesCommand :: ApiCall Bool [ModuleName]
listModulesCommand = ApiCall "ListModulesCommand"

deleteModuleCommand :: ApiCall ModuleName ()
deleteModuleCommand = ApiCall "DeleteModuleCommand"

listProcessesCommand :: ApiCall () [ProcessInfo]
listProcessesCommand = ApiCall "ListProcessesCommand"

gcCommand :: ApiCall () [UUID]
gcCommand = ApiCall "GarbageCollectCommand"

buildCommand :: ApiCall BuildTarball ModuleName
buildCommand = ApiCall "BuildCommand"

testCommand :: ApiCall (QFnName, HttpPath) ProcessInfo
testCommand = ApiCall "TestCommand"

-- | List all supported API calls.
--
-- This list needs to be kept up to date for correct versioning. See "NStack.Comms.ApiHash".
allApiCalls :: [Name]
allApiCalls =
  [ 'buildCommand
  , 'deleteModuleCommand
  , 'gcCommand
  , 'infoCommand
  , 'listCommand
  , 'listModulesCommand
  , 'listProcessesCommand
  , 'logsCommand
  , 'serverLogsCommand
  , 'startCommand
  , 'stopCommand
  ]

-- Not used for now but potentially all toolkit commands will send identifying info (e.g. token)
-- Client data sent upon each DBus request
-- type ClientState = (FilePath)
-- type ClientState' = (String)
