{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NStack.Comms.Types where

import Data.ByteString (ByteString)  -- from: bytestring
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Serialize (Serialize(..), Putter, Get)
import Data.Serialize.Put (putEitherOf)
import Data.Serialize.Get (getEitherOf)
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import DBus (makeRepresentable)
import GHC.Generics (Generic)
import Text.PrettyPrint.Mainland (Pretty)

import NStack.Module.Types (DebugOpt, ModuleName, MethodURI, Stack)
import NStack.Module.Types (Image(..))
import NStack.Prelude.Text (putText, getText)

-- general type aliases used from client/server comms

newtype GitRepo = GitRepo { _gitRepo :: Bool }
  deriving (Eq)
newtype ProcessId = ProcessId { _processId :: Text }
  deriving (Eq, Pretty, Ord)
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
  _processes  :: [ProcessId],
  _methods    :: Map.Map MethodURI TypeSignature,
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
  show = coerce (show :: Text -> String)

instance Show ContainerId where
  show = coerce (show :: Text -> String)

instance Show GitRepo where
  show = coerce (show :: Bool -> String)

instance Show WorkflowSrc where
  show = coerce (show :: Text -> String)

instance Show TypeSignature where
  show = coerce (show :: Text -> String)

instance Show DSLSource where
  show = coerce (show :: Text -> String)

instance Serialize ProcessId where
  put = coerce putText
  get = coerce getText

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
  put = coerce (put :: Putter ByteString)
  get = coerce (get :: Get ByteString)

instance Serialize ServerInfo
instance Serialize ModuleInfo

makeRepresentable ''ProcessId
makeRepresentable ''ContainerId

deriveSafeCopy 0 'base ''ProcessId
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

startCommand :: ApiCall (DSLSource, DebugOpt) (DSLSource, ProcessId)
startCommand = ApiCall "StartCommand"

stopCommand :: ApiCall ProcessId ProcessId
stopCommand = ApiCall "StopCommand"

logsCommand :: ApiCall ProcessId [LogsLine]
logsCommand = ApiCall "LogsCommand"

serverLogsCommand :: ApiCall () [LogsLine]
serverLogsCommand = ApiCall "ServerLogsCommand"

infoCommand :: ApiCall Bool ServerInfo
infoCommand = ApiCall "InfoCommand"

listCommand :: ApiCall (Maybe MethodType, Bool) [(MethodURI, TypeSignature)]
listCommand = ApiCall "ListCommand"

listModulesCommand :: ApiCall Bool [ModuleName]
listModulesCommand = ApiCall "ListModulesCommand"

deleteModuleCommand :: ApiCall ModuleName (Maybe UUID)
deleteModuleCommand = ApiCall "DeleteModuleCommand"

listProcessesCommand :: ApiCall () [ProcessId]
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
