{-# OPTIONS_GHC -fno-warn-orphans #-}

module NStack.Module.ConfigFile where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except(MonadError)              -- mtl
import Control.Monad.Trans(MonadIO, liftIO)          -- mtl
import Data.Aeson.Types (typeMismatch)
import Data.Text(Text, unpack, pack, toLower)
import qualified Data.Yaml as Y
import Data.Yaml((.:), (.:?), (.!=), (.=))
import Data.String (IsString)
import Turtle ((</>))
import qualified Turtle as R

import NStack.Module.Types (APIVersion, ModuleName(..), Language(..), FedoraVersion, FedoraSnapshot)
import NStack.Module.Parser (parseModuleName, pLanguage, inlineParser)
import NStack.Prelude.FilePath (fromFP, toFP)
import NStack.Prelude.Exception (throwPermanentError)
import NStack.Prelude.Text (showT, pprT)
type API = Text
type Package = Text
type Command = Text
type File = Text

instance Y.FromJSON Language where
  parseJSON obj@(Y.String t) = either (`typeMismatch` obj) return (inlineParser pLanguage t)
  parseJSON _ = mzero

instance Y.ToJSON Language where
  toJSON = Y.String . pack . show

data ConfigStack = ConfigStack {
  _cfgLang :: Language,
  _cfgApiVersion :: APIVersion,
  _cfgSnapVersion :: (FedoraVersion, FedoraSnapshot)
} deriving (Show)

instance Y.FromJSON ConfigStack where
  parseJSON (Y.Object v) =
    ConfigStack <$>
    v .: "language" <*>
    v .: "api-version" <*>
    v .: "snapshot"
  parseJSON _ = mzero

instance Y.ToJSON ConfigStack where
  toJSON (ConfigStack l v s) = Y.object ["language" .= toLower (showT l), "api-version" .= v, "snapshot" .= s]

instance Y.FromJSON ModuleName where
  parseJSON (Y.String t) = either fail return $ parseModuleName t
  parseJSON _ = mzero

instance Y.ToJSON ModuleName where
  toJSON = Y.String . pprT

mkStackParent :: Either ConfigStack ModuleName -> Y.Value
mkStackParent (Left cfgStack) = Y.object ["stack" .= cfgStack]
mkStackParent (Right modName) = Y.object ["parent" .= modName]

-- | ConfigFile module configuration file used to describe the module project dir
data ConfigFile = ConfigFile {
  _cfgName :: Maybe ModuleName,
  -- ^ the name of the module - I think we can remove this as is also specified within the module.nml
  _cfgStackParent :: Either ConfigStack ModuleName,
  -- ^ We can define the parent either in terms of the stack, which then resolves to a known-parent, or reference a parent (i.e. Framework) directly - in either case the stack is taken from the parent
  _cfgPackages :: [Package],
  -- ^ system packages needed by the module
  _cfgCommands :: [Command],
  -- ^ script commands to run during the build process
  _cfgFiles :: [File]
  -- ^ files to copy into the container from the directory root
} deriving (Show)

instance Y.FromJSON ConfigFile where
  parseJSON (Y.Object v) =
    ConfigFile <$>
    v .:? "name" <*>
    (Right <$> v .: "parent" <|> Left <$> v .: "stack") <*>
    v .:? "packages" .!= mempty <*>
    v .:? "commands" .!= mempty <*>
    v .:? "files" .!= mempty
  parseJSON _ = mzero

configFile :: IsString s => s
configFile = "nstack.yaml"

workflowFile :: IsString s => s
workflowFile = "module.nml"

-- | Return the config for the module
getConfigFile :: (MonadIO m) => R.FilePath -> m ConfigFile
getConfigFile moduleDir =
  either (throwPermanentError . prettyPrintParseException) return =<<
    liftIO (Y.decodeFileEither $ fromFP (moduleDir </> configFile))

-- | Project File
projectFile :: IsString s => s
projectFile = "nstack-project.yaml"

data ProjectFile = ProjectFile {
  _projectModules :: [R.FilePath]
} deriving (Show)

instance Y.FromJSON ProjectFile where
  parseJSON (Y.Object v) =
    ProjectFile <$>
    v .: "modules"
  parseJSON _ = mzero

instance Y.FromJSON R.FilePath where
  parseJSON (Y.String t) = return.toFP.unpack $ t
  parseJSON _ = mzero

-- | Return the project file in the current dir
getProjectFile :: (MonadIO m, MonadError String m) => m ProjectFile
getProjectFile =
  either (throwPermanentError . prettyPrintParseException) return =<<
    liftIO (Y.decodeFileEither $ fromFP projectFile)

prettyPrintParseException :: Y.ParseException -> String
prettyPrintParseException e =
  case e of
    -- Y.prettyPrintParseException adds 'Aeson exception: ' to the
    -- error message, which is completely unhelpful
    Y.AesonException s -> s
    _ -> Y.prettyPrintParseException e
