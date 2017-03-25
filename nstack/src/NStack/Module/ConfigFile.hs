{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NStack.Module.ConfigFile where

import Control.Monad
import Control.Monad.Except(MonadError, throwError)  -- mtl
import Control.Monad.Trans(MonadIO, liftIO)          -- mtl
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor (first)
import Data.Text(Text, unpack, pack)
import qualified Data.Yaml as Y
import Data.Yaml((.:), (.:?), (.!=))
import qualified Filesystem.Path as Path
import Turtle (testfile, (</>))
import qualified Turtle as R

import NStack.Module.Types (ModuleName(..), Stack(..))
import NStack.Module.Parser (parseModuleName, pStack, inlineParser)
import NStack.Prelude.FilePath (fromFP, toFP)
import NStack.Prelude.Monad (eitherToExcept)
type API = Text
type Package = Text
type Command = Text
type File = Text
-- TODO - rename as Project?

-- | ConfigFile module configuration file
-- used to describe the module project dir
data ConfigFile = ConfigFile {
  _cfgName :: ModuleName,
  _cfgStack :: Maybe Stack,
  _cfgParent :: ModuleName,
  _cfgApi :: Maybe API,
  _cfgPackages :: [Package],
  _cfgCommands :: [Command],
  _cfgFiles :: [File]
} deriving (Show)

instance Y.FromJSON ConfigFile where
  parseJSON (Y.Object v) =
    ConfigFile <$>
    v .: "name" <*>
    v .:? "stack" <*>
    v .: "parent" <*>
    v .:? "api" <*>
    v .:? "packages" .!= mempty <*>
    v .:? "commands" .!= mempty <*>
    v .:? "files" .!= mempty
  parseJSON _ = mzero

-- | We detect user-created framework modules if they have no stack nor API
-- within their ConfigFile
isFrameworkConfig :: ConfigFile -> Bool
isFrameworkConfig (ConfigFile _ Nothing _ Nothing _ _ _) = True
isFrameworkConfig _ = False

instance Y.FromJSON ModuleName where
  parseJSON (Y.String t) = either fail return $ parseModuleName t
  parseJSON _ = mzero

instance Y.FromJSON Stack where
  parseJSON obj@(Y.String t) = either (`typeMismatch` obj) return (inlineParser pStack t)
  parseJSON _ = mzero

instance Y.ToJSON Stack where
  toJSON = Y.String . pack . show

configFile :: R.FilePath
configFile = "nstack.yaml"

workflowFile :: R.FilePath
workflowFile = "workflow.nml"

-- | Return the config for the module
getConfigFile :: (MonadIO m, MonadError String m) => R.FilePath -> m ConfigFile
getConfigFile moduleDir = liftIO (Y.decodeFileEither $ fromFP (moduleDir </> configFile))
                         >>= (eitherToExcept . first prettyPrintParseException)
  where
    prettyPrintParseException e =
      case e of
        -- Y.prettyPrintParseException adds 'Aeson exception: ' to the
        -- error message, which is completely unhelpful
        Y.AesonException s -> s
        _ -> Y.prettyPrintParseException e

-- | Return the current project root by going up directories until a
-- config file is found.
--
-- Throws an exception if a nstack.yaml file cannot be parsed.
discover :: (MonadIO m, MonadError String m) => R.FilePath -> m R.FilePath
discover root = do
    exists <- testfile (root </> configFile)
    if exists then validOrThrow root >> return root
              else next root
 where
    next dir | Path.parent dir == dir = throwError "No config file found"
             | otherwise = discover (Path.parent dir)
    validOrThrow dir = void $ getConfigFile dir


-- | Project File
projectFile :: R.FilePath
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
getProjectFile = liftIO (Y.decodeFileEither $ fromFP projectFile)
                         >>= (eitherToExcept . first Y.prettyPrintParseException)
