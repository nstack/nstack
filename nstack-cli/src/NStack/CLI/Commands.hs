{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module NStack.CLI.Commands (
  CCmd,
  initCommand,
  buildArtefacts,
  InitStack(..),
  Command(..),
  loginSettings,
  showStartMessage,
  showStopMessage,
  printInfo,
  printMethods,
  showModuleBuild,
  showWorkflowBuild
) where

import qualified Control.Foldl as L
import Control.Lens ((&), (?~))
import Control.Monad.Except     -- mtl
import Control.Monad.Trans ()    -- mtl
import Control.Monad.Extra (whenM) -- mtl
import Data.Bifunctor (first)      -- bifunctors
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Tree (Forest, unfoldForest)
import Data.Tree.View (showTree)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Filesystem.Path.CurrentOS as FP -- system-filepath
import Util ((<||>))                 -- ghc
import System.Directory (getCurrentDirectory)
import qualified Text.Mustache as M  -- mustache
import Text.Mustache ((~>))          -- mustache
import qualified Text.PrettyPrint.Mainland as M
import Text.PrettyPrint.Mainland ((</>))
import qualified Turtle as R         -- turtle

import NStack.Auth
import NStack.CLI.Types
import NStack.CLI.Templates (createFromTemplate)
import NStack.Comms.Types (GitRepo(..), ProcessId(..), ModuleInfo(..), ServerInfo(..), MethodType, TypeSignature(..), DSLSource(..))
import NStack.Module.Types (Stack, BaseImage(..), DebugOpt(..), ModuleName(..), MethodURI(..))
import NStack.Module.Parser (parseModuleName)
import qualified NStack.Utils.Archive as Archive
import NStack.Module.ConfigFile (discover, configFile)
import NStack.Prelude.Applicative ((<&>))
import NStack.Prelude.FilePath (fpToText, fromFP, toFP)
import NStack.Prelude.Shell (runCmd_)
import NStack.Prelude.Monad (eitherToExcept)
import NStack.Prelude.Text (pprT, capitaliseT, prettyT, prettyT')
import NStack.Settings

-- | Available sub commands
data Command
  = InitCommand InitStack (Maybe BaseImage) GitRepo
  | NotebookCommand DebugOpt (Maybe DSLSource)
  | StartCommand DebugOpt DSLSource
  | StopCommand ProcessId
  | LogsCommand ProcessId
  | ServerLogsCommand
  | InfoCommand Bool
  | ListCommand (Maybe MethodType) Bool
  | ListModulesCommand Bool
  | DeleteModuleCommand ModuleName
  | ListProcessesCommand
  | GarbageCollectCommand
  | BuildCommand
  | LoginCommand HostName Int UserId SecretKey

data InitProject = InitProject ModuleName (Maybe Stack) (Maybe ModuleName) -- Name, Stack, Parent Module

instance M.ToMustache InitProject where
  toMustache (InitProject name stack parent) = M.object
    [ "name" ~> pprT name
    , "stack" ~> maybe "" show stack
    , "parent" ~> maybe "" pprT parent
    ]

-- Parser for Init command options
data InitStack = InitWorkflow | InitFramework | InitStack Stack

initCommand :: CCmdEff m => InitStack -> Maybe BaseImage -> GitRepo -> m ()
initCommand initStack mBase (GitRepo wantGitRepo) = do
  curDir <- R.pwd
  moduleName <- moduleNameFromDir curDir
  _ <- whenNotExistingProject
  (templateDirs, initProj) <- case initStack of
    InitWorkflow -> return (["workflow"], InitProject moduleName Nothing Nothing)
    InitFramework -> do
      iParentName <- parseModuleName . _baseImage $ baseImage
      return (["framework"], InitProject moduleName Nothing (Just iParentName))
    (InitStack stack) -> do
      iParentName <- parseModuleName . _baseImage $ baseImage
      return (["common", map toLower . show $ stack], InitProject moduleName (Just stack) (Just iParentName))
  -- copy the init files into the module dir
  liftIO $ mapM_ (createFromTemplate (fromFP curDir)) templateDirs
  -- run the template over them
  runTemplates curDir initProj
  when wantGitRepo initGitRepo
  liftIO . TIO.putStrLn $ "Module '" <> pprT moduleName <> "' successfully initialised at " <> T.pack (fromFP curDir)
  where
    -- hardcode the default image and version number temporarily
    baseImage = flip fromMaybe mBase $ case initStack of
      (InitStack stack) -> BaseImage . T.pack $ "NStack."++show stack++":0.24.0"
      _ -> BaseImage . T.pack $ "NStack.Python:0.24.0"

-- | Extract the module name from the current directory
moduleNameFromDir :: CCmdEff m => R.FilePath -> m ModuleName
moduleNameFromDir curDir = ((fmap capitaliseT . fpToText . FP.filename $ curDir) <&> (<> ":0.0.1-SNAPSHOT") >>= parseModuleName) `catchError` (\err -> throwError (
  "Your directory name, " <> FP.encodeString curDir <> ", is not a valid module name.\n"
  <> "Please rename the directory, starting with a capital letter.\n"
  <> err))

-- | Run project git/dir check
whenNotExistingProject  :: CCmdEff m => m ()
whenNotExistingProject =
  whenM (liftIO $ R.testdir ".git" <||> R.testfile configFile)
    (throwError "Found existing project, cancelling")

-- | process the initial module file using templates
runTemplates :: CCmdEff m => R.FilePath -> InitProject -> m ()
runTemplates curDir projInfo = do
  files <- R.fold (R.ls curDir) L.list
  traverse_ (runTemplate . fromFP) files
  where
    runTemplate :: CCmdEff m => FilePath -> m ()
    runTemplate inFile = do
      template <- eitherToExcept =<< liftIO (first show <$> M.localAutomaticCompile inFile)
      let newText = M.substitute template projInfo
      liftIO $ TIO.writeFile inFile newText

-- | init the module using Git
initGitRepo :: CCmdEff m => m ()
initGitRepo = liftIO $ do
  runCmd_ "git" ["init"]
  runCmd_ "git" $ T.words "add ."
  runCmd_ "git" $ T.words "commit -m 'Initial-Commit'"
  -- Sh.run "git" ["branch", "nstack"]

-- | Returns the artefacts needed to build a module
buildArtefacts :: CCmdEff m => m ByteString
buildArtefacts = do
  srcDir <- liftIO getCurrentDirectory >>= (discover . toFP)
  liftIO $ Archive.pack srcDir

printInfo :: ServerInfo -> Text
printInfo (ServerInfo ps meths ms) = prettyT' $
      block "Running processes:" (map M.ppr ps) </>
      block "Available functions:" (prettyPrintMethods $ Map.toList meths) </>
      M.text "Container modules:" </> showModules ms
        where

          showModules :: Map.Map ModuleName ModuleInfo -> M.Doc
          showModules = M.stack . fmap M.text . renderTree . mkTree . Map.toList

          renderTree :: Forest (ModuleName, ModuleInfo) -> [String]
          renderTree = fmap (showTree . fmap renderMod)
            where
              renderMod (modName, ModuleInfo{..}) = unpack . prettyT 120 $
                M.ppr modName M.<+> M.parens (M.commasep [maybe "Base" M.ppr _miStack,
                                                          if _miIsFramework then "Framework" else "User Code",
                                                          M.ppr _miImage])

          mkTree :: [(ModuleName, ModuleInfo)] -> Forest (ModuleName, ModuleInfo)
          mkTree mods = unfoldForest f baseMods
            where baseMods = filter (isNothing . _miParent . snd) mods
                  f mod'@(modName, _) = (mod', filter ((== Just modName) . _miParent . snd) mods)

printMethods :: [(MethodURI, TypeSignature)] -> Text
printMethods = prettyT' . M.stack . prettyPrintMethods

prettyPrintMethods :: [(MethodURI, TypeSignature)] -> [M.Doc]
prettyPrintMethods =  moduleMethodBlocks . fmap (fmap printMethod) . fmap Map.toList . nest
  where printMethod (uri, (TypeSignature ts)) = M.ppr uri <> " : " <> M.ppr ts
        moduleMethodBlocks = fmap (uncurry (block . unpack . pprT)) . Map.toList
        nest = foldr (\((MethodURI c d), a) m -> Map.unionWith (<>) m (newMap c d a)) Map.empty
        newMap c d a = Map.singleton c (Map.singleton d a)

block :: String -> [M.Doc] -> M.Doc
block label stack = M.text label </> M.indent 4 (M.stack stack) </> M.empty

showStartMessage :: (DSLSource, ProcessId) -> Text
showStartMessage ((DSLSource dsl), (ProcessId pId)) = "Started " <> dsl <> " as process " <> pId

showStopMessage :: ProcessId -> Text
showStopMessage (ProcessId pId) = "Successfully stopped process " <> pId

showModuleBuild :: ModuleName -> Text
showModuleBuild mName = "Module " <> pprT mName <> " built successfully. Use `nstack list functions` to see all available functions."

showWorkflowBuild :: ModuleName -> Text
showWorkflowBuild mName = "Workflow module " <> pprT mName <> " built successfully. Use `nstack list all` to see all available functions."

loginSettings :: HostName -> Int -> UserId -> SecretKey -> CCmd ()
loginSettings hostname port username pw = do modifySettings $ \s -> s & serverConn ?~ (ServerDetails (Just hostname)
                                                                                                     (Just port))
                                                                      & authSettings ?~ (NStackHMAC username pw)
                                             liftIO $ putStrLn "Successfully updated configuration"
