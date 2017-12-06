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
  printProcesses,
  printScheduledProcesses,
  showModuleBuild,
  registerCommand,
  sendCommand,
  callWithCookieJar
) where

import qualified Control.Exception as E
import qualified Control.Foldl as L
import Control.Lens ((&), (?~), (^.), to, (.~), (^?), _Just)
import Control.Monad.Except     -- mtl
import Control.Monad.Trans ()    -- mtl
import Control.Monad.Extra (whenM) -- mtl
import Data.Aeson
import Data.Bifunctor (first)      -- bifunctors
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text, unpack)
import Data.Tree (Forest, unfoldForest)
import Data.Tree.View (showTree)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Yaml as Y
import qualified Filesystem.Path.CurrentOS as FP -- system-filepath
import Network.HTTP.Client hiding (responseStatus, Proxy)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq hiding (responseCookieJar, Proxy)
import Util ((<||>))                 -- ghc
import System.Directory (getXdgDirectory, XdgDirectory(..), createDirectoryIfMissing)
import System.IO.Error (isDoesNotExistError)
import qualified Text.Mustache as M  -- mustache
import Text.Mustache ((~>))          -- mustache
import qualified Text.PrettyPrint.Mainland as M
import Text.PrettyPrint.Mainland ((</>))
import qualified Turtle as R         -- turtle

import NStack.Auth
import NStack.CLI.Auth (allowSelfSigned)
import NStack.CLI.Types
import NStack.CLI.Templates (createFromTemplate)
import NStack.Comms.Types
import NStack.Module.Name (ModuleName, ModuleRef, ModuleURI(..), showShortModuleUri)
import NStack.Module.Parser (parseModuleName)
import NStack.Module.QMap (QMap(..))
import NStack.Module.Types
import NStack.Module.Version (ExactRelease(..), SemVer(..))
import qualified NStack.Utils.Archive as Archive
import NStack.Module.ConfigFile (configFile, workflowFile, ConfigStack(..), mkStackParent)
import NStack.Prelude.Applicative ((<&>))
import NStack.Prelude.FilePath (fpToText, fromFP, directory)
import NStack.Prelude.Shell (runCmd_)
import NStack.Prelude.Monad (eitherToExcept, maybeToExcept)
import NStack.Prelude.Text (pprT, capitaliseT, prettyT, prettyT')
import NStack.Settings

type ServerAddr = String
type Path = String
type Snippet = String

-- | Available sub commands
data Command
  = InitCommand InitStack GitRepo
  | NotebookCommand DebugOpt (Maybe DSLSource)
  | StartCommand DebugOpt ModuleName FnName
  | StopCommand ProcessId
  | LogsCommand ProcessId
  | ConnectCommand ProcessId
  | ServerLogsCommand
  | InfoCommand Bool
  | ListFnCommand MethodType Bool
  | ListTypesCommand Bool
  | ListAllCommand Bool
  | ListModulesCommand Bool
  | DeleteModuleCommand ModuleRef
  | ListProcessesCommand
  | ListStoppedCommand (Maybe StoppedFrom) (Maybe StoppedAmount)
  | GarbageCollectCommand
  | BuildCommand
  | RegisterCommand UserName Email ServerAddr
  | SendCommand Path Snippet
  | TestCommand ModuleRef FnName Snippet
  | LoginCommand HostName Int UserId SecretKey
  | ListScheduled


-- Parser for Init command options
data InitStack = InitWorkflow | InitStack Language | InitFramework BaseImage
data TemplateOut = TemplateOut ModuleName (Maybe Y.Value)

instance M.ToMustache TemplateOut where
  toMustache (TemplateOut name stackOrParent) = M.object [ "name" ~> localModName name
                                                         , "stackOrParent" ~> maybe "" (decodeUtf8 . Y.encode) stackOrParent ]

-- TODO - these should be moved to the server
snapshot :: (FedoraVersion, FedoraSnapshot)
snapshot = (25, 0)

-- The default api version for a language on `nstack init`
langStacks :: Language -> APIVersion
langStacks _ = 2

initCommand :: CCmdEff m => InitStack -> GitRepo -> m ()
initCommand initStack (GitRepo wantGitRepo) = do
  curDir <- R.pwd
  tModuleName <- moduleNameFromDir curDir
  _ <- whenNotExistingProject
  (templateDirs, initProj) <- case initStack of
    InitWorkflow -> return (["workflow"], Nothing)
    InitFramework baseImage -> do
      iParentName <- parseModuleName . _baseImage $ baseImage
      return (["framework"], Just . mkStackParent . Right $ iParentName)
    InitStack lang -> do
      let cfgStack = ConfigStack lang (langStacks lang) snapshot
      return (["common", map toLower . show $ lang], Just . mkStackParent . Left $ cfgStack)
  -- copy the init files into the module dir
  liftIO $ mapM_ (createFromTemplate (fromFP curDir)) templateDirs
  -- run the template over them
  runTemplates curDir tModuleName initProj
  when wantGitRepo initGitRepo
  liftIO . TIO.putStrLn $ "Module '" <> localModName tModuleName <> "' successfully initialised at " <> T.pack (fromFP curDir)

-- | Extract the module name from the current directory
moduleNameFromDir :: CCmdEff m => R.FilePath -> m ModuleName
moduleNameFromDir curDir = ((fmap capitaliseT . fpToText . FP.filename $ curDir) <&> (<> ":0.0.1-SNAPSHOT") >>= parseModuleName) `catchError` (\err -> throwError (
  "Your directory name, " <> FP.encodeString curDir <> ", is not a valid module name.\n"
  <> err))

-- | Run project git/dir check
whenNotExistingProject  :: CCmdEff m => m ()
whenNotExistingProject =
  whenM (liftIO $ R.testdir ".git" <||> R.testfile configFile)
    (throwError "Found existing project, cancelling")

-- | process the initial module files using templates
runTemplates :: CCmdEff m => R.FilePath -> ModuleName -> Maybe Y.Value -> m ()
runTemplates curDir modName stackOrParent = do
  files <- R.fold (R.ls curDir) L.list
  traverse_ (runTemplate . fromFP) files
  where
    runTemplate :: CCmdEff m => FilePath -> m ()
    runTemplate inFile = do
      template <- eitherToExcept =<< liftIO (first show <$> M.localAutomaticCompile inFile)
      let newText = M.substitute template (TemplateOut modName stackOrParent)
      liftIO $ TIO.writeFile inFile newText


-- HACK - to remove once we have username on CLI / remove modulename parsing
-- Currently used to display the ModuleName on the CLI without the default `nstack` author
localModName :: ModuleName -> Text
localModName = last . T.splitOn "nstack/" . T.pack . showShortModuleUri


-- | init the module using Git
initGitRepo :: CCmdEff m => m ()
initGitRepo = liftIO $ do
  runCmd_ "git" ["init"]
  runCmd_ "git" ["add", "."]
  runCmd_ "git" ["commit", "-m", "Initial Commit"]
  -- Sh.run "git" ["branch", "nstack"]

-- | Returns the artefacts needed to build a module
-- | TODO: Needs to be updated to take the language from the nstack.yaml to determine the required files
buildArtefacts
  :: CCmdEff m
  => FilePath -- ^ directory
  -> [FilePath] -- ^ globs from the files section of nstack.yaml
  -> m ByteString
buildArtefacts dir globs = do
  let std_files = [configFile, workflowFile, "setup.py", "service.py", "requirements.txt", "service.r"]
  liftIO $ Archive.expandCheckPack dir std_files globs

printInfo :: ServerInfo -> Text
printInfo (ServerInfo ps stopped meths ms) = prettyT' $
      block "Running processes:" (map M.ppr ps) </>
      block "Stopped processes:" (map M.ppr stopped) </>
      block "Available functions:" (prettyPrintMethods $ fmap typeSignature meths) </>
      M.text "Container modules:" </> showModules ms
        where

          typeSignature (MethodInfo t _) = t

          showModules :: Map.Map ModuleRef ModuleInfo -> M.Doc
          showModules = M.stack . fmap M.text . renderTree . mkTree . Map.toList

          renderTree :: Forest (ModuleRef, ModuleInfo) -> [String]
          renderTree = fmap (showTree . fmap renderMod)
            where
              renderMod (modName, ModuleInfo{..}) = unpack . prettyT 120 $
                M.ppr modName M.<+> M.parens (M.commasep [maybe "Base" M.ppr _miStack,
                                                          if _miIsFramework then "Framework" else "User Code",
                                                          M.ppr _miImage])

          mkTree :: [(ModuleRef, ModuleInfo)] -> Forest (ModuleRef, ModuleInfo)
          mkTree mods = unfoldForest f baseMods
            where baseMods = filter (isNothing . _miParent . snd) mods
                  f mod'@(modName, _) = (mod', filter ((== Just modName) . _miParent . snd) mods)

printMethods :: M.Pretty a => QMap a TypeSignature -> Text
printMethods = prettyT' . M.stack . prettyPrintMethods

prettyPrintMethods :: M.Pretty a => QMap a TypeSignature -> [M.Doc]
prettyPrintMethods =  moduleMethodBlocks . methodDocs
  where printMethod (uri, (TypeSignature ts)) = M.ppr uri <> " : " <> M.ppr ts
        printMethod (uri, TypeDefinition td) = "type " <> M.ppr uri <> " = " <> M.ppr td
        methodDocs (QMap inner) = fmap (Map.foldMapWithKey (\k a -> [curry printMethod k a])) inner
        moduleMethodBlocks = fmap (uncurry (block . unpack . pprT)) . Map.toList

printProcesses :: forall a. ProcPrintable a => [ProcessInfo a] -> Text
printProcesses = \case
  [] -> emptyMessage p
  xs -> prettyT' $ M.text "pid" <> M.spaces 4 <> M.text "time" <> M.spaces 21 <> columnHeader p <> M.text "command" </>
                   M.text (replicate (40 + columnWidth p) '=') </>
                   M.stack (map M.ppr xs)
  where p = Proxy @a

class (M.Pretty (ProcessInfo a)) => ProcPrintable a where
  emptyMessage :: Proxy a -> Text
  columnHeader :: Proxy a -> M.Doc
  columnWidth  :: Proxy a -> Int

instance ProcPrintable () where
  emptyMessage _ = "No running processes"
  columnHeader _ = mempty
  columnWidth  _ = 0

instance ProcPrintable StopTime where
  emptyMessage _ = "No stopped processes in range"
  columnHeader _ = M.text "stop time" <> M.spaces 16
  columnWidth  _ = 25

printScheduledProcesses :: [(ProcessInfo (), [ScheduledTime])] -> Text
printScheduledProcesses [] = "No scheduled processes"
printScheduledProcesses xs = prettyT' $
  M.stack items
  where
    items = fmap (\(pInfo, datetimes) ->
                    M.text "Process " <> M.ppr pInfo </>
                    M.text "Next scheduled times: " <> M.stack (M.ppr <$> datetimes) </> M.text (replicate 10 '-')) xs

block :: String -> [M.Doc] -> M.Doc
block label stack = M.text label </> M.indent 4 (M.stack stack) </> M.empty

showStartMessage :: ProcessInfo () -> Text
showStartMessage (ProcessInfo (ProcessId pId) _ _ _) = "Successfully started as process " <> pId

showStopMessage :: (ProcessInfo a) -> Text
showStopMessage (ProcessInfo (ProcessId pId) _ _ _) = "Successfully stopped process " <> pId

showModuleBuild :: ModuleRef -> Text
showModuleBuild mName = "Module " <> T.pack (showShortModuleUri mName) <> " built successfully" <> maybeFullSnap <>  ". Use `nstack list functions` to see all available functions."
  where maybeFullSnap = case release (version mName) of
                          (Snap _) -> " (as " <> pprT mName  <> ")"
                          _        -> ""

loginSettings :: HostName -> Int -> UserId -> SecretKey -> CCmd ()
loginSettings hostname port username pw = do modifySettings $ \s -> s & serverConn ?~ (ServerDetails (Just hostname)
                                                                                                     (Just port))
                                                                      & authSettings ?~ (NStackHMAC username pw)
                                             liftIO $ putStrLn "Successfully updated configuration"


-- | Attempt to register with the auth server directly from the CLI
registerCommand :: UserName -> Email -> ServerAddr -> CCmd ()
registerCommand (UserName userName) (Email email) serverAddr = do
  eitherToExcept =<< liftIO (callServer `E.catch` wreqErrorHandler)
  liftIO . TIO.putStrLn $ "Thanks for registering " <> userName <> ", an email will be sent to " <> email <> " shortly"
  where
    serverAddr' = "https://" <> serverAddr <> "/register"
    body = object ["username" .= userName, "email" .= email]
    callServer = post serverAddr' body $> Right ()

sendCommand :: Path -> Snippet -> CCmd ()
sendCommand path snippet = do
  event <- mkEvent
  (HostName serverHost) <- maybeToExcept "server not set in config file" =<< (^? serverConn . _Just . serverHostname . _Just) <$> settings
  eitherToExcept =<< liftIO (callServer serverHost event `E.catch` wreqErrorHandler)
  liftIO . TIO.putStrLn $ "Event sent successfully"
  where
    mkServerAddr serverHost = "https://" <> T.unpack serverHost <> ":8083" <> path
    -- convert snippet to json event we can send
    mkEvent = do
      p <- eitherToExcept (eitherDecodeStrict' (encodeUtf8 . T.pack $ snippet) :: Either String Value)
      return $ object ["params" .= p]

    callServer serverHost event = callWithCookieJar (doCall' serverHost event) $> Right ()

    doCall' serverHost event cookieJar' = do
      manager' <- newManager $ mkManagerSettings allowSelfSigned Nothing
      let opts = defaults & cookies .~ (Just cookieJar')
                          & manager .~ (Right manager')
      postWith opts (mkServerAddr serverHost) event


wreqErrorHandler :: HttpException -> IO (Either String ())
wreqErrorHandler (HttpExceptionRequest _ (StatusCodeException s msg))
  | s ^. responseStatus . statusCode == 400 = genError msg
  | otherwise = s ^. responseStatus . statusMessage . to genError
  where
    genError = return . Left . ("Error: " <>) . T.unpack . decodeUtf8
wreqErrorHandler e = return . Left . show $ e

-- | Wrap up a HTTP Client call to use a file-based cookie jar
-- should work for HTTP.Client and Wreq (TODO - move nstack-cli fully to wreq)
-- `CookieJar` has well-behaving Show and Read instances so text-based serialisation should work
callWithCookieJar :: (CookieJar -> IO (Response body)) -> IO (Response body)
callWithCookieJar mkRequest = do
  cookieFilePath <- getXdgDirectory XdgCache "nstack-session.txt"
  reqCookieJar <- E.catchJust (guard . isDoesNotExistError)
    (readFile cookieFilePath >>= (return . read :: String -> IO CookieJar))
    (const . return . createCookieJar $ [])

  res <- mkRequest reqCookieJar
  -- TODO - we should take in the current cookieJar and update here, wreq should do this automatically
  -- let (cookieJar', res') = updateCookieJar res

  createDirectoryIfMissing True (directory cookieFilePath) -- just in case ~/.cache does not exist
  writeFile cookieFilePath . show . responseCookieJar $ res
  return res
