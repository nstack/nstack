module NStack.CLI.Commands (
  CCmd,
  initCommand,
  buildArtefacts,
  InitStack(..),
  Command(..),
  loginSettings,
  showStartMessage,
  showStopMessage,
  localModName,
  printInfo,
  printMethods,
  printProcesses,
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
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Tree (Forest, unfoldForest)
import Data.Tree.View (showTree)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Filesystem.Path.CurrentOS as FP -- system-filepath
import Network.HTTP.Client hiding (responseStatus)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq hiding (responseCookieJar)
import Util ((<||>))                 -- ghc
import System.Directory (getCurrentDirectory, getXdgDirectory, XdgDirectory(..))
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
import NStack.Comms.Types (GitRepo(..), ProcessId(..), ProcessInfo(..), ModuleInfo(..), ServerInfo(..), MethodType, TypeSignature(..), DSLSource(..))
import NStack.Module.Types (Stack, BaseImage(..), DebugOpt(..), ModuleName(..), FnName, QFnName, Qualified(..), showShortModuleName)
import NStack.Module.Parser (parseModuleName)
import qualified NStack.Utils.Archive as Archive
import NStack.Module.ConfigFile (configFile)
import NStack.Prelude.Applicative ((<&>))
import NStack.Prelude.FilePath (fpToText, fromFP, toFP)
import NStack.Prelude.Shell (runCmd_)
import NStack.Prelude.Monad (eitherToExcept, maybeToExcept)
import NStack.Prelude.Text (pprT, capitaliseT, prettyT, prettyT')
import NStack.Settings

type ServerAddr = String
type Path = String
type Snippet = String

-- | Available sub commands
data Command
  = InitCommand InitStack (Maybe BaseImage) GitRepo
  | NotebookCommand DebugOpt (Maybe DSLSource)
  | StartCommand DebugOpt ModuleName FnName
  | StopCommand ProcessId
  | LogsCommand ProcessId
  | ConnectCommand ProcessId
  | ServerLogsCommand
  | InfoCommand Bool
  | ListCommand (Maybe MethodType) Bool
  | ListModulesCommand Bool
  | DeleteModuleCommand ModuleName
  | ListProcessesCommand
  | GarbageCollectCommand
  | BuildCommand
  | RegisterCommand UserName Email ServerAddr
  | SendCommand Path Snippet
  | LoginCommand HostName Int UserId SecretKey

data InitProject = InitProject Text (Maybe Stack) (Maybe ModuleName) -- Name, Stack, Parent Module

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
  tModuleName <- moduleNameFromDir curDir
  _ <- whenNotExistingProject
  (templateDirs, initProj) <- case initStack of
    InitWorkflow -> return (["workflow"], InitProject tModuleName Nothing Nothing)
    InitFramework -> do
      iParentName <- parseModuleName . _baseImage $ baseImage
      return (["framework"], InitProject tModuleName Nothing (Just iParentName))
    (InitStack stack) -> do
      iParentName <- parseModuleName . _baseImage $ baseImage
      return (["common", map toLower . show $ stack], InitProject tModuleName (Just stack) (Just iParentName))
  -- copy the init files into the module dir
  liftIO $ mapM_ (createFromTemplate (fromFP curDir)) templateDirs
  -- run the template over them
  runTemplates curDir initProj
  when wantGitRepo initGitRepo
  liftIO . TIO.putStrLn $ "Module '" <> pprT tModuleName <> "' successfully initialised at " <> T.pack (fromFP curDir)
  where
    -- hardcode the default image and version number temporarily
    baseImage = flip fromMaybe mBase $ case initStack of
      (InitStack stack) -> BaseImage . T.pack $ "NStack."++show stack++":0.25.0"
      _ -> BaseImage . T.pack $ "NStack.Python:0.25.0"

-- | Extract the module name from the current directory
moduleNameFromDir :: CCmdEff m => R.FilePath -> m Text
moduleNameFromDir curDir = ((fmap capitaliseT . fpToText . FP.filename $ curDir) <&> (<> ":0.0.1-SNAPSHOT") >>= parseModuleName) `catchError` (\err -> throwError (
  "Your directory name, " <> FP.encodeString curDir <> ", is not a valid module name.\n"
  <> err)) >>= return . localModName

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
  runCmd_ "git" ["add", "."]
  runCmd_ "git" ["commit", "-m", "Initial Commit"]
  -- Sh.run "git" ["branch", "nstack"]

-- | Returns the artefacts needed to build a module
buildArtefacts :: CCmdEff m => m ByteString
buildArtefacts = do
  srcDir <- toFP <$> liftIO getCurrentDirectory
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

printMethods :: [(QFnName, TypeSignature)] -> Text
printMethods = prettyT' . M.stack . prettyPrintMethods

prettyPrintMethods :: [(QFnName, TypeSignature)] -> [M.Doc]
prettyPrintMethods =  moduleMethodBlocks . fmap (fmap printMethod) . fmap Map.toList . nest
  where printMethod (uri, (TypeSignature ts)) = M.ppr uri <> " : " <> M.ppr ts
        moduleMethodBlocks = fmap (uncurry (block . unpack . pprT)) . Map.toList
        nest = foldr (\((Qualified c d), a) m -> Map.unionWith (<>) m (newMap c d a)) Map.empty
        newMap c d a = Map.singleton c (Map.singleton d a)

printProcesses :: [ProcessInfo] -> Text
printProcesses [] = "No running processes"
printProcesses xs = prettyT' $ M.text "pid" <> M.spaces 4 <> M.text "time" <> M.spaces 21 <> M.text "command" </>
                    M.text (replicate 40 '=') </>
                    M.stack (map M.ppr xs)


block :: String -> [M.Doc] -> M.Doc
block label stack = M.text label </> M.indent 4 (M.stack stack) </> M.empty

showStartMessage :: ProcessInfo -> Text
showStartMessage (ProcessInfo (ProcessId pId) _ _) = "Successfully started as process " <> pId

showStopMessage :: ProcessId -> Text
showStopMessage (ProcessId pId) = "Successfully stopped process " <> pId

-- HACK - to remove once we have username on CLI / remove modulename parsing
-- Currently used to display the ModuleName on the CLI without the default `nstack` author
localModName :: ModuleName -> T.Text
localModName = last . T.splitOn "nstack/" . T.pack . showShortModuleName

showModuleBuild :: ModuleName -> Text
showModuleBuild mName = "Module " <> pprT mName <> " built successfully. Use `nstack list functions` to see all available functions."

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
    mkServerAddr serverHost = "http://" <> T.unpack serverHost <> ":8080" <> path
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
  writeFile cookieFilePath . show . responseCookieJar $ res
  return res
