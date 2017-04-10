{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch)
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Classes (ask)  -- from: monad-classes
import Control.Monad.Trans (liftIO)
import Control.Monad.Except (runExceptT, throwError) -- mtl
import Control.Monad.Extra (ifM) -- mtl
import Control.Monad.Reader (runReaderT) -- mtl
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict) -- from: bytestring
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, encode, decode)
import Data.List (isSuffixOf)
import Data.Text (pack, unpack, Text, splitOn, replace, intercalate)
import qualified Data.Text.IO as TIO
import Options.Applicative      -- optparse-applicative
import System.Console.Haskeline (runInputT, defaultSettings, getExternalPrint)
import System.Info (os)
import System.Exit (exitFailure)
import qualified Turtle as R        -- turtle
import Turtle((%), (<>))                  -- turtle

import Network.HTTP.Client
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (ok200)

import NStack.CLI.Auth (signRequest, allowSelfSigned)
import NStack.CLI.Parser (cmds)
import NStack.CLI.Types
import NStack.CLI.Commands
import qualified NStack.CLI.Commands as CLI
import NStack.Common.Environment (httpApiPort)
import NStack.Comms.Types
import NStack.Module.ConfigFile (configFile, workflowFile, projectFile, getProjectFile, _projectModules, _cfgName, getConfigFile)
import NStack.Module.Parser (getDslName)
import NStack.Prelude.FilePath (fromFP)
import NStack.Prelude.Text (pprT, prettyLinesOr, joinLines, showT)
import NStack.Settings
import NStack.Utils.Debug (versionMsg)

-- | Global app options
opts :: Parser (Maybe Command)
opts = flag' Nothing (long "version" <> hidden) <|> (Just <$> cmds)

-- | Main - calls to remote nstack-server
main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnError) opts'
  manager <- newManager $ mkManagerSettings allowSelfSigned Nothing
  server <- serverPath
  let transport = Transport $ callWithHttp manager server
  maybe printVersion (runClient transport . run) cmd
  where
    printVersion = putStrLn versionMsg
    opts' = info (helper <*> opts) (fullDesc
                                    <> progDesc "NStack CLI"
                                    <> header "nstack - a command-line interface into the NStack platform" )

catLogs :: [LogsLine] -> Text
catLogs = joinLines . fmap _logLine

-- A couple of partial functions here (init, replace), but results in a nice error for the user
-- | Add an import line when passing in a fully qualified workflow name
addImport :: DSLSource -> DSLSource
addImport (DSLSource m) = DSLSource . withImport m . intercalate "." . init $ splitOn "." m
  where importLine t = "import " <> t <> " as M" <> "\n"
        withImport a b = importLine b <> replace b "M" a

run :: Command -> CCmd ()
run (InitCommand initStack mBase gitRepo) = CLI.initCommand initStack mBase gitRepo
run (StartCommand debug dsl) = callServer startCommand (addImport dsl, debug) CLI.showStartMessage
run (NotebookCommand debug mDsl) = do
  liftIO . putStrLn $ "NStack Notebook - import modules, write a workflow, and press " <> endStream <> " when finished to start it: "
  dsl <- maybe (liftIO $ DSLSource <$> TIO.getContents) pure mDsl
  liftIO . putStrLn $ "Building and running NStack Workflow. Please wait. This may take some time."
  callServer startCommand (dsl, debug) CLI.showStartMessage
  where endStream = if os == "mingw32" then "<Ctrl-Z>" else "<Ctrl-D>"
run (StopCommand pId)         = callServer stopCommand pId CLI.showStopMessage
run (LogsCommand pId)         = callServer logsCommand pId catLogs
run ServerLogsCommand         = callServer serverLogsCommand () catLogs
run (InfoCommand fAll)        = callServer infoCommand fAll CLI.printInfo
run (ListCommand mType fAll)  = callServer listCommand (mType, fAll) CLI.printMethods
run (ListModulesCommand fAll) = callServer listModulesCommand fAll (`prettyLinesOr` "No registered images")
run (DeleteModuleCommand m)   = callServer deleteModuleCommand m (maybe "Module deleted" pprT)
run (ListProcessesCommand)    = callServer listProcessesCommand () (`prettyLinesOr` "No running processes")
run (GarbageCollectCommand)   = callServer gcCommand () (`prettyLinesOr` "Nothing removed")
run (BuildCommand) =
  ifM (R.testfile projectFile) projectBuild
    (ifM (R.testfile configFile) containerModule
      (ifM (R.testfile workflowFile) workflowModule
        (throwError (unpack $ R.format ("A valid nstack build file ("%R.fp%", "%R.fp%", "%R.fp%") was not found") projectFile configFile workflowFile))))
  where
    projectBuild = do
      liftIO . putStrLn $ "Building NStack Project. Please wait. This may take some time."
      modules <- _projectModules <$> getProjectFile
      projectPath <- R.pwd
      -- change to each dir and run build
      forM_ modules (\modPath -> R.cd modPath >> run BuildCommand >> R.cd projectPath)
    containerModule = do
      modName <- _cfgName <$> (R.pwd >>= getConfigFile)
      liftIO . putStrLn $ "Building NStack Container module " <> unpack (CLI.localModName modName) <> ". Please wait. This may take some time."
      package <- CLI.buildArtefacts
      callServer buildCommand (BuildTarball $ toStrict package) showModuleBuild
    workflowModule = do
      -- TODO - can we parse the workflow on client to surface syntatic errors quicker?
      workflowSrc <- liftIO . TIO.readFile . fromFP $ workflowFile
      modName <- getDslName workflowSrc
      liftIO . putStrLn $ "Building NStack Workflow module " <> unpack (CLI.localModName modName) <> ". Please wait. This may take some time."
      callServer buildWorkflowCommand (WorkflowSrc workflowSrc, modName) showWorkflowBuild
run (LoginCommand a b c d)    = CLI.loginSettings a b c d
run (RegisterCommand userName email mServer) = CLI.registerCommand userName email mServer
run (SendCommand path' snippet) = CLI.sendCommand path' snippet

-- | Run a command on the user client
runClient :: Transport -> CCmd () -> IO ()
runClient t c = runInputT defaultSettings (runExceptT (runReaderT (runSettingsT c) t) >>= either (\s -> liftIO (putStrLn s >> exitFailure)) return)

callServer :: ApiCall a b -> a -> (b -> Text) -> CCmd ()
callServer fn arg formatter = do
  (Transport t) <- ask
  r <- t fn arg
  printer <- (. addTrailingNewline) <$> liftInput getExternalPrint
  liftIO . printer . unpack $ formatResult formatter r
  where
    -- Currently, some messages are not \n-terminated (e.g.  showStartMessage),
    -- and some are (e.g. megaparsec errors).
    -- So we need this hack until we develop consistent conventions.
    addTrailingNewline :: String -> String
    addTrailingNewline s =
      if "\n" `isSuffixOf` s
        then s
        else s ++ "\n"

formatResult :: (a -> Text) -> Result a -> Text
formatResult f (Result      a) = f a
formatResult _ (ClientError e) = "There was an error communicating with the NStack server:\n\nError: " <> e
formatResult _ (ServerError e) = "An error was returned from the NStack Server:\n\nError: " <> e

serverPath :: IO String
serverPath = do
  s <-  (^. serverConn) <$> runSettingsT settings
  let (HostName domain) = ((^. serverHostname) =<< s) `withDefault` HostName "localhost"
  let port' = ((^. serverPort) =<< s) `withDefault` httpApiPort
  return $ "https://" <> unpack domain <> ":" <> show port' <> "/"
    where withDefault = flip fromMaybe

callWithHttp :: CCmdEff m => Manager -> String -> ApiCall a b -> a -> m (Result b)
callWithHttp manager hostname (ApiCall name) args = do
  auth <- (^. authSettings) <$> settings
  liftIO $ maybe (return $ err) (doCall manager path' $ encode args) auth
    where path' = hostname <> unpack name
          err = ClientError "Missing or invalid credentials. Please run the 'nstack set-server' command as described in your email."

handleHttpErr :: Monad m => HttpException -> m (Result a)
handleHttpErr e = return . ClientError $ "Exception sending HTTP request: " <> showT e

doCall :: Serialize a => Manager -> String -> ByteString -> AuthSettings -> IO (Result a)
doCall manager path' body auth = (do
  response <- CLI.callWithCookieJar doCall'
  let status = responseStatus response
  if (status == ok200)
     then (return . either decodeError serverResult . decode . toStrict $ responseBody response)
     else (return . ServerError $ showT status)) `catch` handleHttpErr
  where decodeError = ClientError . ("Cannot decode return value: " <>) . pack
        serverResult = either ServerError Result . _serverReturn
        -- 15 * 60 * 1000 * 1000 == 15 minutes in microseconds
        incTimeout r = r { responseTimeout = responseTimeoutMicro (15 * 60 * 1000 * 1000) }

        doCall' cookieJar' = do
          signedRequest <- signRequest auth . addBody =<< parseRequest path'
          (cookieRequest, _) <- insertCookiesIntoRequest signedRequest cookieJar' <$> R.date
          httpLbs (incTimeout cookieRequest) manager

        addBody :: Request -> Request
        addBody r = r { requestBody = RequestBodyBS body }
