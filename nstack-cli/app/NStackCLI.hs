module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, handleJust, displayException, fromException, SomeException, AsyncException(UserInterrupt))
import Control.Lens
import Control.Monad (forM_, forever, void)
import Control.Monad.Classes (ask)  -- from: monad-classes
import Control.Monad.Trans (liftIO)
import Control.Monad.Except (runExceptT, throwError) -- mtl
import Control.Monad.Extra (ifM, (||^)) -- extra
import Control.Monad.Reader (runReaderT) -- mtl
import Control.Concurrent.Async (withAsync, waitCatch, waitEitherCatch)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict) -- from: bytestring
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize, encode, decode)
import Data.List (isSuffixOf)
import Data.Text (pack, unpack, Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import Options.Applicative      -- optparse-applicative
import qualified System.Console.Haskeline as HL
import System.Info (os)
import System.Exit (exitFailure, ExitCode)
import System.IO (hPutStrLn, stderr, hIsTerminalDevice, stdin, stdout)
import System.IO.Error (isEOFError)
import System.Random (randomIO)
import qualified Turtle as R        -- turtle
import Turtle((%), (<>))                  -- turtle

import Network.HTTP.Client hiding (host)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (ok200)
import qualified Network.WebSockets as WS

import NStack.CLI.Auth (signRequest, allowSelfSigned)
import NStack.CLI.Parser (cmds)
import NStack.CLI.Types
import NStack.CLI.Commands
import qualified NStack.CLI.Commands as CLI
import NStack.Common.Environment (httpApiPort)
import NStack.Comms.Types
import NStack.Comms.ApiHashValue (apiHashValue)
import NStack.Module.ConfigFile (ConfigFile(..), configFile, getConfigFile,
  workflowFile, projectFile, getProjectFile, _projectModules)
import NStack.Module.Types (ModuleName, FnName, Qualified(..))
import NStack.Prelude.Text (pprT, prettyLinesOr, joinLines, showT)
import NStack.Prelude.FilePath (fromFP)
import NStack.Settings
import NStack.Utils.Debug (versionMsg)

-- | Global app options
opts :: Parser (Maybe Command)
opts = flag' Nothing (long "version" <> hidden) <|> (Just <$> cmds)

-- | Main - calls to remote nstack-server
main :: IO ()
main = handleJust
  (\e -> if
    | Just (_ :: ExitCode) <- fromException e -> Nothing
    | Just UserInterrupt <- fromException e -> Nothing
    | otherwise -> Just e
  )
  (hPutStrLn stderr . ("Error: "++) . displayException @SomeException)
  (do
  cmd <- customExecParser (prefs showHelpOnError) opts'
  manager <- newManager $ mkManagerSettings allowSelfSigned Nothing
  server <- runSettingsT serverPath
  let transport = Transport $ callWithHttp manager server
  maybe printVersion (runClient transport . run) cmd
  )
  where
    printVersion = putStrLn versionMsg
    opts' = info (helper <*> opts) (fullDesc
                                    <> progDesc "NStack CLI"
                                    <> header "nstack - a command-line interface into the NStack platform" )

catLogs :: [LogsLine] -> Text
catLogs = joinLines . fmap _logLine

-- | Given a module name and a function name from that module,
-- create a notebook source that runs that function name.
--
-- See the 'notebook' parser in "NStack.Lang.DSL.Parser".
formatNotebook :: ModuleName -> FnName -> DSLSource
formatNotebook module_name fn_name = DSLSource $
  "import " <> pprT module_name <> " as M" <> "\n" <>
  "M." <> pprT fn_name


run :: Command -> CCmd ()
run (InitCommand initStack mBase gitRepo) = CLI.initCommand initStack mBase gitRepo
run (StartCommand debug module_name fn_name) = callServer startCommand (formatNotebook module_name fn_name, debug) CLI.showStartMessage
run (NotebookCommand debug mDsl) = do
  liftInput . HL.outputStrLn $ "NStack Notebook - import modules, write a workflow, and press " <> endStream <> " when finished to start it: "
  dsl <- maybe (liftIO $ DSLSource <$> TIO.getContents) pure mDsl
  liftInput . HL.outputStrLn $ "Building and running NStack Workflow. Please wait. This may take some time."
  callServer startCommand (dsl, debug) CLI.showStartMessage
  where endStream = if os == "mingw32" then "<Ctrl-Z>" else "<Ctrl-D>"
run (StopCommand pId)         = callServer stopCommand pId CLI.showStopMessage
run (LogsCommand pId)         = callServer logsCommand pId catLogs
run ServerLogsCommand         = callServer serverLogsCommand () catLogs
run (InfoCommand fAll)        = callServer infoCommand fAll CLI.printInfo
run (ListCommand mType fAll)  = callServer listCommand (mType, fAll) CLI.printMethods
run (ListModulesCommand fAll) = callServer listModulesCommand fAll (`prettyLinesOr` "No registered images")
run (DeleteModuleCommand m)   = callServer deleteModuleCommand m (const $ "Module deleted: " <> pprT m)
run (ListProcessesCommand)    = callServer listProcessesCommand () CLI.printProcesses
run (GarbageCollectCommand)   = callServer gcCommand () (`prettyLinesOr` "Nothing removed")
run (ConnectCommand pId)      = connectStdInOut pId
run (BuildCommand dropBadModules) =
  ifM (R.testfile projectFile) projectBuild
    (ifM (R.testfile configFile ||^ R.testfile workflowFile) workflowModule
      (throwError (unpack $ R.format ("A valid nstack build file ("%R.fp%", "%R.fp%", "%R.fp%") was not found") projectFile configFile workflowFile)))
  where
    projectBuild = do
      liftInput . HL.outputStrLn $ "Building NStack Project. Please wait. This may take some time."
      modules <- _projectModules <$> getProjectFile
      forM_ modules $ \modPath -> do
        liftInput . HL.outputStrLn . unpack $ R.format ("Building " % R.fp) modPath
        buildDirectory dropBadModules modPath
    workflowModule = do
      liftInput . HL.outputStrLn $ "Building an NStack module. Please wait. This may take some time."
      buildDirectory dropBadModules "."
run (LoginCommand a b c d)    = CLI.loginSettings a b c d
run (RegisterCommand userName email mServer) = CLI.registerCommand userName email mServer
run (SendCommand path' snippet) = CLI.sendCommand path' snippet
run (TestCommand mod' fn snippet) = do
  path' <- liftIO randomPath
  (Transport t) <- ask
  r <- t testCommand ((Qualified mod' fn), HttpPath path')
  (ProcessInfo pId _ _) <- case r of
              (ServerError e) -> throwError $ unpack e
              (ClientError e)-> throwError $ unpack e
              (Result v) -> return v
  wait -- we have a few manual waits to account for the lack of enforced sequencing between the events we're dealing with
  run (SendCommand (unpack path') snippet)
  wait
  run (StopCommand pId)
  wait
  run (LogsCommand pId)
    where wait = liftIO $ threadDelay (500{-ms-} * 1000)

randomPath :: IO Text
randomPath = ("/" <>) . UUID.toText <$> randomIO

-- | Build an nstack module (not a project) that resides in the given
-- directory
buildDirectory :: DropBadModules -> R.FilePath -> CCmd ()
buildDirectory dropBadModules dir = do
  globs <- ifM (R.testfile (dir R.</> configFile))
    (do
      config <- liftIO $ getConfigFile dir
      return $ T.unpack <$> _cfgFiles config
    )
    (return [])

  package <- CLI.buildArtefacts (fromFP dir) globs
  callServer buildCommand (BuildTarball $ toStrict package, dropBadModules) showModuleBuild

-- | Run a command on the user client
runClient :: Transport -> CCmd () -> IO ()
runClient t c = do
  -- Haskeline has two types of interactions: file-style and terminal-style.
  -- This matters for us for two reasons:
  -- 1. On Windows, they use different APIs and different character
  --    encodings
  -- 2. On UNIX, terminal-based Haskeline API writes to the tty and makes
  --    it impossible to redirect the output to a file or pipe.
  --
  -- By default, the type of interaction is determined based on whether
  -- stdin is connected to a terminal.
  -- This is not terribly useful for us because we rarely ask for user input,
  -- but we care a lot about the output.
  -- Thus, we check whether stdout is connected to a terminal, and if so,
  -- we force the file-style interaction.
  -- Therefore, Haskeline will use the terminal-based interaction only when
  -- both stdin and stdout are terminals.
  -- Interestingly, testing on Windows showed that both file-style and
  -- terminal-style interaction works on Windows terminals; however, if
  -- I disable Haskeline altogether, I get the commitBuffer error
  -- (https://github.com/nstack/nstack-server/issues/296#issuecomment-286798496)
  -- Nevertheless, I leave this logic until we have a better understanding
  -- of what is going on. -- RC
  is_stdout_tty <- hIsTerminalDevice stdout
  let
    behavior :: HL.Behavior
    behavior =
      if is_stdout_tty
        then HL.defaultBehavior
        else HL.useFileHandle stdin
  HL.runInputTBehavior behavior HL.defaultSettings (runExceptT (runReaderT (runSettingsT c) t) >>= either (\s -> liftIO (putStrLn s >> exitFailure)) return)

callServer :: ApiCall a b -> a -> (b -> Text) -> CCmd ()
callServer fn arg formatter = do
  (Transport t) <- ask
  r <- t fn arg
  liftInput . HL.outputStr . addTrailingNewline . unpack $ formatResult formatter r
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
formatResult _ (ServerError e) = "An error was returned from the NStack Server:\nError: " <> e

serverHost :: (Monad m, MonadSettings m) => m String
serverHost = do
  s <-  (^. serverConn) <$> settings
  let (HostName domain) = fromMaybe (HostName "localhost") ((^. serverHostname) =<< s)
  return $ unpack domain

serverPath :: (Monad m, MonadSettings m) => m String
serverPath = do
  s <-  (^. serverConn) <$> settings
  domain <- serverHost
  let port' = fromMaybe httpApiPort ((^. serverPort) =<< s)
  return $ "https://" <> domain <> ":" <> show port' <> "/"

callWithHttp :: CCmdEff m => Manager -> String -> ApiCall a b -> a -> m (Result b)
callWithHttp manager hostname (ApiCall name) args = do
  auth <- (^. authSettings) <$> settings
  timeout <- (^. cliTimeout) <$> settings
  liftIO $ maybe (return err) (doCall manager path' (encode args) timeout) auth
    where path' = hostname <> unpack name
          err = ClientError "Missing or invalid credentials. Please run the 'nstack set-server' command as described in your email."

handleHttpErr :: Monad m => HttpException -> m (Result a)
handleHttpErr e = return . ClientError $ "Exception sending HTTP request: " <> showT e

doCall :: Serialize a => Manager -> String -> ByteString -> Int -> AuthSettings -> IO (Result a)
doCall manager path' body timeout auth = (do
  response <- CLI.callWithCookieJar doCall'
  let status = responseStatus response
  if (status == ok200)
     then (return . either decodeError serverResult . decode . toStrict $ responseBody response)
     else (return . ServerError $ T.unlines [showT status, (T.decodeUtf8 . toStrict) (responseBody response)])
  ) `catch` handleHttpErr
  where decodeError = ClientError . ("Cannot decode return value: " <>) . pack
        serverResult = either (ServerError . pack . displayException) Result . _serverReturn
        -- timeout * 60 * 1000 * 1000 == (timeout) minutes in microseconds
        incTimeout r = r { responseTimeout = responseTimeoutMicro (timeout * 60 * 1000 * 1000) }

        doCall' cookieJar' = do
          signedRequest <- signRequest auth . addBody =<< parseRequest path'
          (cookieRequest, _) <- insertCookiesIntoRequest signedRequest cookieJar' <$> R.date
          let versionedRequest = cookieRequest { requestHeaders = ("NSTACK_VERSION", apiHashValue) : requestHeaders cookieRequest }
          httpLbs (incTimeout versionedRequest) manager

        addBody :: Request -> Request
        addBody r = r { requestBody = RequestBodyBS body }

connectStdInOut :: ProcessId -> CCmd ()
connectStdInOut (ProcessId pid) = do
  host <- serverHost
  liftIO $ WS.runClient
    host
    8080
    ("/process/" ++ unpack pid) $ \conn -> do

    Right firstMessage <- decode <$> WS.receiveData conn
    case firstMessage :: Either String String of
      Left e -> do
        hPutStrLn stderr e
        exitFailure
      Right msg -> do
        hPutStrLn stderr msg
        runStdInOut conn

runStdInOut :: WS.Connection -> IO ()
runStdInOut conn =
  withAsync (forever $ WS.receiveData conn >>= TIO.putStrLn)   $ \asy_out ->
  withAsync (forever $ TIO.getLine >>= WS.sendBinaryData conn) $ \asy_in  -> do
    r <- waitEitherCatch asy_in asy_out
    case r of
      Left (Left e) | Just e' <- fromException e, isEOFError e' -> do
        -- EOF on stdin
        -- Wait for remaining data from the server.
        -- This does NOT guarantee that we will get all responses to
        -- our requests. The communication is asynchronous.
        WS.sendClose conn (""::Text)
        void $ waitCatch asy_out
      _ -> case either id id r of
        Right () ->
          hPutStrLn stderr $ "runStdInOut: impossible happened (loop ended)"
        Left e -> do
          hPutStrLn stderr $ displayException e
