module NStack.CLI.Parser (
  cmds
) where
import Control.Lens ((^?))
import Control.Monad.Except (MonadError, runExcept)
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text, pack)
import Text.Megaparsec (try, (<?>))    -- from: megaparsec
import Text.Megaparsec.Char (string)    -- from: megaparsec

import NStack.Auth (hexUserId, textSecretKey, UserName(..), validEmail)
import NStack.CLI.Commands (Command(..), InitStack(..))
import NStack.Comms.Types
import NStack.Module.Parser (pStack, inlineParser)
import NStack.Module.Types (DebugOpt(..), BaseImage(..), ModuleName(..), FnName(..))
import NStack.Module.Parser (parseModuleName)
import NStack.Prelude.Monad (maybeToRight)

import Options.Applicative       -- optparse-applicative

-- Combinators
pModuleName :: Parser ModuleName
pModuleName = argument pModuleName' (metavar "module" <> help "Module name")
  where
    pModuleName' :: ReadM ModuleName
    pModuleName' = eitherReader (runExcept . parseModuleName . pack)

pFnName :: Parser FnName
pFnName = (FnName . pack) <$> strArgument (metavar "function_name" <> help "Function name")

pDSL :: Parser DSLSource
pDSL = DSLSource . pack <$> argument str (metavar "code" <> help "DSL code. If omitted, will be read from standard input.")

pProcessId :: Parser ProcessId
pProcessId = ProcessId . pack <$> argument str (metavar "process" <> help "Process Id")

allSwitch :: Parser Bool
allSwitch = switch (long "all" <> short 'a' <> help "Show older versions of modules")

-- | Parser for Start command options
startOpts :: Parser Command
startOpts =  StartCommand <$> debugFlag <*> pModuleName <*> pFnName

-- | Parser for Notebook command options
notebookOpts :: Parser Command
notebookOpts =  NotebookCommand <$> debugFlag <*> optional pDSL

debugFlag :: Parser DebugOpt
debugFlag = flag NoDebug Debug (long "debug" <> help "enable debug logging")

-- | Parser for Stop command options
stopOpts :: Parser Command
stopOpts =  StopCommand <$> pProcessId

-- | Parser for the log command options
logsOpts :: Parser Command
logsOpts = LogsCommand <$> pProcessId

connectOpts :: Parser Command
connectOpts = ConnectCommand <$> pProcessId

-- Parser for Init command options

pInitStack :: MonadError String m => Text -> m InitStack
pInitStack = inlineParser $ try (pW <|> pF <|> pS)
  where
    pW = InitWorkflow <$ string "workflow" <?> "workflow"
    pF = InitFramework <$ string "framework" <?> "framework"
    pS = InitStack <$> pStack <?> "a valid stack"

initOpts :: Parser Command
initOpts =  InitCommand
            <$> argument pInitStack' (metavar "stack" <> help "Module Stack")
            <*> optional (BaseImage . pack <$> argument str (metavar "base-image" <> help "Base Image to use (e.g. NStack.Python:0.24.0"))
            <*> (GitRepo <$> switch (long "git-repo" <> help "Initialise Git Repository"))
  where
    pInitStack' :: ReadM InitStack
    pInitStack' = eitherReader (runExcept . pInitStack . pack)

-- | Parser for the register command options
regOpts :: Parser Command
regOpts = RegisterCommand <$> (UserName . pack <$> argument str (metavar "username" <> help "User name to register with"))
                          <*> argument pEmail (metavar "email" <> help "Email to register with")
                          <*> serverFlag
  where
    pEmail = eitherReader $ (\x -> maybeToRight "Not a valid email address" (pack x ^? validEmail))
    serverFlag = option str (long "server" <> short 's' <> help "NStack Registry Server" <> showDefault <> value "demo-register.nstack.com:8443" <> metavar "SERVER")

sendOpts :: Parser Command
sendOpts = SendCommand <$> (argument str (metavar "path" <> help "Path the source was created on"))
                       <*> argument str (metavar "event" <> help "JSON Snippet to send as an event")

testOpts :: Parser Command
testOpts = TestCommand <$> pModuleName
                       <*> pFnName
                       <*> argument str (metavar "event" <> help "JSON Snippet to send as an event")


listOpts :: Parser Command
listOpts = hsubparser
  (  command "modules"   (info (pure ListModulesCommand) (progDesc "List all available modules"))
  <> command "all"       (info (pure $ ListCommand Nothing) (progDesc "List all functions and types"))
  <> command "sinks"     (info (pure $ ListCommand $ Just SinkType) (progDesc "List only sinks"))
  <> command "sources"   (info (pure $ ListCommand $ Just SourceType) (progDesc "List only sources"))
  <> command "functions" (info (pure $ ListCommand $ Just MethodType) (progDesc "List only unconnected functions"))
  <> command "workflows" (info (pure $ ListCommand $ Just WorkflowType) (progDesc "List only fully-connected workflows"))
  <> command "types" (info (pure $ ListCommand $ Just TypeType) (progDesc "List only types"))
  ) <*> allSwitch

loginOpts :: Parser Command
loginOpts = LoginCommand <$> argument (fromString <$> str) (metavar "SERVER_HOSTNAME")
                         <*> argument auto (metavar "SERVER_PORT")
                         <*> argument userId (metavar "USERNAME")
                         <*> argument secretKey (metavar "SECRET_KEY")
            where userId    = maybeReader $ (^? hexUserId) . fromString
                  secretKey = maybeReader $ (^? textSecretKey) . fromString

buildOpts :: Parser Command
buildOpts = BuildCommand
  <$> flag FailBadModules DropBadModules
        (long "force" <> short 'f' <> help "drop downstream modules that are broken by the new version of this module")

-- | Parser for all subcommand options
cmds :: Parser Command
cmds =  hsubparser ( command "info" (info (InfoCommand <$> allSwitch) (progDesc "Show the server status"))
                <>  command "init" (info initOpts (progDesc "Initialise a new module/workflow"))
                <>  command "list" (info (helper <*> listOpts) (progDesc "List registered modules or functions"))
                <>  command "build" (info buildOpts (progDesc "Build module"))
                <>  command "delete" (info (DeleteModuleCommand <$> pModuleName) (progDesc "Delete a module"))
                <>  command "start" (info startOpts (progDesc "Start a workflow"))
                <>  command "notebook" (info notebookOpts (progDesc "Enter some DSL interactively"))
                <>  command "stop" (info stopOpts (progDesc "Stop a process"))
                <>  command "ps" (info (pure ListProcessesCommand) (progDesc "List all running processes"))
                <>  command "logs" (info logsOpts (progDesc "Show the logs of a running process"))
                <>  command "connect" (info connectOpts (progDesc "Connect stdin/stdout to a process"))
                <>  command "server-logs" (info (pure ServerLogsCommand) (progDesc "Show the nstack server's logs"))
                <>  command "gc" (info (pure GarbageCollectCommand) (progDesc "Garbage collect images"))
                <>  command "set-server" (info loginOpts (progDesc "Set authentication config for remote server"))
                <>  command "register" (info regOpts (progDesc "Register user with the NStack Demo Server"))
                <>  command "send" (info sendOpts (progDesc "Send event to HTTP Source on NStack Server"))
                <>  command "test" (info testOpts (progDesc "Test executing function with a single given json-snippet value"))
                  )
          <|> hsubparser (command "log" (info logsOpts (progDesc "Show the logs of a running process")) <> internal)
