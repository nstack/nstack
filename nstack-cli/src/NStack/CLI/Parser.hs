module NStack.CLI.Parser (
  cmds
) where
import Control.Lens ((^?))
import Control.Monad.Except (runExcept)
import Data.Monoid ((<>))
import Data.String
import Data.Text (pack)
import Options.Applicative       -- optparse-applicative
import Text.Megaparsec (try)    -- from: megaparsec

import NStack.Auth (hexUserId, textSecretKey, UserName(..), validEmail)
import NStack.CLI.Commands (Command(..), InitStack(..))
import NStack.Comms.Types
import NStack.Module.Parser (pLanguage, inlineParser, parseModuleName, parseModuleRef)
import NStack.Module.Name (ModuleName, ModuleRef)
import NStack.Module.Types (DebugOpt(..), BaseImage(..), FnName(..))
import NStack.Prelude.Monad (maybeToRight)


-- Combinators
pModuleName :: Parser ModuleName
pModuleName = argument pModuleName' (metavar "module" <> help "Module name")
  where
    pModuleName' :: ReadM ModuleName
    pModuleName' = eitherReader (runExcept . parseModuleName . pack)

pModuleRef :: Parser ModuleRef
pModuleRef = argument pModuleRef' (metavar "module" <> help "Module reference")
  where
    pModuleRef' :: ReadM ModuleRef
    pModuleRef' = eitherReader (runExcept . parseModuleRef . pack)

pFnName :: Parser FnName
pFnName = (FnName . pack) <$> strArgument (metavar "function_name" <> help "Function name")

pDSL :: Parser DSLSource
pDSL = DSLSource . pack <$> argument str (metavar "code" <> help "DSL code. If omitted, will be read from standard input.")

pStartPoint :: Parser StoppedFrom
pStartPoint = StoppedFrom <$> argument auto (metavar "from" <>
                                            help "Where to start in the stopped processes. If omitted, will default to 0.")

pAmount :: Parser StoppedAmount
pAmount = StoppedAmount <$> argument auto (metavar "n" <>
                                            help "How many stopped processses to display. If omitted, will default to 10.")

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

-- | Parser for Stopped command options
stoppedOpts :: Parser Command
stoppedOpts = ListStoppedCommand <$> optional pStartPoint <*> optional pAmount

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

-- pInitStack :: MonadError String m => Text -> m InitStack
-- pInitStack = inlineParser $ try (pW <|> pS <|> pF)
--   where
--     pW = InitWorkflow <$ string "workflow" <?> "workflow"
--     pF = InitFramework <$ string "framework" <?> "framework"
--     pS = InitStack <$> pLanguage <?> "a valid stack"

initOpts :: Parser Command
initOpts =  InitCommand
            <$> pInit
            <*> (GitRepo <$> switch (long "git-repo" <> help "Initialise with a git repository"))
  where
    pInit :: Parser InitStack
    pInit = pWorkflow <|> option pInitStack (short 'l' <> long "language" <> metavar "LANGUAGE" <> help "Initialise a module in the given language") <|> pFramework

    pInitStack :: ReadM InitStack
    pInitStack = eitherReader (runExcept . inlineParser (try $ InitStack <$> pLanguage) . pack)

    pWorkflow = flag' InitWorkflow (short 'w' <> long "workflow" <> help "Initialise a new workflow")
    pFramework = InitFramework . BaseImage . pack <$> strOption (short 'f' <> long "framework" <> metavar "MODULENAME" <> help "Initialise a module inheriting from the given parent framework (e.g. NStack.BigQuery:0.3.0)")

-- | Parser for the register command options
regOpts :: Parser Command
regOpts = RegisterCommand <$> (UserName . pack <$> argument str (metavar "username" <> help "User name to register with"))
                          <*> argument pEmail (metavar "email" <> help "Email to register with")
                          <*> serverFlag
  where
    pEmail = eitherReader (\x -> maybeToRight "Not a valid email address" (pack x ^? validEmail))
    serverFlag = option str (long "server" <> short 's' <> help "NStack Registry Server" <> showDefault <> value "demo-register.nstack.com:8443" <> metavar "SERVER")

sendOpts :: Parser Command
sendOpts = SendCommand <$> argument str (metavar "path" <> help "Path the source was created on")
                       <*> argument str (metavar "event" <> help "JSON Snippet to send as an event")

testOpts :: Parser Command
testOpts = TestCommand <$> pModuleRef
                       <*> pFnName
                       <*> argument str (metavar "event" <> help "JSON Snippet to send as an event")


listOpts :: Parser Command
listOpts = hsubparser
  (  command "modules"   (info (pure ListModulesCommand) (progDesc "List all available modules"))
  <> command "all"       (info (pure ListAllCommand) (progDesc "List all functions and types"))
  <> command "sinks"     (info (pure $ ListFnCommand SinkType) (progDesc "List only sinks"))
  <> command "sources"   (info (pure $ ListFnCommand SourceType) (progDesc "List only sources"))
  <> command "functions" (info (pure $ ListFnCommand MethodType) (progDesc "List only unconnected functions"))
  <> command "workflows" (info (pure $ ListFnCommand WorkflowType) (progDesc "List only fully-connected workflows"))
  <> command "types"     (info (pure ListTypesCommand) (progDesc "List only types"))
  ) <*> allSwitch

loginOpts :: Parser Command
loginOpts = LoginCommand <$> argument (fromString <$> str) (metavar "SERVER_HOSTNAME")
                         <*> argument auto (metavar "SERVER_PORT")
                         <*> argument userId (metavar "USERNAME")
                         <*> argument secretKey (metavar "SECRET_KEY")
            where userId    = maybeReader $ (^? hexUserId) . fromString
                  secretKey = maybeReader $ (^? textSecretKey) . fromString

-- | Parser for all subcommand options
cmds :: Parser Command
cmds =  hsubparser ( command "info" (info (InfoCommand <$> allSwitch) (progDesc "Show the server status"))
                <>  command "init" (info initOpts (progDesc "Initialise a new module/workflow"))
                <>  command "list" (info (helper <*> listOpts) (progDesc "List registered modules or functions"))
                <>  command "list-scheduled" (info (pure ListScheduled) (progDesc "List scheduled processes"))
                <>  command "build" (info (pure BuildCommand) (progDesc "Build module"))
                <>  command "delete" (info (DeleteModuleCommand <$> pModuleRef) (progDesc "Delete a module"))
                <>  command "start" (info startOpts (progDesc "Start a workflow"))
                <>  command "notebook" (info notebookOpts (progDesc "Enter some DSL interactively"))
                <>  command "stop" (info stopOpts (progDesc "Stop a process"))
                <>  command "ps" (info (pure ListProcessesCommand) (progDesc "List all running processes"))
                <>  command "stopped" (info stoppedOpts (progDesc "List all stopped processes"))
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
