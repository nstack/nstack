module NStack.Prelude.Shell (
  -- * Running commands
  runCmd , runCmd_, runCmdOut
) where

import Control.Monad.Trans (MonadIO)
import Data.Text (Text)
import qualified Turtle as R

-- | Run commands as blocking subprocesses
runCmd :: (MonadIO io) => Text -> [Text] -> io R.ExitCode
runCmd cmd args = R.proc cmd args R.empty

-- | Run commands as blocking subprocesses
runCmd_ :: (MonadIO io) => Text -> [Text] -> io ()
runCmd_ cmd args = R.procs cmd args R.empty

-- | Run commands as blocking subprocesses
runCmdOut :: (MonadIO io) => Text -> [Text] -> io Text
runCmdOut cmd args = snd <$> R.procStrict cmd args R.empty
