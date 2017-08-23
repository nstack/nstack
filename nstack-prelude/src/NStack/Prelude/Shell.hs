module NStack.Prelude.Shell (
  -- * Running commands
  runCmd , runCmd_, runCmdOut
) where

import Control.Monad.Trans (MonadIO)
import Data.Text (Text)
import qualified Turtle as R

-- | Run as blocking subprocesses, printing stdout and returning exit code
runCmd :: (MonadIO io) => Text -> [Text] -> io R.ExitCode
runCmd cmd args = R.proc cmd args R.empty

-- | Run as blocking subprocesses, printing stdout and throwing exception on non-zero exit
runCmd_ :: (MonadIO io) => Text -> [Text] -> io ()
runCmd_ cmd args = R.procs cmd args R.empty

-- | Run as blocking subprocesses, returning stdout and throwing exception on non-zero exit
runCmdOut :: (MonadIO io) => Text -> [Text] -> io Text
runCmdOut cmd args = snd <$> R.procStrict cmd args R.empty
