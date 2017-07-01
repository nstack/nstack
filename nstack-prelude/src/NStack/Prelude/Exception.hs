module NStack.Prelude.Exception
  ( TransientError(..)
  , PermanentError(..)
  , throwPermanentError
  )
  where

import Control.Exception
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))

data TransientError = TransientError String
  deriving (Show, Typeable)

instance Exception TransientError where
  displayException (TransientError msg) = msg

data PermanentError = PermanentError String
  deriving (Show, Typeable)

instance Exception PermanentError where
  displayException (PermanentError msg) = msg

-- | A shortcut for commonly-occurring @liftIO . throwIO . PermanentError@.
throwPermanentError :: MonadIO m => String -> m a
throwPermanentError = liftIO . throwIO . PermanentError
