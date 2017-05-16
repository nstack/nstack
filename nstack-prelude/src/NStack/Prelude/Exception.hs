module NStack.Prelude.Exception
  ( TransientError(..)
  , PermanentError(..)
  )
  where

import Control.Exception
import Data.Typeable (Typeable)

data TransientError = TransientError String
  deriving (Show, Typeable)

instance Exception TransientError where
  displayException (TransientError msg) = msg

data PermanentError = PermanentError String
  deriving (Show, Typeable)

instance Exception PermanentError where
  displayException (PermanentError msg) = msg
