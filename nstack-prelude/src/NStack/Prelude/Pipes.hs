{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module defines orphan instances needed to integrate pipes and
-- resourcet.
module NStack.Prelude.Pipes where

import Control.Monad.Trans.Resource (MonadResource(..))
import Pipes.Internal (Proxy(..))
import Pipes.Safe () -- contains orphan instances for MonadThrow etc.
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (lift)
import Control.Monad.Base (MonadBase(..))

instance (MonadResource m, MonadThrow m, MonadIO m, MonadBase IO m) => MonadResource (Proxy a' a b' b m) where
  liftResourceT = lift . liftResourceT

instance MonadBase base m => MonadBase base (Proxy a' a b' b m) where
  liftBase = lift . liftBase
