{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module NStack.CLI.Types (
  CCmd, liftInput, CCmdEff, Transport(..), Result(..)
) where

import Control.Monad.Classes (MonadReader)          -- from: monad-classes
import Control.Monad.Except (ExceptT, MonadError)   -- mtl
import Control.Monad.Trans (MonadIO, lift)          -- mtl
import Control.Monad.Reader (ReaderT)               -- mtl
import Data.Text (Text)

import NStack.Comms.Types (ApiCall)
import System.Console.Haskeline
import NStack.Settings (SettingsT, MonadSettings)

data Result a = Result a
              | ServerError Text
              | ClientError Text

type WithSettingsT n = SettingsT n n

-- | Run CLI commands with basic error handling
type CCmd = WithSettingsT (ReaderT Transport (ExceptT String (InputT IO)))

liftInput :: InputT IO a -> CCmd a
liftInput = lift . lift . lift

-- | Remote Server RPC Transport
data Transport = Transport (forall a b m. (CCmdEff m) => ApiCall a b -> a -> m (Result b))

-- | CLI Monad
type CCmdEff m = (MonadReader Transport m, MonadError String m, MonadIO m, MonadSettings m)
