{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module NStack.Settings.Class where
import Control.Monad.Classes     -- from: monad-classes
import Control.Monad.Trans       -- from: mtl
import GHC.Prim (Proxy#, proxy#) -- from: ghc-prim

import NStack.Settings.Types (SettingsT, Settings, SettingState)
import qualified NStack.Settings.Types as S

class Monad m => MonadSettingsN (n :: Nat) m where
  settingsN :: Proxy# n -> m Settings
  modifySettingsN :: Proxy# n -> (Settings -> Settings) -> m ()

type EffSettings n = EffReader (SettingState n)

instance Monad m => MonadSettingsN 'Zero (SettingsT m m) where
  settingsN _ = S.settings
  modifySettingsN _ = S.modifySettings

instance (MonadTrans t, Monad (t m), MonadSettingsN n m) => MonadSettingsN ('Suc n) (t m) where
  settingsN _ = lift $ settingsN (proxy# :: Proxy# n)
  modifySettingsN _ = lift . modifySettingsN (proxy# :: Proxy# n)

type MonadSettings m = MonadSettingsN (Find (EffSettings (InnerEff m)) m) m

type family InnerEff m where
  InnerEff (SettingsT m m) = m
  InnerEff (trans       m) = InnerEff m

settings :: forall m. MonadSettings m => m Settings
settings = settingsN (proxy# :: Proxy# (Find (EffSettings (InnerEff m)) m))

modifySettings :: forall m. MonadSettings m => (Settings -> Settings) -> m ()
modifySettings = modifySettingsN (proxy# :: Proxy# (Find (EffSettings (InnerEff m)) m))
