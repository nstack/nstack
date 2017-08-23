{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module exists to export Aeson instances for NStack.Module.Types definitions
-- where they would normally cause a cycle between that module and NStack.Module.Parser
module NStack.Module.Types.Aeson where
import Data.Aeson

import NStack.Module.Parser
import NStack.Module.Types
import NStack.Prelude.Text (pprS)

instance ToJSON ModuleName where
  toJSON = toJSON . pprS

instance FromJSON ModuleName where
  parseJSON = withText "ModuleName" $ either error return . parseModuleName

instance ToJSONKey ModuleName
instance FromJSONKey ModuleName

instance ToJSON a => ToJSON (Qualified a) where
  toJSON (Qualified m n) = toJSON (m, n)

instance FromJSON a => FromJSON (Qualified a) where
  parseJSON a = uncurry Qualified <$> parseJSON a

instance ToJSON a => ToJSONKey (Qualified a)
instance FromJSON a => FromJSONKey (Qualified a)
