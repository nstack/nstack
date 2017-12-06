{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module exists to export Aeson instances for NStack.Module.Types definitions
-- where they would normally cause a cycle between that module and NStack.Module.Parser
module NStack.Module.Types.Aeson where
import Data.Aeson
import Text.PrettyPrint.Mainland (Pretty)

import NStack.Module.Parser
import NStack.Module.Name (ModuleName, ModuleRef, ModuleURI(..))
import NStack.Module.Types
import NStack.Prelude.Text (pprS)

instance Pretty r => ToJSON (ModuleURI r) where
  toJSON = toJSON . pprS

instance FromJSON ModuleName where
  parseJSON = withText "ModuleName" $ either error return . parseModuleName

instance FromJSON ModuleRef where
  parseJSON = withText "ModuleRef" $ either error return . parseModuleRef

instance ToJSONKey ModuleName
instance FromJSONKey ModuleName
instance ToJSONKey ModuleRef
instance FromJSONKey ModuleRef

instance ToJSON a => ToJSON (Qualified a) where
  toJSON (Qualified m n) = toJSON (m, n)

instance FromJSON a => FromJSON (Qualified a) where
  parseJSON a = uncurry Qualified <$> parseJSON a

instance ToJSON a => ToJSONKey (Qualified a)
instance FromJSON a => FromJSONKey (Qualified a)
