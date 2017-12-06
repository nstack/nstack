{-# LANGUAGE TemplateHaskell #-}

module NStack.Module.Name where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Coerce (coerce)
import Data.Data (Data(..))
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getListOf)
import Data.Serialize.Put (putListOf)
import Data.Text (Text, unpack, intercalate)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.PrettyPrint.Mainland (Pretty, ppr, text)

import NStack.Auth (UserName(..), nstackUserName)
import NStack.Module.Version
import NStack.Prelude.Text (putText, getText, pprS)

{-
   A ModuleRef refers to a single, exact, immutable module
   A ModuleName is a name that can be given to a ModuleRef, which might include a generic snapshot which will resolve to a specific hash at compile time
   A ModuleQuery is a query that will resolve to a given ModuleRef, e.g. 'LATEST'
   -}
type ModuleRef = ModuleURI ExactVersion
type ModuleName = ModuleURI FuzzyVersion
type ModuleQuery = ModuleURI VersionQuery

-- | A `ModuleURI` is a full module identifier parameterized over a version type `v`
--   Different parameterizations allow us to express different versioning semantics
data ModuleURI v = ModuleURI {
  registry :: NSUri,
  author :: UserName,
  name :: NSUri,
  version :: v
} deriving (Functor, Foldable, Traversable, Generic, Eq, Ord, Data)

instance Show v => Show (ModuleURI v) where
  show (ModuleURI r a n v) = show r <> "/" <> unpack (_username a) <> "/" <> show n <> ":" <> show v

-- | A URI for an NStack resource
newtype NSUri = NSUri { _nsUri :: [Text] }
  deriving (Eq, Ord, Typeable, Data, Generic)

instance ToJSON NSUri
instance FromJSON NSUri

instance Show NSUri where
  show = unpack . intercalate "." . _nsUri

instance Serialize NSUri where
  put = coerce (putListOf putText)
  get = coerce (getListOf getText)

$(deriveSafeCopy 0 'base ''NSUri)

showShortModuleUri :: ShowShort v => ModuleURI v -> String
showShortModuleUri (ModuleURI r a n v) = reg <> unpack (_username a) <> "/" <> show n <> ":" <> showShort v
  where reg = if r == nStackRegistry then "" else show r <> "/"

nStackRegistry :: NSUri
nStackRegistry = NSUri ["registry", "nstack", "com"]

instantiateModuleRef :: MonadIO m => ModuleName -> m ModuleRef
instantiateModuleRef = traverse instantiateFuzzy'

isRelease :: ModuleRef -> Bool
isRelease = (== Release) . release . version

nameEq :: ModuleURI a -> ModuleURI b -> Bool
nameEq (ModuleURI ra aa na _) (ModuleURI rb ab nb _) = (ra,aa,na) == (rb,ab,nb)

instance Pretty r => Pretty (ModuleURI r) where
  ppr (ModuleURI r a n v) = text $ reg <> unpack (_username a) <> "/" <> show n <> ":" <> pprS v
    where reg = if r == nStackRegistry then "" else show r <> "/"

-- | Helper function to create a ModuleName using the default registry/author
mkNStackModuleRef :: NSUri -> ExactVersion -> ModuleRef
mkNStackModuleRef = ModuleURI nStackRegistry nstackUserName

instance Serialize v => Serialize (ModuleURI v)
$(deriveSafeCopy 0 'base ''ModuleURI)

refToName :: ModuleRef -> ModuleName
refToName = fmap $ fmap exactToFuzzy
