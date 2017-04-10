{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module NStack.Module.Types where
import Control.Lens (lens, Lens', iso, Iso')   -- from: lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Coerce (coerce)
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(..))
import Data.Semigroup
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getListOf)
import Data.Serialize.Put (putListOf)
import Data.String (IsString)
import Data.Text (Text)                        -- from: text
import qualified Data.Text as T                -- from: text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Text.PrettyPrint.Mainland (Pretty, ppr, text, commasep)   -- from: mainland-pretty
import Text.Printf (printf)
import qualified Turtle as R
import Turtle ((%))

import NStack.SafeCopyOrphans ()
import NStack.UUIDOrphans ()
import NStack.Prelude.Text (showT, putText, getText, pprT)


data Stack = Python | NodeJS
  -- Custom Text  -- TODO - add a Custom Stack that reads config from a YAML file at build-time
  deriving (Show, Eq, Generic)

instance Serialize Stack

$(deriveSafeCopy 0 'base ''Stack)

instance Pretty Stack where
  ppr = text . show

-- System stacks
stacks :: [Stack]
stacks = [Python, NodeJS]

newtype NSUri = NSUri { _nsUri :: [Text] }
  deriving (Eq, Ord, Typeable)

instance Show NSUri where
  show = T.unpack . T.intercalate "." . _nsUri

instance Serialize NSUri where
  put = coerce (putListOf putText)
  get = coerce (getListOf getText)

$(deriveSafeCopy 0 'base ''NSUri)

newtype Author = Author { _author :: Text }
  deriving (Eq, Ord)

instance Show Author where
  show = coerce (show :: Text -> String)

instance Serialize Author where
  put = coerce putText
  get = coerce getText

data Version_v0 = Version_v0 Integer Integer Integer
data Version_v1 = Version_v1 Integer Integer Integer Bool

-- The order of constructors is significant for the Ord instance
data Release = Snapshot | Release
  deriving (Eq, Ord, Show, Generic)

-- TODO add version helper funcs that support semver?
data Version = Version {
  _majorVer :: Integer,
  _minorVer :: Integer,
  _patchVer :: Integer,
  _release  :: Release
} deriving (Eq, Ord, Generic)

instance Migrate Version_v1 where
  type MigrateFrom Version_v1 = Version_v0
  migrate (Version_v0 a b c) = Version_v1 a b c True
instance Migrate Version where
  type MigrateFrom Version = Version_v1
  migrate (Version_v1 a b c isRelease) = Version a b c $
    if isRelease then Release else Snapshot

instance Show Version where
  show (Version ma mi p r) = T.unpack $
    R.format (R.d%"."%R.d%"."%R.d) ma mi p <>
      case r of
        Release -> ""
        Snapshot -> "-SNAPSHOT"

initVersion :: Version
initVersion = Version 0 0 1 Snapshot

-- | Name for a unique module that exists on the server
data ModuleName = ModuleName {
  _mRegistry :: NSUri,
  _mAuthor :: Author,
  _mName :: NSUri,
  _mVersion :: Version
} deriving (Show, Eq, Ord, Generic)

instance Pretty ModuleName where
  ppr = text . showShortModuleName

sameNSName :: ModuleName -> ModuleName -> Bool
sameNSName (ModuleName r a n _) (ModuleName r' a' n' _) = r == r' && a == a' && n == n'

ordByVersion :: ModuleName -> ModuleName -> Ordering
ordByVersion m@(ModuleName _ _ _ v) m'@(ModuleName _ _ _ v') = if sameNSName m m' then compare v v' else EQ

nStackRegistry :: NSUri
nStackRegistry = NSUri ["registry", "nstack", "com"]

-- TODO - unify with `username` in Auth
nStackAuthor :: Author
nStackAuthor = Author "nstack"

mkNStackModuleName :: NSUri -> Version -> ModuleName
mkNStackModuleName = ModuleName nStackRegistry nStackAuthor

-- | display the module name, hiding registry and author if they are the default
showShortModuleName :: ModuleName -> String
showShortModuleName ModuleName{..} = T.unpack $ reg <> aut <> showT _mName <> ":" <> showT _mVersion
  where
    reg = if _mRegistry == nStackRegistry then "" else showT _mRegistry <> "/"
    -- changing to always show author for now, rather than hiding if aut == nStackAuthor
    aut = _author _mAuthor <> "/"


instance Serialize Release
instance Serialize Version
instance Serialize ModuleName

$(deriveSafeCopy 0 'base ''Author)
$(deriveSafeCopy 0 'base ''Release)
$(deriveSafeCopy 0 'base ''Version_v0)
$(deriveSafeCopy 1 'extension ''Version_v1)
$(deriveSafeCopy 2 'extension ''Version)
$(deriveSafeCopy 0 'base ''ModuleName)

-- | A method-name stored within a module
newtype MethodName = MethodName { _methodName :: Text }
  deriving (Eq, Ord, Typeable, IsString, Pretty)

instance Show MethodName where
  show = coerce (show :: Text -> String)

instance Serialize MethodName where
  put = coerce putText
  get = coerce getText

-- | A fully-qualified methodname, including both the full ModuleName Path and the MethodName itself
-- We create this during parsing by mapping from the short qualified modulename to the full ModuleName
data MethodURI = MethodURI { _modName :: ModuleName, _methName :: MethodName }
  deriving (Eq, Ord, Typeable, Generic)

instance Show MethodURI where
  show (MethodURI modName methName) = T.unpack $ pprT modName <> "." <> _methodName methName

instance Pretty MethodURI where
  ppr = text . show

instance Serialize MethodURI





newtype BaseImage = BaseImage { _baseImage :: T.Text }
  deriving (Eq)

instance Show BaseImage where
  show = coerce (show :: T.Text -> String)

newtype SHA512 = SHA512 { _sha512 :: ByteString } deriving (Eq, Generic)

-- | Takes a textual version of a hash, i.e. that generated by sha512sum, and encode it
mkSHA512 :: Text -> Maybe SHA512
mkSHA512 x = if T.length x == 128 && BS.length hash == 64 && BS.length hashRem == 0 then Just (SHA512 hash) else Nothing
  where (hash, hashRem) = Base16.decode . encodeUtf8 $ x

instance Show SHA512 where
  show = T.unpack . decodeUtf8 . Base16.encode . _sha512

-- data LayerType = BtrfsXz deriving (Eq, Show, Read, Generic)

-- | A image is simply a single btrfs layer, that is applied to other parent module images
-- within the module
data Image = Image {
  -- _layerType :: LayerType, -- mediatype, should be 'btrfs'
  _sha :: SHA512, -- the SHA of the image layer for distribution
  _iUuid :: UUID, -- btrfs uuid
  _size :: Integer -- size in bytes of the compressed form of the layer
} deriving (Show, Eq, Generic)

instance Pretty Image where
  ppr Image{..} = commasep [hashFrag, sizeMb]
    where
      hashFrag = text . take 8 . show $ _sha
      sizeMb = text $ printf "%.1f MB" (fromInteger _size / 1000000 :: Double)

$(deriveSafeCopy 0 'base ''SHA512)
-- $(deriveSafeCopy 0 'base ''LayerType)
$(deriveSafeCopy 0 'base ''Image)

-- instance Serialize LayerType
instance Serialize SHA512
instance Serialize Image

newtype BusName = BusName { _unBusName :: Text } deriving (Eq, Show)
data DebugOpt = NoDebug | Debug deriving (Eq, Ord, Show, Generic)
data ServiceOptions
 = ServiceOptions { _busName :: BusName, _debugOpt :: DebugOpt }
 deriving (Eq, Show)

-- monoid instance for DebugOpt is OR
instance Monoid DebugOpt where
  mempty = NoDebug
  NoDebug `mappend` a = a
  Debug   `mappend` _ = Debug

instance Serialize DebugOpt

class HasServiceOptions a where
  serviceOptions :: Lens' a ServiceOptions

class HasDebugOpt a where
  debugOpt :: Lens' a DebugOpt

unBusName :: Iso' BusName Text
unBusName = iso _unBusName BusName

busName :: Lens' ServiceOptions BusName
busName = lens _busName $ \o n -> o { _busName = n }

instance HasDebugOpt ServiceOptions  where
  debugOpt = lens _debugOpt $ \o d -> o { _debugOpt = d }

-- | Given all mandatory options returns an
-- | ServiceOptions with default values set
-- | for the rest.
-- | Currently only busName is mandatory, and only
-- | optional is NoDebug
defaultServiceOptions :: BusName -> ServiceOptions
defaultServiceOptions bn = ServiceOptions bn mempty
