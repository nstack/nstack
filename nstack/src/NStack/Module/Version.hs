{-# LANGUAGE TemplateHaskell #-}

module NStack.Module.Version where

import Control.Lens ((^?))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), ToJSONKey, FromJSONKey)
import Data.Bifunctor (first)
import Data.Data (Data(..))
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Serialize (Serialize(..))
import Data.Text (Text)
import Data.Thyme (getCurrentTime)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric (showHex)
import Numeric.Lens (hex)
import Text.Megaparsec (parse, some, ParsecT)
import Text.Megaparsec.Char (hexDigitChar)
import Text.PrettyPrint.Mainland (Pretty, ppr, text)

import NStack.Prelude.Monad (orError, eitherToExcept)
import NStack.Prelude.Time (timeToUnix)

import NStack.SafeCopyOrphans ()
import NStack.UUIDOrphans ()

-- | Semantic Version with a release tag of type `r`
data SemVer r = SemVer {
  major :: Integer,
  minor :: Integer,
  patch :: Integer,
  release :: r
} deriving (Eq, Ord, Generic, Data, Functor, Foldable, Traversable)

-- Exact versions, e.g.
-- import Foo:1.0.2                        --> A specific release reference
-- import Foo:1.0.2-SNAPSHOT-3457230459837 --> A specific resolved snapshot reference

type FuzzyVersion = SemVer FuzzyRelease

-- Fuzzy versions, e.g.
-- import Foo:LATEST            --> resolve to latest version (release or snapshot?)
-- import Foo:0.0.1-SNAPSHOT    --> resolve to latest actual snapshot for 0.0.1
-- import Foo:1.0.2             --> Exact references are a subset of fuzzy
-- import Foo:1.0.2-SNAPSHOT-3457230459837 --> Exact references are a subset of fuzzy

type ExactVersion = SemVer ExactRelease

data FuzzyRelease = FSnap SnapshotHash
                  | Snapshot
                  | FRelease
                  deriving (Eq, Ord, Generic, Data)

data ExactRelease = Snap SnapshotHash
                  | Release
                  deriving (Eq, Ord, Generic, Data)

-- Version Queries, e.g.
-- start Foo:LATEST                       --> resolve to latest version (release or snapshot?)
-- start Foo:0.0.1-SNAPSHOT               --> resolve to latest actual snapshot for 0.0.1
-- start Foo:1.0.2                        --> Exact references are a subset of queries
-- start Foo:1.0.2-SNAPSHOT-3457230459837 --> Exact references are a subset of queries

data VersionQuery = Latest                    -- Foo:LATEST
                  | Matching FuzzyVersion

-- All snapshots are stored as immutable against a specific version hash
data SnapshotHash = SnapshotHash Int64
  deriving (Eq, Ord, Generic, Data)

mkSnapshotHash :: MonadIO m => m SnapshotHash
mkSnapshotHash = liftIO $ do
  time <- read . timeToUnix <$> getCurrentTime
  return $ SnapshotHash time

instance Serialize SnapshotHash

instance Show SnapshotHash where
  show (SnapshotHash time) = showHex time ""

instance Show ExactRelease where
  show (Snap hash) = "-SNAPSHOT-" <> show hash
  show Release     = ""

instance Show FuzzyRelease where
  show (FSnap hash) = "-SNAPSHOT-" <> show hash
  show Snapshot     = "-SNAPSHOT"
  show FRelease     = ""

instance Pretty ExactRelease where
  ppr = text . show

-- When pretty-printing a fuzzy release, hide the snapshot hash
instance Pretty FuzzyRelease where
  ppr = text . show

-- Typeclass for diplaying a short (abbreviated but human readable) version
class ShowShort a where
  showShort :: a -> String

instance ShowShort FuzzyRelease where
  showShort (FSnap _) = "-SNAPSHOT"
  showShort Snapshot  = "-SNAPSHOT"
  showShort FRelease  = ""

instance ShowShort ExactRelease where
  showShort (Snap _) = "-SNAPSHOT"
  showShort Release  = ""

instance ShowShort r => ShowShort (SemVer r) where
  showShort (SemVer ma mi p r) = show ma <> "." <> show mi <> "." <> show p <> showShort r

instance Show r => Show (SemVer r) where
  show (SemVer ma mi p r) = show ma <> "." <> show mi <> "." <> show p <> show r

instance Show r => Pretty (SemVer r) where
  ppr = text . show

parseSnapshotHash :: MonadError String m => Text -> m SnapshotHash
parseSnapshotHash str = do
  ts :: String <- eitherToExcept $ parse' pTimestamp str
  timestamp <- (ts ^? hex) `orError` "Timestamp is not a valid hex string" -- This should never happen as we have safely parsed
  return $ SnapshotHash timestamp
    where pTimestamp = some hexDigitChar
          parse' :: ParsecT Void Text Identity a -> Text -> Either String a
          parse' p = first show . parse p "<no file>"

-- Instantiate snapshots with unique hashes; preserve exact versions
instantiateFuzzy' :: MonadIO m => FuzzyVersion -> m ExactVersion
instantiateFuzzy' = traverse instantiate
  where instantiate (FSnap hash) = return $ Snap hash
        instantiate FRelease     = return Release
        instantiate Snapshot     = Snap <$> mkSnapshotHash

exactToFuzzy :: ExactRelease -> FuzzyRelease
exactToFuzzy (Snap h) = (FSnap h)
exactToFuzzy Release = FRelease

instance Serialize r => Serialize (SemVer r)
instance Serialize ExactRelease
instance Serialize FuzzyRelease

$(deriveSafeCopy 0 'base ''SemVer)
$(deriveSafeCopy 0 'base ''ExactRelease)
$(deriveSafeCopy 0 'base ''FuzzyRelease)
$(deriveSafeCopy 0 'base ''SnapshotHash)

instance ToJSON r => ToJSON (SemVer r)
instance FromJSON r => FromJSON (SemVer r)
instance ToJSON r => ToJSONKey (SemVer r)
instance FromJSON r => FromJSONKey (SemVer r)
instance ToJSON SnapshotHash
instance FromJSON SnapshotHash
instance ToJSON ExactRelease
instance FromJSON ExactRelease
instance ToJSONKey ExactRelease
instance FromJSONKey ExactRelease
instance ToJSON FuzzyRelease
instance FromJSON FuzzyRelease
