module NStack.Settings.Internal.Lens (AsYAML(..), Yaml(..), yaml) where
import Control.Lens                 -- from: lens
import Data.Aeson                   -- from: aeson
import Data.Aeson.Lens              -- from: lens-aeson
import Data.ByteString (ByteString) -- from: bytestring
import Data.Coerce
import Data.String (IsString)
import qualified Data.Yaml as Yaml  -- from: yaml

class AsYAML t where
  _YAML :: (FromJSON a, ToJSON a) => Prism' t a

instance AsYAML ByteString where
  _YAML = prism' Yaml.encode Yaml.decode

newtype Yaml a = Yaml a
  deriving (Eq, Ord, Show, Monoid, IsString)

instance AsYAML a => AsYAML (Yaml a) where
  _YAML = from yaml . _YAML

instance AsYAML a => AsNumber (Yaml a)
instance AsYAML a => AsPrimitive (Yaml a)
instance AsYAML a => AsValue (Yaml a) where
  _Value = yaml._YAML

yaml :: Iso a b (Yaml a) (Yaml b)
yaml = dimap Yaml (fmap coerce)
