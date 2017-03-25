{-# OPTIONS_GHC -fno-warn-orphans #-}
module NStack.Settings.Internal.Orphans where
import Control.Applicative
import Data.Aeson -- from: aeson
import Data.UUID  -- from: uuid

instance FromJSON UUID where
  parseJSON (String a) = maybe empty pure $ fromText a
  parseJSON _          = empty

instance ToJSON UUID where
  toJSON = String . toText
