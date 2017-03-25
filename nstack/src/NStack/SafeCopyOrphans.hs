{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module NStack.SafeCopyOrphans where

import Data.Coerce (coerce)
import Data.Serialize (Serialize(..))
import Data.SafeCopy (base, deriveSafeCopy, SafeCopy(..), contain, safePut, safeGet)
import qualified Network.URI as URI
import Data.UUID

import NStack.Prelude.Text (putString, getString)
import NStack.UUIDOrphans ()

-- other TH-based SafeCopy derivations
-- TODO - move SafeCopy and Serializable instances from NStack.Module.Types/NStack.Lang.Types here?
instance Serialize URI.URI
instance Serialize URI.URIAuth where
  put (URI.URIAuth x y z) = putString x >> putString y >> putString z
  get = URI.URIAuth <$> getString <*> getString <*> getString

instance SafeCopy UUID where
  putCopy = contain . coerce ((\(a, b, c, d) -> safePut a >> safePut b >> safePut c >> safePut d) . toWords)
  getCopy = contain . coerce $ fromWords <$> safeGet <*> safeGet <*> safeGet <*> safeGet

$(deriveSafeCopy 0 'base ''URI.URI)
$(deriveSafeCopy 0 'base ''URI.URIAuth)
