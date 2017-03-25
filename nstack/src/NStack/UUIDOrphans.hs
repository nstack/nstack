{-# OPTIONS_GHC -fno-warn-orphans #-}

module NStack.UUIDOrphans where

import qualified Data.Binary as B
import Data.UUID
import Data.Serialize
import Text.PrettyPrint.Mainland

instance Pretty UUID where
  ppr = text . show

instance Serialize UUID where
  put = put . B.encode
  get = B.decode <$> get

