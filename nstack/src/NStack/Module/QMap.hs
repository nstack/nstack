{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NStack.Module.QMap where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Serialize (Serialize)

import GHC.Generics (Generic)

import NStack.Module.Name (ModuleRef)
import NStack.Module.Types (Qualified(..))
import NStack.Module.Types.Aeson ()

{-
   Qualified Map datatype - store values under Qualified Keys, with a simplified interface
   for querying and working with them.
-}

newtype QMap k v = QMap {
  inner :: Map ModuleRef (Map k v)
} deriving (Show, Functor, Generic, ToJSON, FromJSON, Eq, Ord)

instance (Serialize k, Serialize v, Ord k) => Serialize (QMap k v)

-- QMap monoid is left-biased like Map
-- This could probably be more performant if we took into account
-- the actual internal structure of the map
instance Ord k => Monoid (QMap k v) where
  (QMap inner) `mappend` b =
    foldl (flip $ uncurry insert) b elements
      where elements = do (mod', map') <- Map.toList inner
                          (k, v) <- Map.toList map'
                          return (Qualified mod' k, v)
  mempty = QMap mempty

lookup :: Ord k => Qualified k -> QMap k v -> Maybe v
lookup (Qualified mod' k) (QMap inner) = Map.lookup mod' inner >>= Map.lookup k

insert :: Ord k => Qualified k -> v -> QMap k v -> QMap k v
insert (Qualified mod' k) v (QMap inner) = QMap $ Map.insertWith f mod' (Map.singleton k v) inner
  where f new old = new <> old

-- Overrides previous members
overrideModule :: Ord k => ModuleRef -> Map k v -> QMap k v -> QMap k v
overrideModule mod' members (QMap inner) = QMap $ Map.insert mod' members inner

flatten :: Ord k => QMap k v -> Map (Qualified k) v
flatten (QMap inner) = Map.foldMapWithKey (\mod' m -> Map.mapKeys (Qualified mod') m) inner

mapKeys :: Ord k2 => (k1 -> k2) -> QMap k1 v -> QMap k2 v
mapKeys f (QMap inner) = QMap $ Map.mapKeys f <$> inner

filter :: (v -> Bool) -> QMap k v -> QMap k v
filter p (QMap inner) = QMap $ Map.filter p <$> inner
