module NStack.Prelude.Map where

import Data.Map (Map)
import qualified Data.Map as Map

-- Retain only elements in m whose keys are in the list ks
selectKeys :: Ord k => [k] -> Map k a -> Map k a
selectKeys ks m = Map.intersection m keep
  where keep = Map.fromList $ zip ks (repeat ())

-- Remove elements in m whose keys are in the list ks
removeKeys :: Ord k => [k] -> Map k a -> Map k a
removeKeys ks m = Map.difference m remove
  where remove = Map.fromList $ zip ks (repeat ())
