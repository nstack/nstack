module NStack.Prelude.Format
  ( module NStack.Prelude.Format
  , module Control.Category.Printf
  ) where

import Prelude hiding (id, (.), FilePath)

import Control.Category.Printf
import Control.Comonad
import Control.Lens (Getter, view, to)
import Data.Coerce
import Data.Text
import Filesystem.Path.CurrentOS (FilePath, toText)

-- | Concatenate two 'Format' strings.
--
-- For when '.' is the one from 'Prelude'
(%) :: Monoid m => Format m b c -> Format m a b -> Format m a c
(%) = (.)

-- formatters for using lenses

lapp :: Monoid m => Getter s a -> Format m (a -> r) (s -> r)
lapp f = apply (view f)

lspliceA :: (Applicative f, Monoid (f a)) => Getter s a -> Format (f a) r (s -> r)
lspliceA f = spliceWith (view $ f . to pure)

lsplice :: Monoid a => Getter s a -> Format a r (s -> r)
lsplice f = spliceWith (view f)

fp :: Format Text a (FilePath -> a)
fp = spliceWith $ either id id . toText

c' :: (Coercible a m, Monoid m) => Format m r (a -> r)
c' = spliceWith coerce

copy :: Format m a (t -> a) -> Format m (t -> a) (t -> a)
copy (Cokleisli f) = Cokleisli $ \g t -> f (`g` t) t
