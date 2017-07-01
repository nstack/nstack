{-# LANGUAGE AllowAmbiguousTypes #-}
module NStack.Prelude.Stateless
  ( Stateless
  , assertStateless
  , StatelessT
  , liftStatelessT
  ) where

import Data.Constraint
import Data.Constraint.Forall
import Control.Monad.Trans.Control

----------------------------------------------------------------------
--                             Stateless monads
----------------------------------------------------------------------

class    StM m a ~ a => StatelessAt m a
instance StM m a ~ a => StatelessAt m a

-- | A constraint that asserts that a given monad is stateless.
--
-- See <https://ro-che.info/articles/2014-12-26-monad-control-type-synonyms>
-- for the explanation.
type Stateless m = Forall (StatelessAt m)

-- | Instantiate the stateless claim at a particular monad and type.
--
-- The @m@ can be inferred from the use site,
-- so you only need to apply this to the type @a@,
-- e.g. @assertStateless 'Int'@.
assertStateless :: forall a m b . Stateless m => (StM m a ~ a => m b) -> m b
assertStateless action = action \\ (inst :: Stateless m :- StatelessAt m a)

----------------------------------------------------------------------
--                         Stateless transformers
----------------------------------------------------------------------

class    StM (t m) a ~ StM m a => StatelessTAt t (m :: * -> *) a
instance StM (t m) a ~ StM m a => StatelessTAt t m a

-- | A statement that a monad trasnformer doesn't alter the state type
type StatelessT t = ForallV (StatelessTAt t)

-- | A proof that if @t@ is a stateless transformer and @m@ is a stateless monad,
-- then @t m@ is a stateless monad.
statelessT :: forall t m . (StatelessT t, Stateless m) :- Stateless (t m)
statelessT = Sub $ forall prf
  where
    prf :: forall a . (StatelessT t, Stateless m) => Dict (StatelessAt (t m) a)
    prf = Dict \\ (instV :: StatelessT t :- StatelessTAt t m a)
               \\ (inst  :: Stateless m  :- StatelessAt m a)

-- | Derive the 'Stateless' constraint for a transformed monad @t m@
liftStatelessT
  :: forall t m b . (StatelessT t, Stateless m)
  => (Stateless (t m) => (t m) b) -> (t m) b
liftStatelessT action = action \\ statelessT @t @m
