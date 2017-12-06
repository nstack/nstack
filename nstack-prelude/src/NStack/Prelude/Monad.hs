module NStack.Prelude.Monad (
  -- * Conversions between monads
  castError
, eitherToExcept
, maybeToExcept
, maybeToRight
, orError
, orElse
, orException
, try'
, (>>->)
, withReaderT
) where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)

eitherToExcept :: MonadError e m => Either e a -> m a
eitherToExcept = either throwError return

maybeToExcept :: MonadError e m => e -> Maybe a -> m a
maybeToExcept l = maybe (throwError l) return

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight l a = case a of Nothing -> Left l
                             Just x -> Right x

orElse :: Maybe a -> a -> a
orElse ma default' = maybe default' id ma

-- | synonym of maybeToExcept that reads better when infix
orError :: MonadError e m => Maybe a -> e -> m a
orError = flip maybeToExcept

-- | Like 'orError', but throw an exception
orException :: (MonadIO m, Exception e) => Maybe a -> e -> m a
orException mb exn = maybe (liftIO $ throwIO exn) return mb

castError :: MonadError e m => Either a b -> (a -> e) -> m b
castError e f = either (throwError . f) return $ e

-- | Catch any sync IO exception within MonadError
try' :: (MonadError String m, MonadIO m) => IO a -> m a
try' a = (liftIO
       . liftM (first show)
       . (try :: IO a -> IO (Either SomeException a)) $ a)
       >>= eitherToExcept


-- | Composition of unwrapped reader transformers
-- | Kleisli composition that ignores the result of the
-- | left kleisli arrow and forwards the input to the second.
(>>->) :: Monad m => (a -> m b) -> (a -> m r) -> a -> m r
(>>->) = liftA2 (>>)
infixl 1 >>->

withReaderT :: r -> ReaderT r m a -> m a
withReaderT = flip runReaderT
