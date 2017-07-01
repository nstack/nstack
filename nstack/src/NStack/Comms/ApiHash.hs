-- | Compute the hash of all API calls to detect when it changes
{-# LANGUAGE BangPatterns #-}
module NStack.Comms.ApiHash (apiHash) where

import Control.Monad.State (MonadState(..), evalStateT)
import Language.Haskell.TH (Q, Name)
import Instances.TH.Lift ()
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.ReifyMany as R
import qualified Language.Haskell.TH.ReifyMany.Internal as R
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteArray as BA
import Data.Generics.Uniplate.Data (universeBi, transformBiM)
import qualified Crypto.Hash as CH
import qualified Data.ByteString.Base16 as B16

import NStack.Comms.Types (allApiCalls)

getType :: Monad m => TH.Info -> m TH.Type
getType = \case
  TH.VarI _ ty _ -> return ty
  _ -> fail "API Call is not a function name"

-- | All types involved in the API.
apiInfos :: Q [(Name, TH.Info)]
apiInfos = do
  callInfos <- traverse TH.reify allApiCalls
  callTypes <- traverse getType callInfos
  tyInfos :: [(Name, TH.Info)] <- R.reifyManyTyCons
    (\(_, dec) -> return (R.isDataDec dec, R.decConcreteNames dec))
    $ universeBi callTypes
  let allInfos = zip allApiCalls callInfos ++ tyInfos
  evalStateT (transformBiM rename allInfos) (Map.empty, 0)

rename :: MonadState (Map.Map Int Int, Int) m => Name -> m Name
rename name@(TH.Name occ flavour) =
  case flavour of
    TH.NameG {} -> return name
    -- see https://ghc.haskell.org/trac/ghc/ticket/13728
    TH.NameU old_ind -> do
      (!old_to_new, !next_new) <- get
      case Map.lookup old_ind old_to_new of
        Nothing -> do
          put (Map.insert old_ind next_new old_to_new, next_new+1)
          return $ mkLocalName next_new
        Just i -> return $ mkLocalName i
    _ -> fail $ "Unexpected name: " ++ show name
  where
    mkLocalName :: Int -> Name
    mkLocalName i = TH.Name occ (TH.NameU i)

sha256 :: BS8.ByteString -> BS8.ByteString
sha256 = (BA.convert :: CH.Digest CH.SHA256 -> BS8.ByteString) . CH.hash

-- | Compute the API hash at compile-time, return an expression of type
-- 'BS8.ByteString'.
apiHash :: Q TH.Exp
apiHash = do
  bs <- B16.encode . sha256 . BS8.pack . show <$> apiInfos
  TH.lift bs
