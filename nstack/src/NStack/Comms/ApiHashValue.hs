-- | This module defines the API hash value ('apiHashValue') as a plain
-- value, not a splice.
--
-- The reason it is not defined in "NStack.Comms.ApiHash" is the ghc
-- stage restriction.
--
-- The reason we don't simply invoke @$(apiHash)@ whenever we need it is
-- this ghc bug: <https://ghc.haskell.org/trac/ghc/ticket/13729>.
-- Besides, it makes it slightly faster to compile because the compile-time
-- calculations are cached here.
{-# LANGUAGE TemplateHaskell #-}
module NStack.Comms.ApiHashValue where

import qualified Data.ByteString as BS
import NStack.Comms.ApiHash (apiHash)

apiHashValue :: BS.ByteString
apiHashValue = $(apiHash)
