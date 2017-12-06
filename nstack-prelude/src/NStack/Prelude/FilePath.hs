module NStack.Prelude.FilePath (
(</>)
  -- * File path conversions
, toFP
, fromFP
, formatFP
, fpToText
, directory
) where

import Control.Monad.Except (MonadError)
import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import qualified Filesystem.Path.CurrentOS as FP
import Turtle (format, fp, (</>))

import NStack.Prelude.Monad (eitherToExcept)

fromFP :: FP.FilePath -> FilePath
fromFP = FP.encodeString

toFP :: FilePath -> FP.FilePath
toFP = FP.decodeString

fpToText :: MonadError String m => FP.FilePath -> m Text
fpToText = eitherToExcept . first (("Failed to parse FilePath. Attempt was: " ++) . unpack) . FP.toText

formatFP :: FP.FilePath -> Text
formatFP = format fp

directory :: FilePath -> FilePath
directory = fromFP . FP.directory . toFP
