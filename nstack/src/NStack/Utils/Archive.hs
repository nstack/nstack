module NStack.Utils.Archive (
pack, unpack
) where

import qualified Codec.Archive.Tar as Tar  -- from: tar
import qualified Data.ByteString.Lazy as BS  -- from: bytestring
import qualified Turtle as R               -- from: turtle

import NStack.Prelude.FilePath (fromFP)

-- | Convert a directory into a tar archive
pack :: R.FilePath -> IO BS.ByteString
pack dir = Tar.write <$> Tar.pack (fromFP dir) ["."]

-- | Unpack a tar project archive to a directory
unpack :: R.FilePath -> BS.ByteString -> IO ()
unpack path = Tar.unpack (fromFP path) . Tar.read
