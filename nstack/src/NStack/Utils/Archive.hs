module NStack.Utils.Archive (
pack, expandCheckPack, unpack
) where

import Control.Monad (filterM, forM)
import qualified Codec.Archive.Tar as Tar  -- from: tar
import qualified Data.ByteString.Lazy as BS  -- from: bytestring
import Data.List (nub)
import System.FilePath ((</>), joinPath, splitDirectories)
import System.Directory (doesFileExist)
import System.FilePath.Glob (namesMatching)
import NStack.Prelude.Exception (throwPermanentError)

-- | Create a tar archive from a list of files
pack
  :: FilePath -- ^ base directory
  -> [FilePath] -- ^ list of files to archive
  -> IO BS.ByteString
pack dir files = Tar.write <$> Tar.pack dir files

-- | Expand globs, check that files exist, and pack them
--
-- See <https://github.com/nstack/nstack-server/issues/432> for the
-- context.
expandCheckPack
  :: FilePath -- ^ base directory
  -> [FilePath]
      -- ^ List of optional files to archive.
      -- If any of these don't exist, they are silently skipped.
  -> [FilePath]
      -- ^ List of glob patterns.
      -- Each pattern must expand to at least one file.
  -> IO BS.ByteString
expandCheckPack dir opt_files globs = do

  globbed_files <- fmap concat . forM globs $ \glob -> do
    files <- namesMatching (dir </> glob)
    if null files
      then throwPermanentError $ "Pattern " ++ glob ++ " did not match any files"
      else return files
  existing_opt_files <- filterM doesFileExist . fmap (dir </>) $ opt_files

  let files = fmap (makeRelativePath dir) . nub $ existing_opt_files ++ globbed_files
  pack dir files

-- | Make a path relative to a new root, assumes each path is nested within the new root
makeRelativePath :: FilePath -> FilePath -> FilePath
makeRelativePath dir file = joinPath $ drop (length dirs) files
  where dirs = splitDirectories dir
        files = splitDirectories file

-- | Unpack a tar project archive to a directory
unpack :: FilePath -> BS.ByteString -> IO ()
unpack path = Tar.unpack path . Tar.read
