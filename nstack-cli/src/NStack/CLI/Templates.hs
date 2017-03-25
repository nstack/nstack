{-# LANGUAGE TemplateHaskell #-}
module NStack.CLI.Templates where
import Prelude hiding (lookup, writeFile)
import Control.Applicative
import Data.ByteString (ByteString, writeFile)     -- from: bytestring
import Data.FileEmbed                              -- from: file-embed
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Map (Map, alter, lookup)               -- from: collections
import System.Directory (createDirectoryIfMissing) -- from: directory
import System.FilePath ((</>),
                        splitFileName,
                        splitPath,
                        dropTrailingPathSeparator,
                        normalise,
                        takeDirectory)             -- from: filepath

templates :: Map String [(FilePath, ByteString)]
templates = foldr step mempty entries
  where step (a, x)  = alterDef (x :) [] a
        alterDef f d = alter (maybe (Just $ f d) (Just . f))
        entries = do (a, b) <- $(makeRelativeToProject ("data" </> "client" </> "templates" </> "init") >>= embedDir)
                     let (path, file) = splitFileName a
                     case uncons (splitPath path) of
                       Nothing          -> empty -- this should never actually happen due
                                                 -- to semantics of `splitFileName`
                                                 -- splitFileName "foo.txt" == ("./", "foo.txt")
                       Just (prefix, r) -> return (dropTrailingPathSeparator prefix, (mconcat r </> file, b))

lookupPrefix :: String -> [(FilePath, ByteString)]
lookupPrefix s = fromMaybe [] $ lookup s templates

createFromTemplate :: FilePath -> String -> IO ()
createFromTemplate fp p = sequence_ [makeFile fn b | (a, b) <- lookupPrefix p
                                                   , let fn = normalise fp </> a]
   where makeFile fn b = createDirectoryIfMissing True (takeDirectory fn) >> writeFile fn b
