{-# LANGUAGE TemplateHaskell #-}
module NStack.Utils.Debug (
  panic, panicList, versionMsg
)
where

import Data.Version (showVersion)
import Development.GitRev              -- from: gitrev
import Paths_nstack (version)

-- | return the full version of the nstack library
versionMsg :: String
versionMsg = concat $ [ "Version ", showVersion version
                      , " [", $(gitBranch), "@", $(gitHash)
                      , " (", $(gitCommitDate), ")"
                      , " (", $(gitCommitCount), " commits in HEAD)"
                      , dirty, "]" ]
  where
    dirty | $(gitDirty) = " (uncommitted files present)"
          | otherwise   = ""

-- | Unrecoverable error
panic :: String -> a
panic msg = error $ versionMsg ++ "\nPanic - " ++ msg

panicList :: (Show b) => String -> [(String, b)] -> a
panicList msg vars = panic (msg ++ "\n" ++ unlines vars')
  where
    vars' = map (\(name, val) -> name ++ ": " ++ show val) vars
