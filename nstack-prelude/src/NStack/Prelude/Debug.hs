module NStack.Prelude.Debug (
DT.traceShow, DT.trace,
traceAll, printAll
)
where

import qualified Debug.Trace as DT

traceAll :: (Show x) => [x] -> y -> y
traceAll xs y = foldl (flip DT.traceShow) y xs

printAll :: (Show x) => [x] -> IO ()
printAll xs = mapM_ print xs
