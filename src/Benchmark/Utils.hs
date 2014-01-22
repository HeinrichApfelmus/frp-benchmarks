module Benchmark.Utils where

import Control.DeepSeq
import System.IO

{-----------------------------------------------------------------------------
    Common utilities for benchmarking
------------------------------------------------------------------------------}
-- | Print a 'String' to indicated progress.
-- Rederict 'stderr' to avoid output in the terminal.
doSomething :: String -> IO ()
doSomething = hPutStrLn stderr