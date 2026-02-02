module Cmd.False (run) where

import System.Exit (exitFailure)

run :: [String] -> IO ()
run _ = exitFailure
