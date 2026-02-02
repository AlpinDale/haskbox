module Cmd.True (run) where

import System.Exit (exitSuccess)

run :: [String] -> IO ()
run _ = exitSuccess
