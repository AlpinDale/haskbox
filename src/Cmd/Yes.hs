{-# LANGUAGE OverloadedStrings #-}

module Cmd.Yes (run) where

import Data.ByteString.Char8 qualified as C8
import System.IO (stdout)

run :: [String] -> IO ()
run [] = yesLoop "y"
run strs = yesLoop (unwords strs)

yesLoop :: String -> IO ()
yesLoop s = do
  let !line = C8.pack (s ++ "\n")
  let go = C8.hPut stdout line >> go
  go
