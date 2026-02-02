module Cmd.Pwd (run) where

import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "pwd") >> exitSuccess
  _ -> getCurrentDirectory >>= putStrLn

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox pwd [OPTION]...",
        "Print the full filename of the current working directory.",
        "",
        "      --help     display this help and exit",
        "      --version  output version information and exit"
      ]
