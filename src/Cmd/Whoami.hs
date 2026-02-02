module Cmd.Whoami (run) where

import System.Exit (exitSuccess)
import System.Posix.User (getEffectiveUserName)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "whoami") >> exitSuccess
  _ -> getEffectiveUserName >>= putStrLn

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox whoami [OPTION]...",
        "Print the user name associated with the current effective user ID.",
        "",
        "      --help     display this help and exit",
        "      --version  output version information and exit"
      ]
