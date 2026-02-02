module Cmd.Logname (run) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getLoginName)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  [] -> getLoginName >>= putStrLn
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "logname") >> exitSuccess
  (('-' : '-' : opt) : _) -> do
    hPutStrLn stderr $ "haskbox logname: unrecognized option '--" ++ opt ++ "'"
    exitFailure
  (('-' : c : _) : _) -> do
    hPutStrLn stderr $ "haskbox logname: invalid option -- '" ++ [c] ++ "'"
    exitFailure
  _ -> do
    hPutStrLn stderr "haskbox logname: extra operand"
    exitFailure

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox logname [OPTION]",
        "Print the name of the current user.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
