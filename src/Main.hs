module Main (main) where

import Cmd.Cat qualified as Cat
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [] -> do
      printUsage
      exitFailure
    ("--help" : _) -> printUsage
    ("--version" : _) -> putStrLn "haskbox 0.1.0"
    (cmd : rest) -> dispatch prog cmd rest

dispatch :: String -> String -> [String] -> IO ()
dispatch _ "cat" args = Cat.run args
dispatch prog cmd _ = do
  hPutStrLn stderr $ prog ++ ": unknown command '" ++ cmd ++ "'"
  hPutStrLn stderr $ "Run '" ++ prog ++ " --help' for usage."
  exitFailure

printUsage :: IO ()
printUsage =
  putStr $
    unlines
      [ "haskbox - Unix utilities in Haskell",
        "",
        "Usage: haskbox <command> [args...]",
        "",
        "Commands:",
        "  cat    Concatenate and print files",
        "",
        "Options:",
        "  --help     Show this help",
        "  --version  Show version",
        "",
        "Run 'haskbox <command> --help' for command-specific help."
      ]
