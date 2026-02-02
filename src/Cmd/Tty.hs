{-# LANGUAGE OverloadedStrings #-}

module Cmd.Tty (run) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (getTerminalName, queryTerminal)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox tty: " ++ err
    exitFailure
  Right opts
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "tty") >> exitSuccess
    | optSilent opts -> do
        isTty <- queryTerminal stdInput
        if isTty then exitSuccess else exitFailure
    | otherwise -> do
        isTty <- queryTerminal stdInput
        if isTty
          then do
            name <- getTerminalName stdInput
            putStrLn name
            exitSuccess
          else do
            putStrLn "not a tty"
            exitFailure

data Opts = Opts
  { optSilent :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optSilent = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String Opts
parseArgs opts [] = Right opts
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--silent" : rest) = parseArgs opts {optSilent = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optSilent = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optSilent = True} rest
parseArgs opts ("--" : _) = Right opts
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (_ : rest) = parseArgs opts rest

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox tty [OPTION]...",
        "Print the file name of the terminal connected to standard input.",
        "",
        "  -s, --silent, --quiet   print nothing, only return an exit status",
        "      --help              display this help and exit",
        "      --version           output version information and exit"
      ]
