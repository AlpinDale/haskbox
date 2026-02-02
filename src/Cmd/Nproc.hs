{-# LANGUAGE OverloadedStrings #-}

module Cmd.Nproc (run) where

import Control.Exception (SomeException, catch)
import GHC.Conc (getNumProcessors)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox nproc: " ++ err
    exitFailure
  Right opts
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "nproc") >> exitSuccess
    | otherwise -> do
        n <- getProcessorCount
        let result = max 1 (n - optIgnore opts)
        print result
        exitSuccess

getProcessorCount :: IO Int
getProcessorCount = catch tryMacOS fallback
  where
    tryMacOS = do
      out <- readProcess "sysctl" ["-n", "hw.ncpu"] ""
      case reads (filter (/= '\n') out) of
        [(n, "")] -> return n
        _ -> fallback (undefined :: SomeException)

    fallback :: SomeException -> IO Int
    fallback _ = getNumProcessors

data Opts = Opts
  { optAll :: !Bool,
    optIgnore :: !Int,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optAll = False,
      optIgnore = 0,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String Opts
parseArgs opts [] = Right opts
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--all" : rest) = parseArgs opts {optAll = True} rest
parseArgs opts ("--" : _) = Right opts
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (_ : rest) = parseArgs opts rest

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox nproc [OPTION]...",
        "Print the number of processing units available.",
        "",
        "      --all        print the number of installed processors",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
