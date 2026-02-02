{-# LANGUAGE OverloadedStrings #-}

module Cmd.Realpath (run) where

import Control.Exception (IOException, catch)
import System.Directory (canonicalizePath, makeAbsolute)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox realpath: " ++ err
    exitFailure
  Right (opts, paths)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "realpath") >> exitSuccess
    | null paths -> do
        hPutStrLn stderr "haskbox realpath: missing operand"
        hPutStrLn stderr "Try 'haskbox realpath --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (resolvePath opts) paths
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optNoSymlinks :: !Bool,
    optQuiet :: !Bool,
    optZero :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optNoSymlinks = False,
      optQuiet = False,
      optZero = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--no-symlinks" : rest) = parseArgs opts {optNoSymlinks = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--zero" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optNoSymlinks = True} rest
parseArgs opts ("-q" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      's' -> parseShortFlags o {optNoSymlinks = True} fs r
      'q' -> parseShortFlags o {optQuiet = True} fs r
      'z' -> parseShortFlags o {optZero = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (path : rest) = do
  (o, paths) <- parseArgs opts rest
  Right (o, path : paths)

resolvePath :: Opts -> FilePath -> IO Bool
resolvePath opts path = catch doResolve handler
  where
    doResolve = do
      resolved <-
        if optNoSymlinks opts
          then makeAbsolute path
          else canonicalizePath path
      if optZero opts
        then putStr (resolved ++ "\0")
        else putStrLn resolved
      return False

    handler :: IOException -> IO Bool
    handler e
      | optQuiet opts = return True
      | otherwise = do
          hPutStrLn stderr $ "haskbox realpath: " ++ path ++ ": " ++ friendlyError (show e)
          return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox realpath [OPTION]... FILE...",
        "Print the resolved absolute file name.",
        "",
        "  -q, --quiet            suppress most error messages",
        "  -s, --no-symlinks      don't expand symlinks",
        "  -z, --zero             end each output line with NUL, not newline",
        "      --help             display this help and exit",
        "      --version          output version information and exit"
      ]
