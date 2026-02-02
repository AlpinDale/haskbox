{-# LANGUAGE OverloadedStrings #-}

module Cmd.Readlink (run) where

import Control.Exception (IOException, catch)
import System.Directory (canonicalizePath)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (readSymbolicLink)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox readlink: " ++ err
    exitFailure
  Right (opts, paths)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "readlink") >> exitSuccess
    | null paths -> do
        hPutStrLn stderr "haskbox readlink: missing operand"
        hPutStrLn stderr "Try 'haskbox readlink --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (readLink opts) paths
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optCanonicalize :: !Bool,
    optNoNewline :: !Bool,
    optQuiet :: !Bool,
    optVerbose :: !Bool,
    optZero :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optCanonicalize = False,
      optNoNewline = False,
      optQuiet = False,
      optVerbose = False,
      optZero = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--canonicalize" : rest) = parseArgs opts {optCanonicalize = True} rest
parseArgs opts ("--canonicalize-existing" : rest) = parseArgs opts {optCanonicalize = True} rest
parseArgs opts ("--canonicalize-missing" : rest) = parseArgs opts {optCanonicalize = True} rest
parseArgs opts ("--no-newline" : rest) = parseArgs opts {optNoNewline = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--silent" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--zero" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optCanonicalize = True} rest
parseArgs opts ("-e" : rest) = parseArgs opts {optCanonicalize = True} rest
parseArgs opts ("-m" : rest) = parseArgs opts {optCanonicalize = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optNoNewline = True} rest
parseArgs opts ("-q" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (c : cs) r = case c of
      'f' -> parseShortFlags o {optCanonicalize = True} cs r
      'e' -> parseShortFlags o {optCanonicalize = True} cs r
      'm' -> parseShortFlags o {optCanonicalize = True} cs r
      'n' -> parseShortFlags o {optNoNewline = True} cs r
      'q' -> parseShortFlags o {optQuiet = True} cs r
      's' -> parseShortFlags o {optQuiet = True} cs r
      'v' -> parseShortFlags o {optVerbose = True} cs r
      'z' -> parseShortFlags o {optZero = True} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (path : rest) = do
  (o, paths) <- parseArgs opts rest
  Right (o, path : paths)

readLink :: Opts -> FilePath -> IO Bool
readLink opts path = catch doRead handler
  where
    doRead = do
      target <-
        if optCanonicalize opts
          then canonicalizePath path
          else readSymbolicLink path
      outputResult target
      return False

    outputResult target
      | optZero opts = putStr (target ++ "\0")
      | optNoNewline opts = putStr target
      | otherwise = putStrLn target

    handler :: IOException -> IO Bool
    handler e
      | optQuiet opts = return True
      | otherwise = do
          when (optVerbose opts || not (optQuiet opts)) $
            hPutStrLn stderr $
              "haskbox readlink: " ++ path ++ ": " ++ friendlyError (show e)
          return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "Invalid argument" `isIn` s = "Invalid argument"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox readlink [OPTION]... FILE...",
        "Print value of a symbolic link or canonical file name.",
        "",
        "  -f, --canonicalize      canonicalize by following every symlink",
        "  -e, --canonicalize-existing  same as -f, but fail if path doesn't exist",
        "  -m, --canonicalize-missing   same as -f, but don't fail for missing components",
        "  -n, --no-newline        do not output trailing newline",
        "  -q, --quiet             suppress most error messages",
        "  -s, --silent            same as -q",
        "  -v, --verbose           report error messages",
        "  -z, --zero              end each output line with NUL, not newline",
        "      --help              display this help and exit",
        "      --version           output version information and exit"
      ]
