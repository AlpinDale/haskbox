{-# LANGUAGE OverloadedStrings #-}

module Cmd.Ln (run) where

import Control.Exception (IOException, catch)
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (createLink, createSymbolicLink, fileExist, removeLink)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox ln: " ++ err
    exitFailure
  Right (opts, targets)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "ln") >> exitSuccess
    | null targets -> do
        hPutStrLn stderr "haskbox ln: missing file operand"
        hPutStrLn stderr "Try 'haskbox ln --help' for more information."
        exitFailure
    | [target] <- targets -> do
        let linkName = takeFileName target
        result <- createLinkFile opts target linkName
        if result then exitFailure else exitSuccess
    | otherwise -> do
        let dest = last targets
            sources = init targets
        isDir <- doesDirectoryExist dest
        if length sources > 1 && not isDir
          then do
            hPutStrLn stderr $ "haskbox ln: target '" ++ dest ++ "' is not a directory"
            exitFailure
          else do
            results <-
              if isDir
                then mapM (\src -> createLinkFile opts src (dest </> takeFileName src)) sources
                else mapM (\src -> createLinkFile opts src dest) sources
            if or results then exitFailure else exitSuccess

data Opts = Opts
  { optSymbolic :: !Bool,
    optForce :: !Bool,
    optNoDereference :: !Bool,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optSymbolic = False,
      optForce = False,
      optNoDereference = False,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--symbolic" : rest) = parseArgs opts {optSymbolic = True} rest
parseArgs opts ("--force" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("--no-dereference" : rest) = parseArgs opts {optNoDereference = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optSymbolic = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optNoDereference = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (c : cs) r = case c of
      's' -> parseShortFlags o {optSymbolic = True} cs r
      'f' -> parseShortFlags o {optForce = True} cs r
      'n' -> parseShortFlags o {optNoDereference = True} cs r
      'v' -> parseShortFlags o {optVerbose = True} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

createLinkFile :: Opts -> FilePath -> FilePath -> IO Bool
createLinkFile opts target linkName = catch doLink handler
  where
    doLink = do
      when (optForce opts) $ do
        exists <- fileExist linkName
        when exists $ removeLink linkName

      if optSymbolic opts
        then createSymbolicLink target linkName
        else createLink target linkName

      when (optVerbose opts) $
        putStrLn $
          "'" ++ linkName ++ "' -> '" ++ target ++ "'"
      return False

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $
        "haskbox ln: failed to create "
          ++ (if optSymbolic opts then "symbolic " else "")
          ++ "link '"
          ++ linkName
          ++ "': "
          ++ friendlyError (show e)
      return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "already exists" `isIn` s = "File exists"
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
      [ "Usage: haskbox ln [OPTION]... TARGET LINK_NAME",
        "       haskbox ln [OPTION]... TARGET... DIRECTORY",
        "Create hard or symbolic links.",
        "",
        "  -f, --force              remove existing destination files",
        "  -n, --no-dereference     treat LINK_NAME as a normal file if it is a symlink",
        "  -s, --symbolic           make symbolic links instead of hard links",
        "  -v, --verbose            print name of each linked file",
        "      --help               display this help and exit",
        "      --version            output version information and exit"
      ]
