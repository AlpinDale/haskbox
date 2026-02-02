{-# LANGUAGE OverloadedStrings #-}

module Cmd.Rm (run) where

import Control.Exception (IOException, catch)
import System.Directory (doesDirectoryExist, listDirectory, removeDirectory, removeFile)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox rm: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "rm") >> exitSuccess
    | null files -> do
        hPutStrLn stderr "haskbox rm: missing operand"
        hPutStrLn stderr "Try 'haskbox rm --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (removeItem opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optForce :: !Bool,
    optInteractive :: !Bool,
    optRecursive :: !Bool,
    optDir :: !Bool,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optForce = False,
      optInteractive = False,
      optRecursive = False,
      optDir = False,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--force" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("--interactive" : rest) = parseArgs opts {optInteractive = True} rest
parseArgs opts ("--recursive" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("--dir" : rest) = parseArgs opts {optDir = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optInteractive = True} rest
parseArgs opts ("-r" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("-R" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optDir = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (c : cs) r = case c of
      'f' -> parseShortFlags o {optForce = True} cs r
      'i' -> parseShortFlags o {optInteractive = True} cs r
      'r' -> parseShortFlags o {optRecursive = True} cs r
      'R' -> parseShortFlags o {optRecursive = True} cs r
      'd' -> parseShortFlags o {optDir = True} cs r
      'v' -> parseShortFlags o {optVerbose = True} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

removeItem :: Opts -> FilePath -> IO Bool
removeItem opts path = do
  isDir <- doesDirectoryExist path
  if isDir
    then
      if optRecursive opts
        then removeRecursive opts path
        else
          if optDir opts
            then removeEmptyDir opts path
            else
              if optForce opts
                then return False
                else do
                  hPutStrLn stderr $ "haskbox rm: cannot remove '" ++ path ++ "': Is a directory"
                  return True
    else removeRegularFile opts path

removeRegularFile :: Opts -> FilePath -> IO Bool
removeRegularFile opts path = catch doRemove handler
  where
    doRemove = do
      removeFile path
      when (optVerbose opts) $ putStrLn $ "removed '" ++ path ++ "'"
      return False

    handler :: IOException -> IO Bool
    handler e
      | optForce opts && "does not exist" `isIn` show e = return False
      | otherwise = do
          hPutStrLn stderr $ "haskbox rm: cannot remove '" ++ path ++ "': " ++ friendlyError (show e)
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

removeEmptyDir :: Opts -> FilePath -> IO Bool
removeEmptyDir opts path = catch doRemove handler
  where
    doRemove = do
      removeDirectory path
      when (optVerbose opts) $ putStrLn $ "removed directory '" ++ path ++ "'"
      return False

    handler :: IOException -> IO Bool
    handler e
      | optForce opts && "does not exist" `isIn` show e = return False
      | otherwise = do
          hPutStrLn stderr $ "haskbox rm: cannot remove '" ++ path ++ "': " ++ friendlyError (show e)
          return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "not empty" `isIn` s || "Directory not empty" `isIn` s = "Directory not empty"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

removeRecursive :: Opts -> FilePath -> IO Bool
removeRecursive opts path = catch doRemove handler
  where
    doRemove = do
      contents <- listDirectory path
      results <- mapM (removeItem opts . (path </>)) contents
      if or results
        then return True
        else do
          removeDirectory path
          when (optVerbose opts) $ putStrLn $ "removed directory '" ++ path ++ "'"
          return False

    handler :: IOException -> IO Bool
    handler e
      | optForce opts && "does not exist" `isIn` show e = return False
      | otherwise = do
          hPutStrLn stderr $ "haskbox rm: cannot remove '" ++ path ++ "': " ++ friendlyError (show e)
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

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox rm [OPTION]... [FILE]...",
        "Remove (unlink) the FILE(s).",
        "",
        "  -f, --force           ignore nonexistent files, never prompt",
        "  -i, --interactive     prompt before every removal",
        "  -r, -R, --recursive   remove directories and their contents recursively",
        "  -d, --dir             remove empty directories",
        "  -v, --verbose         explain what is being done",
        "      --help            display this help and exit",
        "      --version         output version information and exit"
      ]
