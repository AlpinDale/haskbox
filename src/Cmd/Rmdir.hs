{-# LANGUAGE OverloadedStrings #-}

module Cmd.Rmdir (run) where

import Control.Exception (IOException, catch)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Posix.Directory (removeDirectory)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox rmdir: " ++ err
    exitFailure
  Right (opts, dirs)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "rmdir") >> exitSuccess
    | null dirs -> do
        hPutStrLn stderr "haskbox rmdir: missing operand"
        hPutStrLn stderr "Try 'haskbox rmdir --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (removeDir opts) dirs
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optParents :: !Bool,
    optIgnoreNonEmpty :: !Bool,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optParents = False,
      optIgnoreNonEmpty = False,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--parents" : rest) = parseArgs opts {optParents = True} rest
parseArgs opts ("--ignore-fail-on-non-empty" : rest) = parseArgs opts {optIgnoreNonEmpty = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-p" : rest) = parseArgs opts {optParents = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      'p' -> parseShortFlags o {optParents = True} fs r
      'v' -> parseShortFlags o {optVerbose = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (dir : rest) = do
  (o, dirs) <- parseArgs opts rest
  Right (o, dir : dirs)

removeDir :: Opts -> FilePath -> IO Bool
removeDir opts dir
  | optParents opts = removeDirParents opts dir
  | otherwise = removeDirSingle opts dir

removeDirSingle :: Opts -> FilePath -> IO Bool
removeDirSingle opts dir = catch doRemove handler
  where
    doRemove = do
      removeDirectory dir
      when (optVerbose opts) $
        putStrLn $
          "haskbox rmdir: removing directory, '" ++ dir ++ "'"
      return False

    handler :: IOException -> IO Bool
    handler e
      | optIgnoreNonEmpty opts && ("not empty" `isIn` show e || "Directory not empty" `isIn` show e) =
          return False
      | otherwise = do
          hPutStrLn stderr $ "haskbox rmdir: failed to remove '" ++ dir ++ "': " ++ friendlyError (show e)
          return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "not empty" `isIn` s || "Directory not empty" `isIn` s = "Directory not empty"
      | "Not a directory" `isIn` s = "Not a directory"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

removeDirParents :: Opts -> FilePath -> IO Bool
removeDirParents opts = go
  where
    go path
      | path == "/" || path == "." || path == "" = return False
      | otherwise = do
          result <- removeDirSingle opts path
          if result
            then return True
            else go (takeDirectory path)

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox rmdir [OPTION]... DIRECTORY...",
        "Remove the DIRECTORY(ies), if they are empty.",
        "",
        "      --ignore-fail-on-non-empty",
        "                    ignore each failure that is solely because a directory",
        "                    is non-empty",
        "  -p, --parents     remove DIRECTORY and its ancestors",
        "  -v, --verbose     output a diagnostic for every directory processed",
        "      --help        display this help and exit",
        "      --version     output version information and exit"
      ]
