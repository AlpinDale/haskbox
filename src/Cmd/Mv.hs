{-# LANGUAGE OverloadedStrings #-}

module Cmd.Mv (run) where

import Control.Exception (IOException, catch)
import System.Directory (doesDirectoryExist, doesFileExist, renamePath)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox mv: " ++ err
    exitFailure
  Right (opts, paths)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "mv") >> exitSuccess
    | length paths < 2 -> do
        hPutStrLn stderr "haskbox mv: missing destination file operand"
        hPutStrLn stderr "Try 'haskbox mv --help' for more information."
        exitFailure
    | otherwise -> do
        let dest = last paths
            sources = init paths
        destIsDir <- doesDirectoryExist dest
        if length sources > 1 && not destIsDir
          then do
            hPutStrLn stderr $ "haskbox mv: target '" ++ dest ++ "' is not a directory"
            exitFailure
          else do
            results <- mapM (moveItem opts dest destIsDir) sources
            if or results then exitFailure else exitSuccess

data Opts = Opts
  { optForce :: !Bool,
    optInteractive :: !Bool,
    optNoClobber :: !Bool,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optForce = False,
      optInteractive = False,
      optNoClobber = False,
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
parseArgs opts ("--no-clobber" : rest) = parseArgs opts {optNoClobber = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optInteractive = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optNoClobber = True} rest
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
      'n' -> parseShortFlags o {optNoClobber = True} cs r
      'v' -> parseShortFlags o {optVerbose = True} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

moveItem :: Opts -> FilePath -> Bool -> FilePath -> IO Bool
moveItem opts dest destIsDir src = catch doMove handler
  where
    targetPath = if destIsDir then dest </> takeFileName src else dest

    doMove = do
      destExists <- doesFileExist targetPath
      destDirExists <- doesDirectoryExist targetPath

      if (destExists || destDirExists) && optNoClobber opts
        then return False
        else do
          renamePath src targetPath
          when (optVerbose opts) $ putStrLn $ "renamed '" ++ src ++ "' -> '" ++ targetPath ++ "'"
          return False

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox mv: cannot move '" ++ src ++ "': " ++ friendlyError (show e)
      return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "Directory not empty" `isIn` s = "Directory not empty"
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
      [ "Usage: haskbox mv [OPTION]... SOURCE... DEST",
        "Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY.",
        "",
        "  -f, --force           do not prompt before overwriting",
        "  -i, --interactive     prompt before overwrite",
        "  -n, --no-clobber      do not overwrite an existing file",
        "  -v, --verbose         explain what is being done",
        "      --help            display this help and exit",
        "      --version         output version information and exit"
      ]
