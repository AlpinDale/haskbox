{-# LANGUAGE OverloadedStrings #-}

module Cmd.Cp (run) where

import Control.Exception (IOException, catch)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (createLink, createSymbolicLink, fileMode, getFileStatus, setFileMode)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox cp: " ++ err
    exitFailure
  Right (opts, paths)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "cp") >> exitSuccess
    | length paths < 2 -> do
        hPutStrLn stderr "haskbox cp: missing destination file operand"
        hPutStrLn stderr "Try 'haskbox cp --help' for more information."
        exitFailure
    | otherwise -> do
        let dest = last paths
            sources = init paths
        destIsDir <- doesDirectoryExist dest
        if length sources > 1 && not destIsDir
          then do
            hPutStrLn stderr $ "haskbox cp: target '" ++ dest ++ "' is not a directory"
            exitFailure
          else do
            results <- mapM (copyItem opts dest destIsDir) sources
            if or results then exitFailure else exitSuccess

data DerefMode = DerefNone | DerefAll | DerefCommandLine
  deriving (Eq)

data Opts = Opts
  { optRecursive :: !Bool,
    optForce :: !Bool,
    optInteractive :: !Bool,
    optNoClobber :: !Bool,
    optVerbose :: !Bool,
    optPreserve :: !Bool,
    optArchive :: !Bool,
    optLink :: !Bool,
    optSymlink :: !Bool,
    optUpdate :: !Bool,
    optDeref :: !DerefMode,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optRecursive = False,
      optForce = False,
      optInteractive = False,
      optNoClobber = False,
      optVerbose = False,
      optPreserve = False,
      optArchive = False,
      optLink = False,
      optSymlink = False,
      optUpdate = False,
      optDeref = DerefNone,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--recursive" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("--force" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("--interactive" : rest) = parseArgs opts {optInteractive = True} rest
parseArgs opts ("--no-clobber" : rest) = parseArgs opts {optNoClobber = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--preserve" : rest) = parseArgs opts {optPreserve = True} rest
parseArgs opts ("--archive" : rest) = parseArgs opts {optArchive = True, optRecursive = True, optPreserve = True} rest
parseArgs opts ("--link" : rest) = parseArgs opts {optLink = True} rest
parseArgs opts ("--symbolic-link" : rest) = parseArgs opts {optSymlink = True} rest
parseArgs opts ("--update" : rest) = parseArgs opts {optUpdate = True} rest
parseArgs opts ("--no-dereference" : rest) = parseArgs opts {optDeref = DerefNone} rest
parseArgs opts ("--dereference" : rest) = parseArgs opts {optDeref = DerefAll} rest
parseArgs opts ("-a" : rest) = parseArgs opts {optArchive = True, optRecursive = True, optPreserve = True} rest
parseArgs opts ("-l" : rest) = parseArgs opts {optLink = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optSymlink = True} rest
parseArgs opts ("-u" : rest) = parseArgs opts {optUpdate = True} rest
parseArgs opts ("-P" : rest) = parseArgs opts {optDeref = DerefNone} rest
parseArgs opts ("-L" : rest) = parseArgs opts {optDeref = DerefAll} rest
parseArgs opts ("-H" : rest) = parseArgs opts {optDeref = DerefCommandLine} rest
parseArgs opts ("-r" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("-R" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optForce = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optInteractive = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optNoClobber = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-p" : rest) = parseArgs opts {optPreserve = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (c : cs) r = case c of
      'r' -> parseShortFlags o {optRecursive = True} cs r
      'R' -> parseShortFlags o {optRecursive = True} cs r
      'f' -> parseShortFlags o {optForce = True} cs r
      'i' -> parseShortFlags o {optInteractive = True} cs r
      'n' -> parseShortFlags o {optNoClobber = True} cs r
      'v' -> parseShortFlags o {optVerbose = True} cs r
      'p' -> parseShortFlags o {optPreserve = True} cs r
      'a' -> parseShortFlags o {optArchive = True, optRecursive = True, optPreserve = True} cs r
      'l' -> parseShortFlags o {optLink = True} cs r
      's' -> parseShortFlags o {optSymlink = True} cs r
      'u' -> parseShortFlags o {optUpdate = True} cs r
      'P' -> parseShortFlags o {optDeref = DerefNone} cs r
      'L' -> parseShortFlags o {optDeref = DerefAll} cs r
      'H' -> parseShortFlags o {optDeref = DerefCommandLine} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

copyItem :: Opts -> FilePath -> Bool -> FilePath -> IO Bool
copyItem opts dest destIsDir src = do
  srcIsDir <- doesDirectoryExist src
  let targetPath = if destIsDir then dest </> takeFileName src else dest
  if srcIsDir
    then
      if optRecursive opts
        then copyDirRecursive opts src targetPath
        else do
          hPutStrLn stderr $ "haskbox cp: -r not specified; omitting directory '" ++ src ++ "'"
          return True
    else copyFileItem opts src targetPath

copyFileItem :: Opts -> FilePath -> FilePath -> IO Bool
copyFileItem opts src dest = catch doCopy handler
  where
    doCopy = do
      destExists <- doesFileExist dest
      if destExists && optNoClobber opts
        then return False
        else do
          shouldCopy <-
            if optUpdate opts && destExists
              then do
                srcTime <- getModificationTime src
                destTime <- getModificationTime dest
                return (srcTime > destTime)
              else return True
          when shouldCopy $ do
            if optSymlink opts
              then createSymbolicLink src dest
              else
                if optLink opts
                  then createLink src dest
                  else do
                    copyFile src dest
                    when (optPreserve opts || optArchive opts) $ preserveAttrs src dest
            when (optVerbose opts) $ putStrLn $ "'" ++ src ++ "' -> '" ++ dest ++ "'"
          return False

    preserveAttrs srcPath destPath = do
      srcStatus <- getFileStatus srcPath
      setFileMode destPath (fileMode srcStatus)

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox cp: cannot copy '" ++ src ++ "': " ++ friendlyError (show e)
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

copyDirRecursive :: Opts -> FilePath -> FilePath -> IO Bool
copyDirRecursive opts src dest = catch doCopy handler
  where
    doCopy = do
      createDirectoryIfMissing True dest
      when (optVerbose opts) $ putStrLn $ "'" ++ src ++ "' -> '" ++ dest ++ "'"
      contents <- listDirectory src
      results <- mapM copyChild contents
      return $ or results

    copyChild name = do
      let srcPath = src </> name
          destPath = dest </> name
      isDir <- doesDirectoryExist srcPath
      if isDir
        then copyDirRecursive opts srcPath destPath
        else copyFileItem opts srcPath destPath

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox cp: cannot copy '" ++ src ++ "': " ++ friendlyError (show e)
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
      [ "Usage: haskbox cp [OPTION]... SOURCE... DEST",
        "Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY.",
        "",
        "  -a, --archive           same as -dR --preserve=all",
        "  -f, --force             if an existing destination file cannot be opened,",
        "                            remove it and try again",
        "  -H                      follow command-line symbolic links in SOURCE",
        "  -i, --interactive       prompt before overwrite",
        "  -l, --link              hard link files instead of copying",
        "  -L, --dereference       always follow symbolic links in SOURCE",
        "  -n, --no-clobber        do not overwrite an existing file",
        "  -P, --no-dereference    never follow symbolic links in SOURCE",
        "  -p, --preserve          preserve mode, ownership, and timestamps",
        "  -r, -R, --recursive     copy directories recursively",
        "  -s, --symbolic-link     make symbolic links instead of copying",
        "  -u, --update            copy only when SOURCE is newer than destination",
        "  -v, --verbose           explain what is being done",
        "      --help              display this help and exit",
        "      --version           output version information and exit"
      ]
