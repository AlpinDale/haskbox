{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cmd.Sync (run) where

import Control.Exception (IOException, catch)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Unistd (fileSynchronise, fileSynchroniseDataOnly)
import Version (versionString)

foreign import capi "unistd.h sync" c_sync :: IO ()

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox sync: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "sync") >> exitSuccess
    | null files -> c_sync >> exitSuccess
    | otherwise -> do
        results <- mapM (syncFile opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optDataSync :: !Bool,
    optFileSystem :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optDataSync = False,
      optFileSystem = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optDataSync = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optFileSystem = True} rest
parseArgs opts ("--data" : rest) = parseArgs opts {optDataSync = True} rest
parseArgs opts ("--file-system" : rest) = parseArgs opts {optFileSystem = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      'd' -> parseShortFlags o {optDataSync = True} fs r
      'f' -> parseShortFlags o {optFileSystem = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (arg : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, arg : files)

syncFile :: Opts -> FilePath -> IO Bool
syncFile opts path = catch doSync handler
  where
    doSync = do
      fd <- openFd path ReadOnly defaultFileFlags
      result <- doSyncFd fd
      closeFd fd
      return result

    doSyncFd fd
      | optDataSync opts = tryDataSync fd
      | otherwise = trySyncFd fd

    -- Try fdatasync, fall back to fsync if unsupported (e.g., on macOS)
    tryDataSync fd =
      catch
        (fileSynchroniseDataOnly fd >> return False)
        (\(_ :: IOException) -> trySyncFd fd)

    trySyncFd fd =
      catch
        (fileSynchronise fd >> return False)
        ( \(_ :: IOException) -> do
            hPutStrLn stderr $ "haskbox sync: error syncing '" ++ path ++ "'"
            return True
        )

    handler :: IOException -> IO Bool
    handler _ = do
      hPutStrLn stderr $ "haskbox sync: error opening '" ++ path ++ "'"
      return True

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox sync [OPTION]... [FILE]...",
        "Synchronize cached writes to persistent storage.",
        "",
        "If one or more files are specified, sync only them,",
        "otherwise, sync all filesystems.",
        "",
        "  -d, --data         avoid syncing metadata",
        "  -f, --file-system  sync the filesystems that contain the files",
        "                     (falls back to fsync on systems without syncfs)",
        "      --help         display this help and exit",
        "      --version      output version information and exit"
      ]
