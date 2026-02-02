{-# LANGUAGE OverloadedStrings #-}

module Cmd.Tee (run) where

import Control.Exception (IOException, catch, finally)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, IOMode (..), hClose, hPutStrLn, openFile, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox tee: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "tee") >> exitSuccess
    | otherwise -> teeFiles opts files

data Opts = Opts
  { optAppend :: !Bool,
    optIgnoreInterrupts :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optAppend = False,
      optIgnoreInterrupts = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--append" : rest) = parseArgs opts {optAppend = True} rest
parseArgs opts ("--ignore-interrupts" : rest) = parseArgs opts {optIgnoreInterrupts = True} rest
parseArgs opts ("-a" : rest) = parseArgs opts {optAppend = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optIgnoreInterrupts = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

teeFiles :: Opts -> [FilePath] -> IO ()
teeFiles opts files = do
  let mode = if optAppend opts then AppendMode else WriteMode
  handles <- mapM (openFileWithError mode) files
  let validHandles = catMaybes handles
  finally
    (teeLoop validHandles)
    (mapM_ hClose validHandles)
  if length validHandles < length files
    then exitFailure
    else exitSuccess

openFileWithError :: IOMode -> FilePath -> IO (Maybe Handle)
openFileWithError mode path = catch (Just <$> openFile path mode) handler
  where
    handler :: IOException -> IO (Maybe Handle)
    handler e = do
      hPutStrLn stderr $ "haskbox tee: " ++ path ++ ": " ++ friendlyError (show e)
      return Nothing

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s || "inappropriate type" `isIn` s = "Is a directory"
      | otherwise = dropPrefix s

    dropPrefix str = case dropWhile (/= ':') str of
      [] -> str
      (_ : r) -> dropWhile (== ' ') r

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

teeLoop :: [Handle] -> IO ()
teeLoop handles = go
  where
    go = do
      chunk <- BS.hGetSome stdin 65536
      if BS.null chunk
        then return ()
        else do
          BS.hPut stdout chunk
          mapM_ (`BS.hPut` chunk) handles
          go

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox tee [OPTION]... [FILE]...",
        "Copy standard input to each FILE, and also to standard output.",
        "",
        "  -a, --append              append to the given FILEs, do not overwrite",
        "  -i, --ignore-interrupts   ignore interrupt signals",
        "      --help                display this help and exit",
        "      --version             output version information and exit"
      ]
