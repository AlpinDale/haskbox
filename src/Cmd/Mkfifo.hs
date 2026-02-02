{-# LANGUAGE OverloadedStrings #-}

module Cmd.Mkfifo (run) where

import Control.Exception (IOException, catch)
import Data.Bits ((.|.))
import Data.Char (isOctDigit)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (createNamedPipe, groupReadMode, groupWriteMode, otherReadMode, otherWriteMode, ownerReadMode, ownerWriteMode)
import System.Posix.Types (FileMode)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox mkfifo: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "mkfifo") >> exitSuccess
    | null files -> do
        hPutStrLn stderr "haskbox mkfifo: missing operand"
        hPutStrLn stderr "Try 'haskbox mkfifo --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (makeFifo opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optMode :: !FileMode,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optMode = defaultMode,
      optShowHelp = False,
      optShowVersion = False
    }
  where
    defaultMode =
      ownerReadMode
        .|. ownerWriteMode
        .|. groupReadMode
        .|. groupWriteMode
        .|. otherReadMode
        .|. otherWriteMode -- 0666

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts (('-' : 'm' : m) : rest)
  | null m = case rest of
      (mode : rest') -> case parseMode mode of
        Just md -> parseArgs opts {optMode = md} rest'
        Nothing -> Left $ "invalid mode: '" ++ mode ++ "'"
      [] -> Left "option requires an argument -- 'm'"
  | otherwise = case parseMode m of
      Just md -> parseArgs opts {optMode = md} rest
      Nothing -> Left $ "invalid mode: '" ++ m ++ "'"
parseArgs opts ("--mode" : m : rest) = case parseMode m of
  Just md -> parseArgs opts {optMode = md} rest
  Nothing -> Left $ "invalid mode: '" ++ m ++ "'"
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

parseMode :: String -> Maybe FileMode
parseMode s
  | all isOctDigit s && not (null s) = Just $ parseOctal s
  | otherwise = Nothing
  where
    parseOctal = foldl (\acc c -> acc * 8 + fromIntegral (fromEnum c - fromEnum '0')) 0

makeFifo :: Opts -> FilePath -> IO Bool
makeFifo opts path = catch doMkfifo handler
  where
    doMkfifo = do
      createNamedPipe path (optMode opts)
      return False

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox mkfifo: cannot create fifo '" ++ path ++ "': " ++ friendlyError (show e)
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

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox mkfifo [OPTION]... NAME...",
        "Create named pipes (FIFOs) with the given NAMEs.",
        "",
        "  -m, --mode=MODE   set file permission bits to MODE, not a=rw - umask",
        "      --help        display this help and exit",
        "      --version     output version information and exit"
      ]
