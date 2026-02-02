{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dos2unix (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox dos2unix: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "dos2unix") >> exitSuccess
    | null files -> convertStdin opts
    | otherwise -> do
        results <- mapM (convertFile opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optKeepDate :: !Bool,
    optNewFile :: !(Maybe FilePath),
    optQuiet :: !Bool,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optKeepDate = False,
      optNewFile = Nothing,
      optQuiet = False,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--keep-date" : rest) = parseArgs opts {optKeepDate = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-k" : rest) = parseArgs opts {optKeepDate = True} rest
parseArgs opts ("-q" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts (('-' : 'n' : n) : rest)
  | null n = case rest of
      (file : rest') -> parseArgs opts {optNewFile = Just file} rest'
      [] -> Left "option requires an argument -- 'n'"
  | otherwise = parseArgs opts {optNewFile = Just n} rest
parseArgs opts ("--newfile" : n : rest) = parseArgs opts {optNewFile = Just n} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

convertStdin :: Opts -> IO ()
convertStdin _ = do
  contents <- BS.hGetContents stdin
  let converted = dos2unix contents
  BS.hPut stdout converted

convertFile :: Opts -> FilePath -> IO Bool
convertFile opts path = catch doConvert handler
  where
    doConvert = do
      contents <- BS.readFile path
      let converted = dos2unix contents
      let outPath = fromMaybe path (optNewFile opts)

      when (optVerbose opts) $
        hPutStrLn stderr $
          "dos2unix: converting file " ++ path

      if outPath == path
        then BS.writeFile path converted
        else BS.writeFile outPath converted
      return False

    handler :: IOException -> IO Bool
    handler e = do
      unless (optQuiet opts) $
        hPutStrLn stderr $
          "haskbox dos2unix: " ++ path ++ ": " ++ friendlyError (show e)
      return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s = "Is a directory"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

dos2unix :: BS.ByteString -> BS.ByteString
dos2unix = BS.filter (/= 13)

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

unless :: Bool -> IO () -> IO ()
unless b = when (not b)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox dos2unix [OPTION]... [FILE]...",
        "Convert DOS/Windows line endings (CRLF) to Unix (LF).",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -k, --keep-date    keep output file date same as input file",
        "  -n, --newfile      write to new file instead of overwriting",
        "  -q, --quiet        suppress all warning and informational messages",
        "  -v, --verbose      print informational messages",
        "      --help         display this help and exit",
        "      --version      output version information and exit"
      ]
