{-# LANGUAGE OverloadedStrings #-}

module Cmd.Mktemp (run) where

import Control.Exception (IOException, catch)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectory, getTemporaryDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hClose, hPutStrLn, openTempFile, stderr)
import System.Random (randomRIO)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox mktemp: " ++ err
    exitFailure
  Right (opts, templates)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "mktemp") >> exitSuccess
    | otherwise -> do
        let template = case templates of
              [] -> "tmp.XXXXXXXXXX"
              (t : _) -> t
        result <- createTemp opts template
        case result of
          Left err -> do
            hPutStrLn stderr $ "haskbox mktemp: " ++ err
            exitFailure
          Right path -> putStrLn path

data Opts = Opts
  { optDirectory :: !Bool,
    optDryRun :: !Bool,
    optQuiet :: !Bool,
    optTmpdir :: !(Maybe FilePath),
    optSuffix :: !(Maybe String),
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optDirectory = False,
      optDryRun = False,
      optQuiet = False,
      optTmpdir = Nothing,
      optSuffix = Nothing,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--directory" : rest) = parseArgs opts {optDirectory = True} rest
parseArgs opts ("--dry-run" : rest) = parseArgs opts {optDryRun = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optDirectory = True} rest
parseArgs opts ("-u" : rest) = parseArgs opts {optDryRun = True} rest
parseArgs opts ("-q" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts (('-' : 'p' : p) : rest)
  | null p = case rest of
      (dir : rest') -> parseArgs opts {optTmpdir = Just dir} rest'
      [] -> Left "option requires an argument -- 'p'"
  | otherwise = parseArgs opts {optTmpdir = Just p} rest
parseArgs opts ("--tmpdir" : dir : rest) = parseArgs opts {optTmpdir = Just dir} rest
parseArgs opts (('-' : '-' : 's' : 'u' : 'f' : 'f' : 'i' : 'x' : '=' : s) : rest) =
  parseArgs opts {optSuffix = Just s} rest
parseArgs opts ("--suffix" : s : rest) = parseArgs opts {optSuffix = Just s} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

createTemp :: Opts -> String -> IO (Either String FilePath)
createTemp opts template = do
  tmpDir <- maybe getTemporaryDirectory return (optTmpdir opts)

  let (prefix, rest) = break (== 'X') template
      xCount = length $ takeWhile (== 'X') rest
      suffix = dropWhile (== 'X') rest ++ fromMaybe "" (optSuffix opts)

  if xCount < 3
    then return $ Left "too few X's in template"
    else do
      randomPart <- generateRandom xCount
      let name = prefix ++ randomPart ++ suffix
          fullPath = tmpDir </> name

      if optDryRun opts
        then return $ Right fullPath
        else
          if optDirectory opts
            then catch (createTempDir fullPath) handler
            else catch (createTempFile tmpDir (prefix ++ randomPart) suffix) handler
  where
    handler :: IOException -> IO (Either String FilePath)
    handler e = return $ Left $ friendlyError (show e)

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

createTempDir :: FilePath -> IO (Either String FilePath)
createTempDir path = do
  createDirectory path
  return $ Right path

createTempFile :: FilePath -> String -> String -> IO (Either String FilePath)
createTempFile dir prefix suffix = do
  (path, handle) <- openTempFile dir (prefix ++ suffix)
  hClose handle
  return $ Right path

generateRandom :: Int -> IO String
generateRandom n = mapM (const randomChar) [1 .. n]
  where
    chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    randomChar = do
      idx <- randomRIO (0, length chars - 1)
      return $ chars !! idx

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox mktemp [OPTION]... [TEMPLATE]",
        "Create a temporary file or directory, safely, and print its name.",
        "",
        "TEMPLATE must contain at least 3 consecutive 'X's in last component.",
        "If TEMPLATE is not specified, use tmp.XXXXXXXXXX.",
        "",
        "  -d, --directory        create a directory, not a file",
        "  -u, --dry-run          do not create anything; merely print a name",
        "  -q, --quiet            suppress diagnostics about file/dir-creation failure",
        "  -p DIR, --tmpdir=DIR   use DIR as the temp directory",
        "      --suffix=SUFF      append SUFF to TEMPLATE",
        "      --help             display this help and exit",
        "      --version          output version information and exit"
      ]
