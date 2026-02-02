{-# LANGUAGE OverloadedStrings #-}

module Cmd.Paste (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox paste: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "paste") >> exitSuccess
    | null files -> do
        hPutStrLn stderr "haskbox paste: missing operand"
        hPutStrLn stderr "Try 'haskbox paste --help' for more information."
        exitFailure
    | otherwise -> do
        hasError <- pasteFiles opts files
        if hasError then exitFailure else exitSuccess

data Opts = Opts
  { optDelimiters :: !String,
    optSerial :: !Bool,
    optZeroTerminated :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optDelimiters = "\t",
      optSerial = False,
      optZeroTerminated = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--serial" : rest) = parseArgs opts {optSerial = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--delimiters" : d : rest) = parseArgs opts {optDelimiters = expandDelims d} rest
parseArgs opts (('-' : 'd' : d) : rest)
  | null d = case rest of
      (delim : rest') -> parseArgs opts {optDelimiters = expandDelims delim} rest'
      [] -> Left "option requires an argument -- 'd'"
  | otherwise = parseArgs opts {optDelimiters = expandDelims d} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optSerial = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

expandDelims :: String -> String
expandDelims [] = []
expandDelims ('\\' : 'n' : rest) = '\n' : expandDelims rest
expandDelims ('\\' : 't' : rest) = '\t' : expandDelims rest
expandDelims ('\\' : '\\' : rest) = '\\' : expandDelims rest
expandDelims ('\\' : '0' : rest) = '\0' : expandDelims rest
expandDelims (c : rest) = c : expandDelims rest

pasteFiles :: Opts -> [FilePath] -> IO Bool
pasteFiles opts files
  | optSerial opts = pasteSerial opts files
  | otherwise = pasteParallel opts files

pasteSerial :: Opts -> [FilePath] -> IO Bool
pasteSerial opts = go False
  where
    go hasErr [] = return hasErr
    go hasErr (f : fs) = do
      result <- readInputFile f
      case result of
        Left err -> do
          hPutStrLn stderr $ "haskbox paste: " ++ err
          go True fs
        Right contents -> do
          let lns = splitLines opts contents
              delims = cycle (optDelimiters opts)
              merged = mergeWithDelims delims lns
          C8.hPutStrLn stdout merged
          go hasErr fs

    mergeWithDelims _ [] = C8.empty
    mergeWithDelims _ [x] = x
    mergeWithDelims (d : ds) (x : xs) = x <> C8.singleton d <> mergeWithDelims ds xs
    mergeWithDelims [] xs = mergeWithDelims (optDelimiters opts) xs

pasteParallel :: Opts -> [FilePath] -> IO Bool
pasteParallel opts files = do
  results <- mapM readInputFile files
  case sequence results of
    Left err -> do
      hPutStrLn stderr $ "haskbox paste: " ++ err
      return True
    Right contents -> do
      let linesList = map (splitLines opts) contents
          maxLines = maximum (map length linesList)
          padded = map (padToLength maxLines) linesList
          zipped = transpose' padded
          delims = optDelimiters opts
      mapM_ (outputLine delims) zipped
      return False
  where
    padToLength n xs = xs ++ replicate (n - length xs) C8.empty

    transpose' [] = []
    transpose' xss
      | all null xss = []
      | otherwise = map headOrEmpty xss : transpose' (map tailOrEmpty xss)

    headOrEmpty [] = C8.empty
    headOrEmpty (x : _) = x

    tailOrEmpty [] = []
    tailOrEmpty (_ : xs) = xs

    outputLine delims lns = do
      let merged = mergeWithDelims (cycle delims) lns
      C8.hPutStrLn stdout merged

    mergeWithDelims _ [] = C8.empty
    mergeWithDelims _ [x] = x
    mergeWithDelims (d : ds) (x : xs) = x <> C8.singleton d <> mergeWithDelims ds xs
    mergeWithDelims [] xs = mergeWithDelims (optDelimiters opts) xs

splitLines :: Opts -> C8.ByteString -> [C8.ByteString]
splitLines opts bs
  | optZeroTerminated opts = C8.split '\0' bs
  | otherwise = C8.lines bs

readInputFile :: FilePath -> IO (Either String C8.ByteString)
readInputFile path = catch (Right <$> readFile') handler
  where
    readFile' =
      if path == "-"
        then BS.hGetContents stdin
        else BS.readFile path

    handler :: IOException -> IO (Either String C8.ByteString)
    handler e = return $ Left $ path ++ ": " ++ friendlyError (show e)

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s || "inappropriate type" `isIn` s = "Is a directory"
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
      [ "Usage: haskbox paste [OPTION]... [FILE]...",
        "Write lines consisting of sequentially corresponding lines from each FILE,",
        "separated by TABs, to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -d, --delimiters=LIST  use characters from LIST instead of TABs",
        "  -s, --serial           paste one file at a time instead of in parallel",
        "  -z, --zero-terminated  line delimiter is NUL, not newline",
        "      --help             display this help and exit",
        "      --version          output version information and exit"
      ]
