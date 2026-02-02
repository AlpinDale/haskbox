{-# LANGUAGE OverloadedStrings #-}

module Cmd.Shuf (run) where

import Control.Exception (IOException, catch)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Maybe (isJust)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import System.Random (randomRIO)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox shuf: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "shuf") >> exitSuccess
    | optEcho opts -> shuffleEcho opts files
    | isJust (optInputRange opts) -> shuffleRange opts
    | otherwise -> shuffleFiles opts files

data Opts = Opts
  { optEcho :: !Bool,
    optInputRange :: !(Maybe (Int, Int)),
    optHeadCount :: !(Maybe Int),
    optOutput :: !(Maybe FilePath),
    optZero :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optEcho = False,
      optInputRange = Nothing,
      optHeadCount = Nothing,
      optOutput = Nothing,
      optZero = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--echo" : rest) = parseArgs opts {optEcho = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("-e" : rest) = parseArgs opts {optEcho = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts (('-' : 'n' : n) : rest)
  | null n = case rest of
      (num : rest') -> case readInt num of
        Just c -> parseArgs opts {optHeadCount = Just c} rest'
        Nothing -> Left $ "invalid line count: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'n'"
  | otherwise = case readInt n of
      Just c -> parseArgs opts {optHeadCount = Just c} rest
      Nothing -> Left $ "invalid line count: '" ++ n ++ "'"
parseArgs opts (('-' : 'i' : r) : rest)
  | null r = case rest of
      (range : rest') -> case parseRange range of
        Just rng -> parseArgs opts {optInputRange = Just rng} rest'
        Nothing -> Left $ "invalid input range: '" ++ range ++ "'"
      [] -> Left "option requires an argument -- 'i'"
  | otherwise = case parseRange r of
      Just rng -> parseArgs opts {optInputRange = Just rng} rest
      Nothing -> Left $ "invalid input range: '" ++ r ++ "'"
parseArgs opts (('-' : 'o' : o) : rest)
  | null o = case rest of
      (out : rest') -> parseArgs opts {optOutput = Just out} rest'
      [] -> Left "option requires an argument -- 'o'"
  | otherwise = parseArgs opts {optOutput = Just o} rest
parseArgs opts ("--head-count" : n : rest) = case readInt n of
  Just c -> parseArgs opts {optHeadCount = Just c} rest
  Nothing -> Left $ "invalid line count: '" ++ n ++ "'"
parseArgs opts ("--input-range" : r : rest) = case parseRange r of
  Just rng -> parseArgs opts {optInputRange = Just rng} rest
  Nothing -> Left $ "invalid input range: '" ++ r ++ "'"
parseArgs opts ("--output" : o : rest) = parseArgs opts {optOutput = Just o} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

readInt :: String -> Maybe Int
readInt s = case reads s of
  [(n, "")] | n >= 0 -> Just n
  _ -> Nothing

parseRange :: String -> Maybe (Int, Int)
parseRange s = case break (== '-') s of
  (lo, '-' : hi) -> case (readInt lo, readInt hi) of
    (Just l, Just h) | l <= h -> Just (l, h)
    _ -> Nothing
  _ -> Nothing

shuffleEcho :: Opts -> [String] -> IO ()
shuffleEcho opts items = do
  shuffled <- fisherYatesShuffle items
  let result = maybe shuffled (`take` shuffled) (optHeadCount opts)
  outputLines opts (map C8.pack result)

shuffleRange :: Opts -> IO ()
shuffleRange opts = case optInputRange opts of
  Just (lo, hi) -> do
    let nums = [lo .. hi]
    shuffled <- fisherYatesShuffle nums
    let result = maybe shuffled (`take` shuffled) (optHeadCount opts)
    outputLines opts (map (C8.pack . show) result)
  Nothing -> return ()

shuffleFiles :: Opts -> [String] -> IO ()
shuffleFiles opts files = do
  let paths = if null files then ["-"] else files
  result <- readInputFiles paths
  case result of
    Left err -> do
      hPutStrLn stderr $ "haskbox shuf: " ++ err
      exitFailure
    Right contents -> do
      let delim = if optZero opts then '\0' else '\n'
          lns = concatMap (splitOn delim) contents
      shuffled <- fisherYatesShuffle lns
      let output = maybe shuffled (`take` shuffled) (optHeadCount opts)
      outputLines opts output

splitOn :: Char -> BS.ByteString -> [BS.ByteString]
splitOn c bs = filter (not . BS.null) $ C8.split c bs

readInputFiles :: [FilePath] -> IO (Either String [BS.ByteString])
readInputFiles = go []
  where
    go acc [] = return $ Right (reverse acc)
    go acc (p : ps) = do
      result <- catch (Right <$> readFile' p) handler
      case result of
        Left err -> return $ Left err
        Right content -> go (content : acc) ps

    readFile' "-" = BS.hGetContents stdin
    readFile' path = BS.readFile path

    handler :: IOException -> IO (Either String a)
    handler e = return $ Left $ friendlyError (show e)

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

outputLines :: Opts -> [BS.ByteString] -> IO ()
outputLines opts lns = do
  let delim = if optZero opts then C8.singleton '\0' else C8.singleton '\n'
      output = BS.intercalate delim lns <> delim
  case optOutput opts of
    Nothing -> BS.hPut stdout output
    Just path -> BS.writeFile path output

-- | Fisher-Yates shuffle algorithm
fisherYatesShuffle :: [a] -> IO [a]
fisherYatesShuffle [] = return []
fisherYatesShuffle xs = do
  let n = length xs
  arr <- newListArray (0, n - 1) xs
  mapM_ (shuffle arr) [n - 1, n - 2 .. 1]
  mapM (readArray arr) [0 .. n - 1]
  where
    shuffle :: IOArray Int a -> Int -> IO ()
    shuffle arr i = do
      j <- randomRIO (0, i)
      vi <- readArray arr i
      vj <- readArray arr j
      writeArray arr j vi
      writeArray arr i vj

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox shuf [OPTION]... [FILE]",
        "       haskbox shuf -e [OPTION]... [ARG]...",
        "       haskbox shuf -i LO-HI [OPTION]...",
        "Write a random permutation of the input lines to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -e, --echo                 treat each ARG as an input line",
        "  -i, --input-range=LO-HI   treat each number LO through HI as an input line",
        "  -n, --head-count=COUNT    output at most COUNT lines",
        "  -o, --output=FILE         write result to FILE instead of standard output",
        "  -z, --zero-terminated     line delimiter is NUL, not newline",
        "      --help                display this help and exit",
        "      --version             output version information and exit"
      ]
