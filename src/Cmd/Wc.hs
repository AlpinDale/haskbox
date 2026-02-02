{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Wc (run) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox wc: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "wc") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
            opts' = normalizeOpts opts
        (totals, hasError) <- wcFiles opts' paths
        when (length paths > 1) $ printCounts opts' totals "total"
        when hasError exitFailure

data Opts = Opts
  { optCountLines :: !Bool,
    optCountWords :: !Bool,
    optCountBytes :: !Bool,
    optCountChars :: !Bool,
    optMaxLineLen :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

data Counts = Counts
  { cntLines :: !Int,
    cntWords :: !Int,
    cntBytes :: !Int,
    cntMaxLen :: !Int
  }
  deriving (Show)

defaultOpts :: Opts
defaultOpts =
  Opts
    { optCountLines = False,
      optCountWords = False,
      optCountBytes = False,
      optCountChars = False,
      optMaxLineLen = False,
      optShowHelp = False,
      optShowVersion = False
    }

zeroCounts :: Counts
zeroCounts = Counts 0 0 0 0

addCounts :: Counts -> Counts -> Counts
addCounts a b =
  Counts
    { cntLines = cntLines a + cntLines b,
      cntWords = cntWords a + cntWords b,
      cntBytes = cntBytes a + cntBytes b,
      cntMaxLen = max (cntMaxLen a) (cntMaxLen b)
    }

normalizeOpts :: Opts -> Opts
normalizeOpts opts
  | not (optCountLines opts || optCountWords opts || optCountBytes opts || optCountChars opts || optMaxLineLen opts) =
      opts {optCountLines = True, optCountWords = True, optCountBytes = True}
  | otherwise = opts

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--lines" : rest) = parseArgs opts {optCountLines = True} rest
parseArgs opts ("--words" : rest) = parseArgs opts {optCountWords = True} rest
parseArgs opts ("--bytes" : rest) = parseArgs opts {optCountBytes = True} rest
parseArgs opts ("--chars" : rest) = parseArgs opts {optCountChars = True} rest
parseArgs opts ("--max-line-length" : rest) = parseArgs opts {optMaxLineLen = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : shorts) : rest)
  | null shorts = do
      (o, files) <- parseArgs opts rest
      Right (o, "-" : files)
  | all (`elem` ("lwcmL" :: String)) shorts = parseArgs (applyShorts opts shorts) rest
  | (c : _) <- filter (`notElem` ("lwcmL" :: String)) shorts =
      Left $ "invalid option -- '" ++ [c] ++ "'"
  | otherwise = parseArgs opts rest
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

applyShorts :: Opts -> String -> Opts
applyShorts opts [] = opts
applyShorts opts (c : cs) = applyShorts (applyShort opts c) cs
  where
    applyShort o 'l' = o {optCountLines = True}
    applyShort o 'w' = o {optCountWords = True}
    applyShort o 'c' = o {optCountBytes = True}
    applyShort o 'm' = o {optCountChars = True}
    applyShort o 'L' = o {optMaxLineLen = True}
    applyShort o _ = o

wcFiles :: Opts -> [FilePath] -> IO (Counts, Bool)
wcFiles opts = go zeroCounts False
  where
    go !totals !hasErr [] = return (totals, hasErr)
    go !totals !hasErr (p : ps) = do
      (counts, err) <- wcFile opts p
      printCounts opts counts p
      go (addCounts totals counts) (hasErr || err) ps

wcFile :: Opts -> FilePath -> IO (Counts, Bool)
wcFile opts path = catch (wcFile' opts path >>= \c -> return (c, False)) handler
  where
    handler :: IOException -> IO (Counts, Bool)
    handler e = do
      hPutStrLn stderr $ "haskbox wc: " ++ path ++ ": " ++ friendlyError (show e)
      return (zeroCounts, True)

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

wcFile' :: Opts -> FilePath -> IO Counts
wcFile' _ path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  return $! countBytes contents

countBytes :: BS.ByteString -> Counts
countBytes bs = go 0 0 0 False 0 0
  where
    !len = BS.length bs
    go !pos !lines' !words' !inWord !lineLen !maxLen
      | pos >= len =
          let !finalWords = if inWord then words' + 1 else words'
              !finalMaxLen = max maxLen lineLen
           in Counts lines' finalWords len finalMaxLen
      | otherwise =
          let !b = BS.index bs pos
              !isSpace = isWhitespace b
              !isNewline = b == 10
              !newInWord = not isSpace
              !wordInc = if inWord && isSpace then 1 else 0
              !lineInc = if isNewline then 1 else 0
              !newLineLen = if isNewline then 0 else lineLen + 1
              !newMaxLen = if isNewline then max maxLen lineLen else maxLen
           in go (pos + 1) (lines' + lineInc) (words' + wordInc) newInWord newLineLen newMaxLen

isWhitespace :: Word8 -> Bool
isWhitespace b = b == 32 || b == 9 || b == 10 || b == 13 || b == 11 || b == 12

printCounts :: Opts -> Counts -> String -> IO ()
printCounts opts counts name = do
  let fields =
        concat
          [ [show (cntLines counts) | optCountLines opts],
            [show (cntWords counts) | optCountWords opts],
            [show (cntBytes counts) | optCountBytes opts || optCountChars opts],
            [show (cntMaxLen counts) | optMaxLineLen opts]
          ]
      suffix = if name == "-" then "" else " " ++ name
      formatted = formatFields fields
  putStrLn $ formatted ++ suffix
  where
    formatFields [] = ""
    formatFields [x] = x -- Single field: no padding (GNU wc behavior)
    formatFields fs =
      -- GNU wc: first field no leading space, subsequent fields have leading space
      -- Minimum width is 1, but typically computed from file sizes
      -- For simplicity, use the max length of any field as width (like GNU does for small files)
      let width = maximum (map length fs)
       in concatMap (\(i, s) -> if i == (0 :: Int) then padLeft width s else ' ' : padLeft width s) (zip [0 ..] fs)
    padLeft n s = replicate (n - length s) ' ' ++ s

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox wc [OPTION]... [FILE]...",
        "Print newline, word, and byte counts for each FILE, and a total line if",
        "more than one FILE is specified.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -c, --bytes            print the byte counts",
        "  -m, --chars            print the character counts",
        "  -l, --lines            print the newline counts",
        "  -L, --max-line-length  print the maximum display width",
        "  -w, --words            print the word counts",
        "      --help             display this help and exit",
        "      --version          output version information and exit"
      ]
