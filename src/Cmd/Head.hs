{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Head (run) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox head: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "head") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
            showHeaders = optVerbose opts || (not (optQuiet opts) && length paths > 1)
        hasError <- headFiles opts showHeaders paths
        when hasError exitFailure

data Opts = Opts
  { optLines :: !Int,
    optLinesFromEnd :: !Bool, -- True if -n -NUM (all but last NUM)
    optBytes :: !(Maybe Int),
    optBytesFromEnd :: !Bool, -- True if -c -NUM (all but last NUM)
    optQuiet :: !Bool,
    optVerbose :: !Bool,
    optZeroTerminated :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optLines = 10,
      optLinesFromEnd = False,
      optBytes = Nothing,
      optBytesFromEnd = False,
      optQuiet = False,
      optVerbose = False,
      optZeroTerminated = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--silent" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--lines" : n : rest) = case parseNumSigned n of
  Just (num, fromEnd) -> parseArgs opts {optLines = num, optLinesFromEnd = fromEnd} rest
  Nothing -> Left $ "invalid number of lines: '" ++ n ++ "'"
parseArgs opts ("--bytes" : n : rest) = case parseNumSigned n of
  Just (num, fromEnd) -> parseArgs opts {optBytes = Just num, optBytesFromEnd = fromEnd} rest
  Nothing -> Left $ "invalid number of bytes: '" ++ n ++ "'"
parseArgs opts (('-' : 'n' : n) : rest)
  | null n = case rest of
      (num : rest') -> case parseNumSigned num of
        Just (x, fromEnd) -> parseArgs opts {optLines = x, optLinesFromEnd = fromEnd} rest'
        Nothing -> Left $ "invalid number of lines: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'n'"
  | otherwise = case parseNumSigned n of
      Just (num, fromEnd) -> parseArgs opts {optLines = num, optLinesFromEnd = fromEnd} rest
      Nothing -> Left $ "invalid number of lines: '" ++ n ++ "'"
parseArgs opts (('-' : 'c' : n) : rest)
  | null n = case rest of
      (num : rest') -> case parseNumSigned num of
        Just (x, fromEnd) -> parseArgs opts {optBytes = Just x, optBytesFromEnd = fromEnd} rest'
        Nothing -> Left $ "invalid number of bytes: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'c'"
  | otherwise = case parseNumSigned n of
      Just (num, fromEnd) -> parseArgs opts {optBytes = Just num, optBytesFromEnd = fromEnd} rest
      Nothing -> Left $ "invalid number of bytes: '" ++ n ++ "'"
parseArgs opts ("-q" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

-- Parse a number that may have a leading - (meaning "from end")
parseNumSigned :: String -> Maybe (Int, Bool)
parseNumSigned ('-' : s)
  | all isDigit s && not (null s) = Just (read s, True)
  | otherwise = Nothing
parseNumSigned s
  | all isDigit s && not (null s) = Just (read s, False)
  | otherwise = Nothing

headFiles :: Opts -> Bool -> [FilePath] -> IO Bool
headFiles opts showHeaders paths = do
  let go _ [] = return False
      go !isFirst (p : ps) = do
        when showHeaders $ do
          unless isFirst $ C8.hPut stdout "\n"
          C8.hPut stdout $ "==> " <> C8.pack p <> " <==\n"
        err <- headFile opts p
        errRest <- go False ps
        return (err || errRest)
  go True paths

headFile :: Opts -> FilePath -> IO Bool
headFile opts path = catch (headFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox head: " ++ path ++ ": " ++ friendlyError (show e)
      return True

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

headFile' :: Opts -> FilePath -> IO ()
headFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let delim = if optZeroTerminated opts then 0 else 10
  case optBytes opts of
    Just n
      | optBytesFromEnd opts -> BS.hPut stdout (dropLastBytes n contents)
      | otherwise -> BS.hPut stdout (BS.take n contents)
    Nothing
      | optLinesFromEnd opts -> BS.hPut stdout (dropLastLines delim (optLines opts) contents)
      | otherwise -> do
          let !n = optLines opts
              lines' = takeLines delim n contents
          BS.hPut stdout lines'

takeLines :: Word8 -> Int -> BS.ByteString -> BS.ByteString
takeLines delim n bs = go n 0 bs
  where
    go 0 !pos _ = BS.take pos bs
    go !remaining !pos rest
      | BS.null rest = bs
      | otherwise = case BS.elemIndex delim rest of
          Nothing -> bs
          Just i -> go (remaining - 1) (pos + i + 1) (BS.drop (i + 1) rest)

-- Drop the last n lines from content
dropLastLines :: Word8 -> Int -> BS.ByteString -> BS.ByteString
dropLastLines delim n bs =
  let allLines = splitOn delim bs
      numToKeep = max 0 (length allLines - n)
      kept = take numToKeep allLines
   in if null kept
        then BS.empty
        else BS.intercalate (BS.singleton delim) kept <> BS.singleton delim

splitOn :: Word8 -> BS.ByteString -> [BS.ByteString]
splitOn delim bs
  | BS.null bs = []
  | otherwise = case BS.elemIndex delim bs of
      Nothing -> [bs]
      Just i -> BS.take i bs : splitOn delim (BS.drop (i + 1) bs)

-- Drop the last n bytes from content
dropLastBytes :: Int -> BS.ByteString -> BS.ByteString
dropLastBytes n bs = BS.take (max 0 (BS.length bs - n)) bs

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox head [OPTION]... [FILE]...",
        "Print the first 10 lines of each FILE to standard output.",
        "With more than one FILE, precede each with a header giving the file name.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -c, --bytes=NUM        print the first NUM bytes of each file",
        "  -n, --lines=NUM        print the first NUM lines instead of the first 10",
        "  -q, --quiet, --silent  never print headers giving file names",
        "  -v, --verbose          always print headers giving file names",
        "  -z, --zero-terminated  line delimiter is NUL, not newline",
        "      --help             display this help and exit",
        "      --version          output version information and exit"
      ]
