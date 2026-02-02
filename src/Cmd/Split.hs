{-# LANGUAGE OverloadedStrings #-}

module Cmd.Split (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox split: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "split") >> exitSuccess
    | otherwise -> do
        let (inputFile, prefix) = case files of
              [] -> ("-", "x")
              [f] -> (f, "x")
              [f, p] -> (f, p)
              (f : p : _) -> (f, p)
        result <- splitFile opts inputFile prefix
        if result then exitFailure else exitSuccess

data Opts = Opts
  { optLines :: !(Maybe Int),
    optBytes :: !(Maybe Int),
    optNumber :: !(Maybe Int),
    optSuffixLength :: !Int,
    optNumericSuffix :: !Bool,
    optSeparator :: !Word8,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optLines = Nothing,
      optBytes = Nothing,
      optNumber = Nothing,
      optSuffixLength = 2,
      optNumericSuffix = False,
      optSeparator = 10,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--numeric-suffixes" : rest) = parseArgs opts {optNumericSuffix = True} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optNumericSuffix = True} rest
parseArgs opts ("-t" : "\\0" : rest) = parseArgs opts {optSeparator = 0} rest
parseArgs opts ("-t" : "" : rest) = parseArgs opts {optSeparator = 0} rest
parseArgs opts ("-t" : (c : _) : rest) = parseArgs opts {optSeparator = fromIntegral (fromEnum c)} rest
parseArgs opts ("--separator" : "\\0" : rest) = parseArgs opts {optSeparator = 0} rest
parseArgs opts ("--separator" : "" : rest) = parseArgs opts {optSeparator = 0} rest
parseArgs opts ("--separator" : (c : _) : rest) = parseArgs opts {optSeparator = fromIntegral (fromEnum c)} rest
parseArgs opts (('-' : 'l' : l) : rest)
  | null l = case rest of
      (num : rest') -> case readNum num of
        Just n -> parseArgs opts {optLines = Just n} rest'
        Nothing -> Left $ "invalid number of lines: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'l'"
  | otherwise = case readNum l of
      Just n -> parseArgs opts {optLines = Just n} rest
      Nothing -> Left $ "invalid number of lines: '" ++ l ++ "'"
parseArgs opts (('-' : 'b' : b) : rest)
  | null b = case rest of
      (num : rest') -> case parseSize num of
        Just n -> parseArgs opts {optBytes = Just n} rest'
        Nothing -> Left $ "invalid number of bytes: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'b'"
  | otherwise = case parseSize b of
      Just n -> parseArgs opts {optBytes = Just n} rest
      Nothing -> Left $ "invalid number of bytes: '" ++ b ++ "'"
parseArgs opts (('-' : 'n' : n) : rest)
  | null n = case rest of
      (num : rest') -> case readNum num of
        Just c -> parseArgs opts {optNumber = Just c} rest'
        Nothing -> Left $ "invalid number of chunks: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'n'"
  | otherwise = case readNum n of
      Just c -> parseArgs opts {optNumber = Just c} rest
      Nothing -> Left $ "invalid number of chunks: '" ++ n ++ "'"
parseArgs opts (('-' : 'a' : a) : rest)
  | null a = case rest of
      (num : rest') -> case readNum num of
        Just n -> parseArgs opts {optSuffixLength = n} rest'
        Nothing -> Left $ "invalid suffix length: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'a'"
  | otherwise = case readNum a of
      Just n -> parseArgs opts {optSuffixLength = n} rest
      Nothing -> Left $ "invalid suffix length: '" ++ a ++ "'"
parseArgs opts ("--lines" : l : rest) = case readNum l of
  Just n -> parseArgs opts {optLines = Just n} rest
  Nothing -> Left $ "invalid number of lines: '" ++ l ++ "'"
parseArgs opts ("--bytes" : b : rest) = case parseSize b of
  Just n -> parseArgs opts {optBytes = Just n} rest
  Nothing -> Left $ "invalid number of bytes: '" ++ b ++ "'"
parseArgs opts ("--number" : n : rest) = case readNum n of
  Just c -> parseArgs opts {optNumber = Just c} rest
  Nothing -> Left $ "invalid number of chunks: '" ++ n ++ "'"
parseArgs opts ("--suffix-length" : a : rest) = case readNum a of
  Just n -> parseArgs opts {optSuffixLength = n} rest
  Nothing -> Left $ "invalid suffix length: '" ++ a ++ "'"
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : n) : rest)
  | all isDigit n = parseArgs opts {optLines = Just (read n)} rest
  | otherwise = Left $ "invalid option -- '" ++ take 1 n ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

readNum :: String -> Maybe Int
readNum s = case reads s of
  [(n, "")] | n > 0 -> Just n
  _ -> Nothing

parseSize :: String -> Maybe Int
parseSize s =
  let (numPart, suffix) = span isDigit s
   in case reads numPart of
        [(n, "")] -> Just $ n * multiplier suffix
        _ -> Nothing
  where
    multiplier "" = 1
    multiplier "K" = 1024
    multiplier "KB" = 1000
    multiplier "M" = 1024 * 1024
    multiplier "MB" = 1000 * 1000
    multiplier "G" = 1024 * 1024 * 1024
    multiplier "GB" = 1000 * 1000 * 1000
    multiplier _ = 1

splitFile :: Opts -> FilePath -> String -> IO Bool
splitFile opts inputFile prefix = catch doSplit handler
  where
    doSplit = do
      contents <-
        if inputFile == "-"
          then BS.hGetContents stdin
          else BS.readFile inputFile

      case (optBytes opts, optLines opts, optNumber opts) of
        (Just byteCnt, _, _) -> splitByBytes opts prefix contents byteCnt
        (_, Just lineCnt, _) -> splitByLines opts prefix contents lineCnt
        (_, _, Just chunks) -> splitByChunks opts prefix contents chunks
        _ -> splitByLines opts prefix contents 1000 -- Default: 1000 lines
      return False

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox split: " ++ inputFile ++ ": " ++ friendlyError (show e)
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

splitByBytes :: Opts -> String -> BS.ByteString -> Int -> IO ()
splitByBytes opts prefix contents chunkSize = go contents 0
  where
    go bs n
      | BS.null bs = return ()
      | otherwise = do
          let (chunk, rest) = BS.splitAt chunkSize bs
              filename = prefix ++ makeSuffix opts n
          when (optVerbose opts) $ putStrLn $ "creating file '" ++ filename ++ "'"
          BS.writeFile filename chunk
          go rest (n + 1)

splitByLines :: Opts -> String -> BS.ByteString -> Int -> IO ()
splitByLines opts prefix contents lineCount = go (splitOn (optSeparator opts) contents) 0
  where
    go [] _ = return ()
    go lns n = do
      let (chunk, rest) = splitAt lineCount lns
          filename = prefix ++ makeSuffix opts n
          delim = optSeparator opts
          output = BS.intercalate (BS.singleton delim) chunk <> BS.singleton delim
      when (optVerbose opts) $ putStrLn $ "creating file '" ++ filename ++ "'"
      BS.writeFile filename output
      go rest (n + 1)

splitOn :: Word8 -> BS.ByteString -> [BS.ByteString]
splitOn delim bs
  | BS.null bs = []
  | otherwise = case BS.elemIndex delim bs of
      Nothing -> [bs]
      Just i -> BS.take i bs : splitOn delim (BS.drop (i + 1) bs)

splitByChunks :: Opts -> String -> BS.ByteString -> Int -> IO ()
splitByChunks opts prefix contents chunks = go 0
  where
    totalSize = BS.length contents
    chunkSize = (totalSize + chunks - 1) `div` chunks

    go n
      | n >= chunks = return ()
      | otherwise = do
          let start = n * chunkSize
              chunk = BS.take chunkSize (BS.drop start contents)
              filename = prefix ++ makeSuffix opts n
          when (optVerbose opts) $ putStrLn $ "creating file '" ++ filename ++ "'"
          BS.writeFile filename chunk
          go (n + 1)

makeSuffix :: Opts -> Int -> String
makeSuffix opts n
  | optNumericSuffix opts = pad (optSuffixLength opts) (show n)
  | otherwise = alphabeticSuffix (optSuffixLength opts) n
  where
    pad len s = replicate (len - length s) '0' ++ s

    alphabeticSuffix len idx =
      let chars = "abcdefghijklmnopqrstuvwxyz"
          base = length chars
          go 0 _ = []
          go l i =
            let (q, r) = i `divMod` base
             in chars !! r : go (l - 1) q
       in reverse $ take len $ go len idx ++ repeat 'a'

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox split [OPTION]... [FILE [PREFIX]]",
        "Output pieces of FILE to PREFIXaa, PREFIXab, ...",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -a, --suffix-length=N   generate suffixes of length N (default 2)",
        "  -b, --bytes=SIZE        put SIZE bytes per output file",
        "  -d, --numeric-suffixes  use numeric suffixes instead of alphabetic",
        "  -l, --lines=NUMBER      put NUMBER lines per output file",
        "  -n, --number=CHUNKS     split into CHUNKS files",
        "  -t, --separator=SEP     use SEP instead of newline as line separator",
        "      --verbose           print a diagnostic just before each file is opened",
        "      --help              display this help and exit",
        "      --version           output version information and exit",
        "",
        "SIZE may have a suffix: K (1024), M (1024^2), G (1024^3)"
      ]
