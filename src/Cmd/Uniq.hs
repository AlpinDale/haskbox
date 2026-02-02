{-# LANGUAGE OverloadedStrings #-}

module Cmd.Uniq (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit, toLower)
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox uniq: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "uniq") >> exitSuccess
    | otherwise -> do
        let (input, output) = case files of
              [] -> ("-", Nothing)
              [f] -> (f, Nothing)
              [f, o] -> (f, Just o)
              _ -> ("-", Nothing)
        success <- uniqFile opts input output
        if success then exitSuccess else exitFailure

data Opts = Opts
  { optCount :: !Bool,
    optRepeated :: !Bool,
    optUnique :: !Bool,
    optIgnoreCase :: !Bool,
    optSkipFields :: !Int,
    optSkipChars :: !Int,
    optCheckChars :: !(Maybe Int),
    optZeroTerminated :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optCount = False,
      optRepeated = False,
      optUnique = False,
      optIgnoreCase = False,
      optSkipFields = 0,
      optSkipChars = 0,
      optCheckChars = Nothing,
      optZeroTerminated = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--count" : rest) = parseArgs opts {optCount = True} rest
parseArgs opts ("--repeated" : rest) = parseArgs opts {optRepeated = True} rest
parseArgs opts ("--unique" : rest) = parseArgs opts {optUnique = True} rest
parseArgs opts ("--ignore-case" : rest) = parseArgs opts {optIgnoreCase = True} rest
parseArgs opts ("--skip-fields" : n : rest) = case parseNum n of
  Just num -> parseArgs opts {optSkipFields = num} rest
  Nothing -> Left $ "invalid number of fields to skip: '" ++ n ++ "'"
parseArgs opts ("--skip-chars" : n : rest) = case parseNum n of
  Just num -> parseArgs opts {optSkipChars = num} rest
  Nothing -> Left $ "invalid number of characters to skip: '" ++ n ++ "'"
parseArgs opts ("--check-chars" : n : rest) = case parseNum n of
  Just num -> parseArgs opts {optCheckChars = Just num} rest
  Nothing -> Left $ "invalid number of characters to check: '" ++ n ++ "'"
parseArgs opts ("-c" : rest) = parseArgs opts {optCount = True} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optRepeated = True} rest
parseArgs opts ("-u" : rest) = parseArgs opts {optUnique = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optIgnoreCase = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts (('-' : 'f' : n) : rest) = parseShortNum 'f' n opts (\o num -> o {optSkipFields = num}) rest
parseArgs opts (('-' : 's' : n) : rest) = parseShortNum 's' n opts (\o num -> o {optSkipChars = num}) rest
parseArgs opts (('-' : 'w' : n) : rest) = parseShortNum 'w' n opts (\o num -> o {optCheckChars = Just num}) rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseShortNum :: Char -> String -> Opts -> (Opts -> Int -> Opts) -> [String] -> Either String (Opts, [FilePath])
parseShortNum c n opts setter rest
  | null n = case rest of
      (num : rest') -> case parseNum num of
        Just x -> parseArgs (setter opts x) rest'
        Nothing -> Left $ "invalid number: '" ++ num ++ "'"
      [] -> Left $ "option requires an argument -- '" ++ [c] ++ "'"
  | otherwise = case parseNum n of
      Just x -> parseArgs (setter opts x) rest
      Nothing -> Left $ "invalid number: '" ++ n ++ "'"

parseNum :: String -> Maybe Int
parseNum s
  | all isDigit s && not (null s) = Just (read s)
  | otherwise = Nothing

uniqFile :: Opts -> FilePath -> Maybe FilePath -> IO Bool
uniqFile opts input output = catch (uniqFile' opts input output >> return True) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox uniq: " ++ friendlyError (show e)
      return False

    friendlyError s
      | "does not exist" `isIn` s = input ++ ": No such file or directory"
      | "Permission denied" `isIn` s = input ++ ": Permission denied"
      | "is a directory" `isIn` s || "inappropriate type" `isIn` s = input ++ ": Is a directory"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

uniqFile' :: Opts -> FilePath -> Maybe FilePath -> IO ()
uniqFile' opts input output = do
  contents <-
    if input == "-"
      then BS.hGetContents stdin
      else BS.readFile input
  let delim = if optZeroTerminated opts then 0 else 10 :: Word8
      lns = splitOn delim contents
      result = processLines opts lns
      outputBs = BS.intercalate (BS.singleton delim) result <> if null result then BS.empty else BS.singleton delim
  case output of
    Nothing -> BS.hPut stdout outputBs
    Just outPath -> BS.writeFile outPath outputBs

splitOn :: Word8 -> BS.ByteString -> [BS.ByteString]
splitOn delim bs
  | BS.null bs = []
  | otherwise = case BS.elemIndex delim bs of
      Nothing -> [bs]
      Just i -> BS.take i bs : splitOn delim (BS.drop (i + 1) bs)

processLines :: Opts -> [C8.ByteString] -> [C8.ByteString]
processLines opts = go Nothing (0 :: Int)
  where
    go Nothing _ [] = []
    go Nothing _ (l : ls) = go (Just l) 1 ls
    go (Just prev) count [] = outputLine opts prev count
    go (Just prev) count (l : ls)
      | linesEqual opts prev l = go (Just prev) (count + 1) ls
      | otherwise = outputLine opts prev count ++ go (Just l) 1 ls

    outputLine o line count
      | optRepeated o && count < 2 = []
      | optUnique o && count > 1 = []
      | optCount o = [C8.pack (padCount count) <> " " <> line]
      | otherwise = [line]

    padCount n =
      let s = show n
       in replicate (7 - length s) ' ' ++ s

linesEqual :: Opts -> C8.ByteString -> C8.ByteString -> Bool
linesEqual opts a b =
  let a' = prepareForCompare opts a
      b' = prepareForCompare opts b
   in if optIgnoreCase opts
        then C8.map toLower a' == C8.map toLower b'
        else a' == b'

prepareForCompare :: Opts -> C8.ByteString -> C8.ByteString
prepareForCompare opts line =
  let afterFields = skipFields (optSkipFields opts) line
      afterChars = BS.drop (optSkipChars opts) afterFields
   in case optCheckChars opts of
        Nothing -> afterChars
        Just n -> BS.take n afterChars

skipFields :: Int -> C8.ByteString -> C8.ByteString
skipFields 0 bs = bs
skipFields n bs =
  let stripped = C8.dropWhile (== ' ') bs
      afterField = C8.dropWhile (/= ' ') stripped
   in skipFields (n - 1) afterField

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox uniq [OPTION]... [INPUT [OUTPUT]]",
        "Filter adjacent matching lines from INPUT (or standard input),",
        "writing to OUTPUT (or standard output).",
        "",
        "  -c, --count           prefix lines by the number of occurrences",
        "  -d, --repeated        only print duplicate lines, one for each group",
        "  -f, --skip-fields=N   avoid comparing the first N fields",
        "  -i, --ignore-case     ignore differences in case when comparing",
        "  -s, --skip-chars=N    avoid comparing the first N characters",
        "  -u, --unique          only print unique lines",
        "  -w, --check-chars=N   compare no more than N characters in lines",
        "  -z, --zero-terminated line delimiter is NUL, not newline",
        "      --help            display this help and exit",
        "      --version         output version information and exit"
      ]
