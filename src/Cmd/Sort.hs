{-# LANGUAGE OverloadedStrings #-}

module Cmd.Sort (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import System.Random (RandomGen, mkStdGen, randomR)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox sort: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "sort") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- sortFiles opts paths
        if hasError then exitFailure else exitSuccess

data SortMethod
  = SortLexical
  | SortNumeric
  | SortHumanNumeric
  | SortMonth
  | SortVersion
  | SortRandom
  deriving (Eq)

data Opts = Opts
  { optReverse :: !Bool,
    optSortMethod :: !SortMethod,
    optIgnoreCase :: !Bool,
    optIgnoreLeadingBlanks :: !Bool,
    optDictionaryOrder :: !Bool,
    optUnique :: !Bool,
    optStable :: !Bool,
    optCheck :: !Bool,
    optOutput :: !(Maybe FilePath),
    optFieldSep :: !(Maybe Char),
    optKey :: !(Maybe (Int, Maybe Int)),
    optZeroTerminated :: !Bool,
    optRandomSeed :: !Int,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optReverse = False,
      optSortMethod = SortLexical,
      optIgnoreCase = False,
      optIgnoreLeadingBlanks = False,
      optDictionaryOrder = False,
      optUnique = False,
      optStable = False,
      optCheck = False,
      optOutput = Nothing,
      optFieldSep = Nothing,
      optKey = Nothing,
      optZeroTerminated = False,
      optRandomSeed = 42,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--reverse" : rest) = parseArgs opts {optReverse = True} rest
parseArgs opts ("--numeric-sort" : rest) = parseArgs opts {optSortMethod = SortNumeric} rest
parseArgs opts ("--human-numeric-sort" : rest) = parseArgs opts {optSortMethod = SortHumanNumeric} rest
parseArgs opts ("--month-sort" : rest) = parseArgs opts {optSortMethod = SortMonth} rest
parseArgs opts ("--version-sort" : rest) = parseArgs opts {optSortMethod = SortVersion} rest
parseArgs opts ("--random-sort" : rest) = parseArgs opts {optSortMethod = SortRandom} rest
parseArgs opts ("--ignore-case" : rest) = parseArgs opts {optIgnoreCase = True} rest
parseArgs opts ("--ignore-leading-blanks" : rest) = parseArgs opts {optIgnoreLeadingBlanks = True} rest
parseArgs opts ("--dictionary-order" : rest) = parseArgs opts {optDictionaryOrder = True} rest
parseArgs opts ("--unique" : rest) = parseArgs opts {optUnique = True} rest
parseArgs opts ("--stable" : rest) = parseArgs opts {optStable = True} rest
parseArgs opts ("--check" : rest) = parseArgs opts {optCheck = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--output" : f : rest) = parseArgs opts {optOutput = Just f} rest
parseArgs opts ("-r" : rest) = parseArgs opts {optReverse = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optSortMethod = SortNumeric} rest
parseArgs opts ("-h" : rest) = parseArgs opts {optSortMethod = SortHumanNumeric} rest
parseArgs opts ("-M" : rest) = parseArgs opts {optSortMethod = SortMonth} rest
parseArgs opts ("-V" : rest) = parseArgs opts {optSortMethod = SortVersion} rest
parseArgs opts ("-R" : rest) = parseArgs opts {optSortMethod = SortRandom} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optIgnoreCase = True} rest
parseArgs opts ("-b" : rest) = parseArgs opts {optIgnoreLeadingBlanks = True} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optDictionaryOrder = True} rest
parseArgs opts ("-u" : rest) = parseArgs opts {optUnique = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optStable = True} rest
parseArgs opts ("-c" : rest) = parseArgs opts {optCheck = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts (('-' : 'o' : f) : rest)
  | null f = case rest of
      (file : rest') -> parseArgs opts {optOutput = Just file} rest'
      [] -> Left "option requires an argument -- 'o'"
  | otherwise = parseArgs opts {optOutput = Just f} rest
parseArgs opts (('-' : 't' : sep) : rest)
  | null sep = case rest of
      ((c : _) : rest') -> parseArgs opts {optFieldSep = Just c} rest'
      _ -> Left "option requires an argument -- 't'"
  | (c : _) <- sep = parseArgs opts {optFieldSep = Just c} rest
  | otherwise = Left "option requires an argument -- 't'"
parseArgs opts (('-' : 'k' : k) : rest)
  | null k = case rest of
      (key : rest') -> case parseKey key of
        Just kv -> parseArgs opts {optKey = Just kv} rest'
        Nothing -> Left $ "invalid key: '" ++ key ++ "'"
      [] -> Left "option requires an argument -- 'k'"
  | otherwise = case parseKey k of
      Just kv -> parseArgs opts {optKey = Just kv} rest
      Nothing -> Left $ "invalid key: '" ++ k ++ "'"
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseKey :: String -> Maybe (Int, Maybe Int)
parseKey s = case break (== ',') s of
  (start, "") | all isDigit start && not (null start) -> Just (read start, Nothing)
  (start, ',' : end)
    | all isDigit start && all isDigit end && not (null start) && not (null end) ->
        Just (read start, Just (read end))
  _ -> Nothing

sortFiles :: Opts -> [FilePath] -> IO Bool
sortFiles opts paths = do
  results <- mapM (readInputFile opts) paths
  case sequence results of
    Left err -> do
      hPutStrLn stderr $ "haskbox sort: " ++ err
      return True
    Right contents -> do
      let delim = if optZeroTerminated opts then 0 else 10
          allLines = concatMap (splitOn delim) contents
          sorted = sortLines opts allLines
          checkName = case paths of
            [] -> "stdin"
            (p : _) -> if p == "-" then "stdin" else p
      if optCheck opts
        then checkSorted opts checkName allLines
        else outputResult opts delim sorted

splitOn :: Word8 -> C8.ByteString -> [C8.ByteString]
splitOn delim bs
  | BS.null bs = []
  | otherwise = case BS.elemIndex delim bs of
      Nothing -> [bs]
      Just i -> BS.take i bs : splitOn delim (BS.drop (i + 1) bs)

readInputFile :: Opts -> FilePath -> IO (Either String C8.ByteString)
readInputFile _ path = catch (Right <$> readFile') handler
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

sortLines :: Opts -> [C8.ByteString] -> [C8.ByteString]
sortLines opts lns =
  let sorted = case optSortMethod opts of
        SortRandom -> randomShuffle (optRandomSeed opts) lns
        _ ->
          let cmp = buildComparator opts
           in sortBy cmp lns
      sorted' = if optReverse opts && optSortMethod opts /= SortRandom then reverse sorted else sorted
   in if optUnique opts then removeDups opts sorted' else sorted'

randomShuffle :: Int -> [a] -> [a]
randomShuffle seed xs = map snd $ sortBy (comparing fst) (zip (randoms seed (length xs)) xs)
  where
    randoms :: Int -> Int -> [Int]
    randoms s n = take n $ randomList (mkStdGen s)

    randomList :: (RandomGen g) => g -> [Int]
    randomList g = let (x, g') = randomR (minBound, maxBound) g in x : randomList g'

buildComparator :: Opts -> C8.ByteString -> C8.ByteString -> Ordering
buildComparator opts a b =
  let a' = preprocess opts (extractKey opts a)
      b' = preprocess opts (extractKey opts b)
   in case optSortMethod opts of
        SortNumeric -> compareNumeric a' b'
        SortHumanNumeric -> compareHumanNumeric a' b'
        SortMonth -> compareMonth a' b'
        SortVersion -> compareVersion a' b'
        SortRandom -> EQ -- Random sort doesn't use comparator
        SortLexical ->
          if optIgnoreCase opts
            then comparing (C8.map toLower) a' b'
            else compare a' b'

preprocess :: Opts -> C8.ByteString -> C8.ByteString
preprocess opts bs =
  let bs1 = if optIgnoreLeadingBlanks opts then C8.dropWhile isSpace bs else bs
      bs2 = if optDictionaryOrder opts then C8.filter (\c -> isAlphaNum c || isSpace c) bs1 else bs1
   in bs2

extractKey :: Opts -> C8.ByteString -> C8.ByteString
extractKey opts line = case optKey opts of
  Nothing -> line
  Just (start, mEnd) ->
    let sep = fromMaybe ' ' (optFieldSep opts)
        fields = C8.split sep line
        startIdx = start - 1
        endIdx = maybe (length fields - 1) (\e -> e - 1) mEnd
     in if startIdx >= length fields
          then C8.empty
          else C8.intercalate (C8.singleton sep) (take (endIdx - startIdx + 1) (drop startIdx fields))

compareNumeric :: C8.ByteString -> C8.ByteString -> Ordering
compareNumeric a b =
  let aNum = parseNumeric a
      bNum = parseNumeric b
   in compare aNum bNum

parseNumeric :: C8.ByteString -> Double
parseNumeric bs =
  let s = C8.unpack $ C8.dropWhile (== ' ') bs
   in case reads s of
        [(n, _)] -> n
        _ -> 0

-- Human-numeric sort (e.g., 1K, 2M, 3G)
compareHumanNumeric :: C8.ByteString -> C8.ByteString -> Ordering
compareHumanNumeric a b =
  let aVal = parseHumanNumeric a
      bVal = parseHumanNumeric b
   in compare aVal bVal

parseHumanNumeric :: C8.ByteString -> Double
parseHumanNumeric bs =
  let s = C8.unpack $ C8.dropWhile isSpace bs
   in case reads s of
        [(n, suffix)] -> n * multiplier (map toLower suffix)
        _ -> 0
  where
    multiplier "" = 1
    multiplier ('k' : _) = 1024
    multiplier ('m' : _) = 1024 * 1024
    multiplier ('g' : _) = 1024 * 1024 * 1024
    multiplier ('t' : _) = 1024 * 1024 * 1024 * 1024
    multiplier ('p' : _) = 1024 * 1024 * 1024 * 1024 * 1024
    multiplier ('e' : _) = 1024 * 1024 * 1024 * 1024 * 1024 * 1024
    multiplier _ = 1

-- Month sort (e.g., JAN, FEB, MAR)
compareMonth :: C8.ByteString -> C8.ByteString -> Ordering
compareMonth a b =
  let aMonth = parseMonth a
      bMonth = parseMonth b
   in compare aMonth bMonth

parseMonth :: C8.ByteString -> Int
parseMonth bs =
  let s = map toLower $ C8.unpack $ C8.dropWhile isSpace bs
      prefix = take 3 s
   in case prefix of
        "jan" -> 1
        "feb" -> 2
        "mar" -> 3
        "apr" -> 4
        "may" -> 5
        "jun" -> 6
        "jul" -> 7
        "aug" -> 8
        "sep" -> 9
        "oct" -> 10
        "nov" -> 11
        "dec" -> 12
        _ -> 0

-- Version sort (e.g., 1.2.3, 1.10.2)
compareVersion :: C8.ByteString -> C8.ByteString -> Ordering
compareVersion a b =
  let aParts = parseVersionParts (C8.unpack a)
      bParts = parseVersionParts (C8.unpack b)
   in compareVersionParts aParts bParts

data VersionPart = VNum Int | VStr String deriving (Eq)

instance Ord VersionPart where
  compare (VNum x) (VNum y) = compare x y
  compare (VStr x) (VStr y) = compare x y
  compare (VNum _) (VStr _) = LT
  compare (VStr _) (VNum _) = GT

parseVersionParts :: String -> [VersionPart]
parseVersionParts "" = []
parseVersionParts s =
  let (digits, rest1) = span isDigit s
      (nonDigits, rest2) = break isDigit rest1
   in case digits of
        "" -> case nonDigits of
          "" -> []
          _ -> VStr nonDigits : parseVersionParts rest2
        _ -> VNum (read digits) : parseVersionParts rest1

compareVersionParts :: [VersionPart] -> [VersionPart] -> Ordering
compareVersionParts [] [] = EQ
compareVersionParts [] _ = LT
compareVersionParts _ [] = GT
compareVersionParts (x : xs) (y : ys) = case compare x y of
  EQ -> compareVersionParts xs ys
  other -> other

removeDups :: Opts -> [C8.ByteString] -> [C8.ByteString]
removeDups _ [] = []
removeDups _ [x] = [x]
removeDups opts (x : y : rest)
  | linesEqual opts x y = removeDups opts (x : rest)
  | otherwise = x : removeDups opts (y : rest)

linesEqual :: Opts -> C8.ByteString -> C8.ByteString -> Bool
linesEqual opts a b
  | optIgnoreCase opts = C8.map toLower a == C8.map toLower b
  | otherwise = a == b

checkSorted :: Opts -> String -> [C8.ByteString] -> IO Bool
checkSorted opts filename = go (1 :: Int)
  where
    go _ [] = return False
    go _ [_] = return False
    go n (a : b : rest)
      | inOrder opts a b = go (n + 1) (b : rest)
      | otherwise = do
          -- GNU sort format: "sort: FILENAME:LINE: disorder: CONTENT"
          hPutStrLn stderr $ "haskbox sort: " ++ filename ++ ":" ++ show (n + 1) ++ ": disorder: " ++ C8.unpack b
          return True

    inOrder o x y =
      let cmp = buildComparator o x y
       in if optReverse o
            then cmp /= LT
            else cmp /= GT

outputResult :: Opts -> Word8 -> [C8.ByteString] -> IO Bool
outputResult opts delim lns = do
  let output = BS.intercalate (BS.singleton delim) lns <> BS.singleton delim
  case optOutput opts of
    Nothing -> BS.hPut stdout output
    Just path -> BS.writeFile path output
  return False

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox sort [OPTION]... [FILE]...",
        "Write sorted concatenation of all FILE(s) to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "Ordering options:",
        "  -b, --ignore-leading-blanks  ignore leading blanks",
        "  -d, --dictionary-order       consider only blanks and alphanumeric characters",
        "  -f, --ignore-case            fold lower case to upper case characters",
        "  -h, --human-numeric-sort     compare human readable numbers (e.g., 2K 1G)",
        "  -M, --month-sort             compare (unknown) < 'JAN' < ... < 'DEC'",
        "  -n, --numeric-sort           compare according to string numerical value",
        "  -R, --random-sort            shuffle, but group identical keys",
        "  -r, --reverse                reverse the result of comparisons",
        "  -V, --version-sort           natural sort of (version) numbers within text",
        "",
        "Other options:",
        "  -c, --check                  check for sorted input; do not sort",
        "  -k, --key=KEYDEF             sort via a key; KEYDEF gives location and type",
        "  -o, --output=FILE            write result to FILE instead of standard output",
        "  -s, --stable                 stabilize sort by disabling last-resort comparison",
        "  -t, --field-separator=SEP    use SEP instead of non-blank to blank transition",
        "  -u, --unique                 output only unique lines",
        "  -z, --zero-terminated        line delimiter is NUL, not newline",
        "      --help                   display this help and exit",
        "      --version                output version information and exit",
        "",
        "KEYDEF is F[,F] where F is a field number (origin 1)."
      ]
