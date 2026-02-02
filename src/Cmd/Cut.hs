{-# LANGUAGE OverloadedStrings #-}

module Cmd.Cut (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox cut: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "cut") >> exitSuccess
    | isNothing (optMode opts) -> do
        hPutStrLn stderr "haskbox cut: you must specify a list of bytes, characters, or fields"
        exitFailure
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- cutFiles opts paths
        if hasError then exitFailure else exitSuccess

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

data Opts = Opts
  { optMode :: !(Maybe CutMode),
    optDelimiter :: !Char,
    optOutputDelim :: !(Maybe String),
    optOnlyDelimited :: !Bool,
    optComplement :: !Bool,
    optZeroTerminated :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

data CutMode
  = CutBytes [Range]
  | CutChars [Range]
  | CutFields [Range]
  deriving (Show)

data Range = Single Int | FromTo Int Int | From Int | To Int
  deriving (Show)

defaultOpts :: Opts
defaultOpts =
  Opts
    { optMode = Nothing,
      optDelimiter = '\t',
      optOutputDelim = Nothing,
      optOnlyDelimited = False,
      optComplement = False,
      optZeroTerminated = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--bytes" : list : rest) = do
  ranges <- parseRanges list
  parseArgs opts {optMode = Just (CutBytes ranges)} rest
parseArgs opts ("--characters" : list : rest) = do
  ranges <- parseRanges list
  parseArgs opts {optMode = Just (CutChars ranges)} rest
parseArgs opts ("--fields" : list : rest) = do
  ranges <- parseRanges list
  parseArgs opts {optMode = Just (CutFields ranges)} rest
parseArgs opts ("--delimiter" : d : rest) = case d of
  [c] -> parseArgs opts {optDelimiter = c} rest
  _ -> Left "the delimiter must be a single character"
parseArgs opts ("--output-delimiter" : d : rest) = parseArgs opts {optOutputDelim = Just d} rest
parseArgs opts ("--only-delimited" : rest) = parseArgs opts {optOnlyDelimited = True} rest
parseArgs opts ("--complement" : rest) = parseArgs opts {optComplement = True} rest
parseArgs opts (('-' : 'b' : list) : rest) = parseShortList 'b' list opts CutBytes rest
parseArgs opts (('-' : 'c' : list) : rest) = parseShortList 'c' list opts CutChars rest
parseArgs opts (('-' : 'f' : list) : rest) = parseShortList 'f' list opts CutFields rest
parseArgs opts (('-' : 'd' : d) : rest)
  | null d = case rest of
      (delim : rest') -> case delim of
        [c] -> parseArgs opts {optDelimiter = c} rest'
        _ -> Left "the delimiter must be a single character"
      [] -> Left "option requires an argument -- 'd'"
  | otherwise = case d of
      [c] -> parseArgs opts {optDelimiter = c} rest
      _ -> Left "the delimiter must be a single character"
parseArgs opts ("-s" : rest) = parseArgs opts {optOnlyDelimited = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseShortList :: Char -> String -> Opts -> ([Range] -> CutMode) -> [String] -> Either String (Opts, [FilePath])
parseShortList c list opts mkMode rest
  | null list = case rest of
      (l : rest') -> do
        ranges <- parseRanges l
        parseArgs opts {optMode = Just (mkMode ranges)} rest'
      [] -> Left $ "option requires an argument -- '" ++ [c] ++ "'"
  | otherwise = do
      ranges <- parseRanges list
      parseArgs opts {optMode = Just (mkMode ranges)} rest

parseRanges :: String -> Either String [Range]
parseRanges s = traverse parseRange (splitOn ',' s)

parseRange :: String -> Either String Range
parseRange [] = Left "invalid byte, character or field list"
parseRange ('-' : rest)
  | all isDigit rest && not (null rest) = Right $ To (read rest)
  | otherwise = Left $ "invalid range: '-" ++ rest ++ "'"
parseRange s
  | not (null s) && last s == '-' =
      let init' = init s
       in if all isDigit init' && not (null init')
            then Right $ From (read init')
            else Left $ "invalid range: '" ++ s ++ "'"
  | '-' `elem` s =
      let (before, after') = break (== '-') s
          after = drop 1 after'
       in if all isDigit before && all isDigit after && not (null before) && not (null after)
            then Right $ FromTo (read before) (read after)
            else Left $ "invalid range: '" ++ s ++ "'"
  | all isDigit s && not (null s) = Right $ Single (read s)
  | otherwise = Left $ "invalid range: '" ++ s ++ "'"

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c str =
  let (before, after) = break (== c) str
   in before : case after of
        [] -> []
        (_ : rest) -> splitOn c rest

cutFiles :: Opts -> [FilePath] -> IO Bool
cutFiles opts = go False
  where
    go hasErr [] = return hasErr
    go hasErr (p : ps) = do
      err <- cutFile opts p
      go (hasErr || err) ps

cutFile :: Opts -> FilePath -> IO Bool
cutFile opts path = catch (cutFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox cut: " ++ path ++ ": " ++ friendlyError (show e)
      return True

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

cutFile' :: Opts -> FilePath -> IO ()
cutFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let delim = if optZeroTerminated opts then 0 else 10 :: Word8
      lns = splitOnByte delim contents
  mapM_ (processLine opts) lns

splitOnByte :: Word8 -> BS.ByteString -> [BS.ByteString]
splitOnByte delim bs
  | BS.null bs = []
  | otherwise = case BS.elemIndex delim bs of
      Nothing -> [bs]
      Just i -> BS.take i bs : splitOnByte delim (BS.drop (i + 1) bs)

processLine :: Opts -> C8.ByteString -> IO ()
processLine opts line = case optMode opts of
  Just (CutBytes ranges) -> do
    let indices = expandRanges ranges (BS.length line)
        indices' =
          if optComplement opts
            then complementIndices indices (BS.length line)
            else indices
        result = BS.pack [BS.index line (i - 1) | i <- indices', i <= BS.length line]
    outputLine opts result
  Just (CutChars ranges) -> do
    let indices = expandRanges ranges (BS.length line)
        indices' =
          if optComplement opts
            then complementIndices indices (BS.length line)
            else indices
        result = BS.pack [BS.index line (i - 1) | i <- indices', i <= BS.length line]
    outputLine opts result
  Just (CutFields ranges) -> do
    let delim = optDelimiter opts
        outDelim = fromMaybe [delim] (optOutputDelim opts)
        fields = C8.split delim line
        numFields = length fields
    if numFields == 1 && optOnlyDelimited opts
      then return ()
      else
        if numFields == 1
          then outputLine opts line
          else do
            let indices = expandRanges ranges numFields
                indices' =
                  if optComplement opts
                    then complementIndices indices numFields
                    else indices
                selected = [fields !! (i - 1) | i <- indices', i <= numFields]
                result = C8.intercalate (C8.pack outDelim) selected
            outputLine opts result
  Nothing -> return ()

outputLine :: Opts -> C8.ByteString -> IO ()
outputLine opts bs = do
  BS.hPut stdout bs
  BS.hPut stdout (BS.singleton (if optZeroTerminated opts then 0 else 10))

expandRanges :: [Range] -> Int -> [Int]
expandRanges ranges maxVal =
  let expanded = concatMap (expandRange maxVal) ranges
      sorted = sortBy (comparing id) expanded
   in dedup sorted
  where
    dedup [] = []
    dedup [x] = [x]
    dedup (x : y : xs)
      | x == y = dedup (y : xs)
      | otherwise = x : dedup (y : xs)

expandRange :: Int -> Range -> [Int]
expandRange _ (Single n) = [n]
expandRange maxVal (FromTo a b) = [a .. min b maxVal]
expandRange maxVal (From n) = [n .. maxVal]
expandRange _ (To n) = [1 .. n]

complementIndices :: [Int] -> Int -> [Int]
complementIndices selected maxVal =
  [i | i <- [1 .. maxVal], i `notElem` selected]

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox cut OPTION... [FILE]...",
        "Print selected parts of lines from each FILE to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -b, --bytes=LIST        select only these bytes",
        "  -c, --characters=LIST   select only these characters",
        "  -d, --delimiter=DELIM   use DELIM instead of TAB for field delimiter",
        "  -f, --fields=LIST       select only these fields",
        "  -s, --only-delimited    do not print lines not containing delimiters",
        "  -z, --zero-terminated   line delimiter is NUL, not newline",
        "      --output-delimiter=STRING  use STRING as output delimiter",
        "      --complement        complement the set of selected bytes/chars/fields",
        "      --help              display this help and exit",
        "      --version           output version information and exit",
        "",
        "LIST is a comma-separated list of ranges:",
        "  N       N'th byte, character or field, counted from 1",
        "  N-      from N'th byte, character or field, to end of line",
        "  N-M     from N'th to M'th byte, character or field",
        "  -M      from first to M'th byte, character or field"
      ]
