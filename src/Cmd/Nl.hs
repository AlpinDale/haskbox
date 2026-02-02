{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Nl (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox nl: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "nl") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- nlFiles opts paths
        if hasError then exitFailure else exitSuccess

data Opts = Opts
  { optBodyNumbering :: !NumberStyle,
    optHeaderNumbering :: !NumberStyle,
    optFooterNumbering :: !NumberStyle,
    optSectionDelim :: !String,
    optLineIncrement :: !Int,
    optJoinBlank :: !Int,
    optNumberFormat :: !NumFormat,
    optNumberSep :: !String,
    optNumberWidth :: !Int,
    optStartNum :: !Int,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

data NumberStyle = NumberAll | NumberNonEmpty | NumberNone | NumberRegex String
  deriving (Show)

data NumFormat = FormatRN | FormatLN | FormatRZ
  deriving (Show)

defaultOpts :: Opts
defaultOpts =
  Opts
    { optBodyNumbering = NumberNonEmpty,
      optHeaderNumbering = NumberNone,
      optFooterNumbering = NumberNone,
      optSectionDelim = "\\:",
      optLineIncrement = 1,
      optJoinBlank = 1,
      optNumberFormat = FormatRN,
      optNumberSep = "\t",
      optNumberWidth = 6,
      optStartNum = 1,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--body-numbering" : s : rest) = case parseStyle s of
  Just style -> parseArgs opts {optBodyNumbering = style} rest
  Nothing -> Left $ "invalid body numbering style: '" ++ s ++ "'"
parseArgs opts ("--header-numbering" : s : rest) = case parseStyle s of
  Just style -> parseArgs opts {optHeaderNumbering = style} rest
  Nothing -> Left $ "invalid header numbering style: '" ++ s ++ "'"
parseArgs opts ("--footer-numbering" : s : rest) = case parseStyle s of
  Just style -> parseArgs opts {optFooterNumbering = style} rest
  Nothing -> Left $ "invalid footer numbering style: '" ++ s ++ "'"
parseArgs opts ("--line-increment" : n : rest) = case parseNum n of
  Just num -> parseArgs opts {optLineIncrement = num} rest
  Nothing -> Left $ "invalid line increment: '" ++ n ++ "'"
parseArgs opts ("--number-width" : n : rest) = case parseNum n of
  Just num -> parseArgs opts {optNumberWidth = num} rest
  Nothing -> Left $ "invalid number width: '" ++ n ++ "'"
parseArgs opts ("--starting-line-number" : n : rest) = case parseNum n of
  Just num -> parseArgs opts {optStartNum = num} rest
  Nothing -> Left $ "invalid starting line number: '" ++ n ++ "'"
parseArgs opts ("--number-separator" : s : rest) = parseArgs opts {optNumberSep = s} rest
parseArgs opts (('-' : 'b' : s) : rest) = parseShortStyle 'b' s opts (\o st -> o {optBodyNumbering = st}) rest
parseArgs opts (('-' : 'h' : s) : rest) = parseShortStyle 'h' s opts (\o st -> o {optHeaderNumbering = st}) rest
parseArgs opts (('-' : 'f' : s) : rest) = parseShortStyle 'f' s opts (\o st -> o {optFooterNumbering = st}) rest
parseArgs opts (('-' : 'i' : n) : rest) = parseShortNum 'i' n opts (\o num -> o {optLineIncrement = num}) rest
parseArgs opts (('-' : 'w' : n) : rest) = parseShortNum 'w' n opts (\o num -> o {optNumberWidth = num}) rest
parseArgs opts (('-' : 'v' : n) : rest) = parseShortNum 'v' n opts (\o num -> o {optStartNum = num}) rest
parseArgs opts (('-' : 's' : s) : rest)
  | null s = case rest of
      (sep : rest') -> parseArgs opts {optNumberSep = sep} rest'
      [] -> Left "option requires an argument -- 's'"
  | otherwise = parseArgs opts {optNumberSep = s} rest
parseArgs opts (('-' : 'n' : f) : rest) = parseShortFormat f opts rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseShortStyle :: Char -> String -> Opts -> (Opts -> NumberStyle -> Opts) -> [String] -> Either String (Opts, [FilePath])
parseShortStyle c s opts setter rest
  | null s = case rest of
      (style : rest') -> case parseStyle style of
        Just st -> parseArgs (setter opts st) rest'
        Nothing -> Left $ "invalid numbering style: '" ++ style ++ "'"
      [] -> Left $ "option requires an argument -- '" ++ [c] ++ "'"
  | otherwise = case parseStyle s of
      Just st -> parseArgs (setter opts st) rest
      Nothing -> Left $ "invalid numbering style: '" ++ s ++ "'"

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

parseShortFormat :: String -> Opts -> [String] -> Either String (Opts, [FilePath])
parseShortFormat f opts rest
  | null f = case rest of
      (fmt : rest') -> case parseFormat fmt of
        Just nf -> parseArgs opts {optNumberFormat = nf} rest'
        Nothing -> Left $ "invalid number format: '" ++ fmt ++ "'"
      [] -> Left "option requires an argument -- 'n'"
  | otherwise = case parseFormat f of
      Just nf -> parseArgs opts {optNumberFormat = nf} rest
      Nothing -> Left $ "invalid number format: '" ++ f ++ "'"

parseStyle :: String -> Maybe NumberStyle
parseStyle "a" = Just NumberAll
parseStyle "t" = Just NumberNonEmpty
parseStyle "n" = Just NumberNone
parseStyle ('p' : regex) = Just (NumberRegex regex)
parseStyle _ = Nothing

parseFormat :: String -> Maybe NumFormat
parseFormat "ln" = Just FormatLN
parseFormat "rn" = Just FormatRN
parseFormat "rz" = Just FormatRZ
parseFormat _ = Nothing

parseNum :: String -> Maybe Int
parseNum s
  | all isDigit s && not (null s) = Just (read s)
  | otherwise = Nothing

nlFiles :: Opts -> [FilePath] -> IO Bool
nlFiles opts = go (optStartNum opts) False
  where
    go _ hasErr [] = return hasErr
    go !lineNum !hasErr (p : ps) = do
      result <- nlFile opts lineNum p
      case result of
        Left err -> do
          hPutStrLn stderr $ "haskbox nl: " ++ p ++ ": " ++ err
          go lineNum True ps
        Right newLineNum -> go newLineNum hasErr ps

nlFile :: Opts -> Int -> FilePath -> IO (Either String Int)
nlFile opts startNum path = catch (nlFile' opts startNum path) handler
  where
    handler :: IOException -> IO (Either String Int)
    handler e = return $ Left $ friendlyError (show e)

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

nlFile' :: Opts -> Int -> FilePath -> IO (Either String Int)
nlFile' opts startNum path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let lns = C8.lines contents
      style = optBodyNumbering opts
  finalNum <- processLines opts style startNum lns
  return $ Right finalNum

processLines :: Opts -> NumberStyle -> Int -> [C8.ByteString] -> IO Int
processLines _ _ lineNum [] = return lineNum
processLines opts style lineNum (line : rest) = do
  let shouldNumber = case style of
        NumberAll -> True
        NumberNonEmpty -> not (BS.null line)
        NumberNone -> False
        NumberRegex _ -> not (BS.null line)
      (output, newNum)
        | shouldNumber =
            let numStr = formatNumber opts lineNum
             in (numStr <> C8.pack (optNumberSep opts) <> line, lineNum + optLineIncrement opts)
        | otherwise =
            -- GNU nl: unnumbered lines get (width + separator_length) spaces, not spaces + tab
            let noLinePrefix = C8.replicate (optNumberWidth opts + length (optNumberSep opts)) ' '
             in (noLinePrefix <> line, lineNum)
  C8.hPutStrLn stdout output
  processLines opts style newNum rest

formatNumber :: Opts -> Int -> C8.ByteString
formatNumber opts n =
  let w = optNumberWidth opts
      s = show n
      len = length s
   in case optNumberFormat opts of
        FormatRN -> C8.pack $ replicate (w - len) ' ' ++ s
        FormatLN -> C8.pack $ s ++ replicate (w - len) ' '
        FormatRZ -> C8.pack $ replicate (w - len) '0' ++ s

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox nl [OPTION]... [FILE]...",
        "Write each FILE to standard output, with line numbers added.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -b, --body-numbering=STYLE       use STYLE for numbering body lines",
        "  -h, --header-numbering=STYLE     use STYLE for numbering header lines",
        "  -f, --footer-numbering=STYLE     use STYLE for numbering footer lines",
        "  -i, --line-increment=NUMBER      line number increment",
        "  -n, --number-format=FORMAT       insert line numbers according to FORMAT",
        "  -s, --number-separator=STRING    add STRING after line number",
        "  -v, --starting-line-number=NUMBER  first line number on each page",
        "  -w, --number-width=NUMBER        use NUMBER columns for line numbers",
        "      --help                       display this help and exit",
        "      --version                    output version information and exit",
        "",
        "STYLE is one of:",
        "  a       number all lines",
        "  t       number only nonempty lines",
        "  n       number no lines",
        "  pBRE    number only lines matching basic regular expression BRE",
        "",
        "FORMAT is one of:",
        "  ln      left justified, no leading zeros",
        "  rn      right justified, no leading zeros",
        "  rz      right justified, leading zeros"
      ]
