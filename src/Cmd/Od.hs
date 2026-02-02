{-# LANGUAGE OverloadedStrings #-}

module Cmd.Od (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Word (Word8)
import Numeric (showHex, showOct)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox od: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "od") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        result <- dumpFiles opts paths
        if result then exitFailure else exitSuccess

data Opts = Opts
  { optFormat :: !Format,
    optAddressRadix :: !AddressRadix,
    optWidth :: !Int,
    optSkip :: !Int,
    optCount :: !(Maybe Int),
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

data Format = OctalByte | OctalWord | HexWord | DecimalWord | Char | Named
  deriving (Eq)

data AddressRadix = AddrOctal | AddrDecimal | AddrHex | AddrNone
  deriving (Eq)

defaultOpts :: Opts
defaultOpts =
  Opts
    { optFormat = OctalWord, -- Default is octal 2-byte units
      optAddressRadix = AddrOctal, -- Default is octal addresses
      optWidth = 16,
      optSkip = 0,
      optCount = Nothing,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("-A" : "x" : rest) = parseArgs opts {optAddressRadix = AddrHex} rest
parseArgs opts ("-A" : "d" : rest) = parseArgs opts {optAddressRadix = AddrDecimal} rest
parseArgs opts ("-A" : "o" : rest) = parseArgs opts {optAddressRadix = AddrOctal} rest
parseArgs opts ("-A" : "n" : rest) = parseArgs opts {optAddressRadix = AddrNone} rest
parseArgs _ ("-A" : r : _) = Left $ "invalid output address radix '" ++ r ++ "'"
parseArgs _ ["-A"] = Left "option requires an argument -- 'A'"
parseArgs opts ("-b" : rest) = parseArgs opts {optFormat = OctalByte} rest
parseArgs opts ("-c" : rest) = parseArgs opts {optFormat = Char} rest
parseArgs opts ("-d" : rest) = parseArgs opts {optFormat = DecimalWord} rest
parseArgs opts ("-x" : rest) = parseArgs opts {optFormat = HexWord} rest
parseArgs opts ("-o" : rest) = parseArgs opts {optFormat = OctalWord} rest
parseArgs opts (('-' : 'w' : w) : rest)
  | null w = case rest of
      (num : rest') -> case reads num of
        [(n, "")] -> parseArgs opts {optWidth = n} rest'
        _ -> Left $ "invalid width: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'w'"
  | otherwise = case reads w of
      [(n, "")] -> parseArgs opts {optWidth = n} rest
      _ -> Left $ "invalid width: '" ++ w ++ "'"
parseArgs opts (('-' : 'j' : j) : rest)
  | null j = case rest of
      (num : rest') -> case reads num of
        [(n, "")] -> parseArgs opts {optSkip = n} rest'
        _ -> Left $ "invalid skip: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'j'"
  | otherwise = case reads j of
      [(n, "")] -> parseArgs opts {optSkip = n} rest
      _ -> Left $ "invalid skip: '" ++ j ++ "'"
parseArgs opts (('-' : 'N' : n) : rest)
  | null n = case rest of
      (num : rest') -> case reads num of
        [(c, "")] -> parseArgs opts {optCount = Just c} rest'
        _ -> Left $ "invalid count: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'N'"
  | otherwise = case reads n of
      [(c, "")] -> parseArgs opts {optCount = Just c} rest
      _ -> Left $ "invalid count: '" ++ n ++ "'"
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

dumpFiles :: Opts -> [FilePath] -> IO Bool
dumpFiles opts paths = do
  result <- readAllFiles paths
  case result of
    Left err -> do
      hPutStrLn stderr $ "haskbox od: " ++ err
      return True
    Right contents -> do
      let skipped = BS.drop (optSkip opts) contents
          limited = case optCount opts of
            Just n -> BS.take n skipped
            Nothing -> skipped
      dumpContents opts limited
      return False

readAllFiles :: [FilePath] -> IO (Either String BS.ByteString)
readAllFiles paths = catch (Right . BS.concat <$> mapM readOne paths) handler
  where
    readOne "-" = BS.hGetContents stdin
    readOne path = BS.readFile path

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

dumpContents :: Opts -> BS.ByteString -> IO ()
dumpContents opts = go 0
  where
    width = optWidth opts
    fmt = optFormat opts
    addrRadix = optAddressRadix opts

    go offset remaining
      | BS.null remaining = printFinalOffset offset
      | otherwise = do
          let (line, rest) = BS.splitAt width remaining
          printOffset offset
          printLine fmt line
          go (offset + BS.length line) rest

    printOffset n = case addrRadix of
      AddrOctal -> putStr $ padLeft 7 (showOct n "") ++ " "
      AddrDecimal -> putStr $ padLeft 7 (show n) ++ " "
      AddrHex -> putStr $ padLeft 6 (showHex n "") ++ " "
      AddrNone -> putStr " "

    printFinalOffset n = case addrRadix of
      AddrOctal -> putStrLn $ padLeft 7 (showOct n "")
      AddrDecimal -> putStrLn $ padLeft 7 (show n)
      AddrHex -> putStrLn $ padLeft 6 (showHex n "")
      AddrNone -> return ()

    padLeft n s = replicate (n - length s) '0' ++ s

    printLine OctalByte line =
      putStrLn $ unwords [padLeft 3 (showOct b "") | b <- BS.unpack line]
    printLine OctalWord line =
      putStrLn $ formatWords showOct 6 line
    printLine HexWord line =
      putStrLn $ formatWords showHex 4 line
    printLine DecimalWord line =
      putStrLn $ formatWordsDecimal line
    printLine Char line =
      putStrLn $ unwords [formatChar b | b <- BS.unpack line]
    printLine Named line =
      putStrLn $ unwords [formatNamed b | b <- BS.unpack line]

    -- Format as 2-byte words (little-endian)
    formatWords showFn fieldWidth bytes' =
      let bytes'' = BS.unpack bytes'
          words' = toWords bytes''
       in unwords [padLeft fieldWidth (showFn w "") | w <- words']

    formatWordsDecimal bytes' =
      let bytes'' = BS.unpack bytes'
          words' = toWords bytes''
       in unwords [padLeft 5 (show w) | w <- words']

    -- Convert bytes to 2-byte words (little-endian)
    toWords :: [Word8] -> [Int]
    toWords [] = []
    toWords [b] = [fromIntegral b] -- Odd byte at end
    toWords (b1 : b2 : rest) = (fromIntegral b2 * 256 + fromIntegral b1) : toWords rest

formatChar :: Word8 -> String
formatChar b
  | b == 0 = " \\0"
  | b == 7 = " \\a"
  | b == 8 = " \\b"
  | b == 9 = " \\t"
  | b == 10 = " \\n"
  | b == 11 = " \\v"
  | b == 12 = " \\f"
  | b == 13 = " \\r"
  | b >= 32 && b < 127 = "  " ++ [chr (fromIntegral b)]
  | otherwise = padLeft 3 (showOct b "")
  where
    padLeft n s = replicate (n - length s) ' ' ++ s

formatNamed :: Word8 -> String
formatNamed b = case b of
  0 -> "nul"
  1 -> "soh"
  2 -> "stx"
  3 -> "etx"
  4 -> "eot"
  5 -> "enq"
  6 -> "ack"
  7 -> "bel"
  8 -> " bs"
  9 -> " ht"
  10 -> " nl"
  11 -> " vt"
  12 -> " ff"
  13 -> " cr"
  14 -> " so"
  15 -> " si"
  16 -> "dle"
  17 -> "dc1"
  18 -> "dc2"
  19 -> "dc3"
  20 -> "dc4"
  21 -> "nak"
  22 -> "syn"
  23 -> "etb"
  24 -> "can"
  25 -> " em"
  26 -> "sub"
  27 -> "esc"
  28 -> " fs"
  29 -> " gs"
  30 -> " rs"
  31 -> " us"
  32 -> " sp"
  127 -> "del"
  _
    | b >= 33 && b < 127 -> "  " ++ [chr (fromIntegral b)]
    | otherwise -> padLeft 3 (showOct b "")
  where
    padLeft n s = replicate (n - length s) ' ' ++ s

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox od [OPTION]... [FILE]...",
        "Write an unambiguous representation of FILE to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -A RADIX       output offset radix (d, o, x, or n for none)",
        "  -b             select octal bytes",
        "  -c             select printable characters or backslash escapes",
        "  -d             select unsigned decimal 2-byte units",
        "  -o             select octal 2-byte units",
        "  -x             select hexadecimal 2-byte units",
        "  -j BYTES       skip BYTES input bytes first",
        "  -N BYTES       limit dump to BYTES input bytes",
        "  -w[BYTES]      output BYTES bytes per output line (default 16)",
        "      --help     display this help and exit",
        "      --version  output version information and exit"
      ]
