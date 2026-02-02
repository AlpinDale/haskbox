{-# LANGUAGE OverloadedStrings #-}

module Cmd.Echo (run) where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as C8
import Data.Char (chr, digitToInt, isHexDigit, isOctDigit)
import System.Exit (exitSuccess)
import System.IO (stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "echo") >> exitSuccess
  _ -> do
    let (opts, rest) = parseOpts defaultOpts args
    echoOutput opts rest

data Opts = Opts
  { optNewline :: !Bool,
    optEscape :: !Bool
  }

defaultOpts :: Opts
defaultOpts = Opts True False

parseOpts :: Opts -> [String] -> (Opts, [String])
parseOpts opts [] = (opts, [])
parseOpts opts (arg : rest)
  | arg == "-n" = parseOpts opts {optNewline = False} rest
  | arg == "-e" = parseOpts opts {optEscape = True} rest
  | arg == "-E" = parseOpts opts {optEscape = False} rest
  | arg == "-nE" || arg == "-En" = parseOpts opts {optNewline = False, optEscape = False} rest
  | arg == "-ne" || arg == "-en" = parseOpts opts {optNewline = False, optEscape = True} rest
  | arg == "-eE" || arg == "-Ee" = parseOpts opts {optEscape = False} rest
  | otherwise = (opts, arg : rest)

echoOutput :: Opts -> [String] -> IO ()
echoOutput opts strs = do
  let output = unwords strs
      (processed, stop) =
        if optEscape opts
          then processEscapes output
          else (output, False)
  C8.hPut stdout (C8.pack processed)
  when (not stop && optNewline opts) $ C8.hPut stdout "\n"

-- Returns (processed string, whether \c was encountered)
processEscapes :: String -> (String, Bool)
processEscapes [] = ([], False)
processEscapes ('\\' : c : rest) = case c of
  '\\' -> let (s, stop) = processEscapes rest in ('\\' : s, stop)
  'a' -> let (s, stop) = processEscapes rest in ('\a' : s, stop)
  'b' -> let (s, stop) = processEscapes rest in ('\b' : s, stop)
  'c' -> ([], True)
  'e' -> let (s, stop) = processEscapes rest in ('\x1b' : s, stop)
  'f' -> let (s, stop) = processEscapes rest in ('\f' : s, stop)
  'n' -> let (s, stop) = processEscapes rest in ('\n' : s, stop)
  'r' -> let (s, stop) = processEscapes rest in ('\r' : s, stop)
  't' -> let (s, stop) = processEscapes rest in ('\t' : s, stop)
  'v' -> let (s, stop) = processEscapes rest in ('\v' : s, stop)
  '0' -> parseOctal rest
  'x' -> parseHex rest
  _ -> let (s, stop) = processEscapes rest in ('\\' : c : s, stop)
processEscapes (c : rest) = let (s, stop) = processEscapes rest in (c : s, stop)

parseOctal :: String -> (String, Bool)
parseOctal s =
  let (digits, _) = span isOctDigit (take 3 s)
      remaining = drop (length digits) s
      (rest, stop) = processEscapes remaining
   in if null digits
        then let (r, st) = processEscapes s in ('\0' : r, st)
        else (chr (octalToInt digits) : rest, stop)
  where
    octalToInt = foldl (\acc d -> acc * 8 + digitToInt d) 0

parseHex :: String -> (String, Bool)
parseHex s =
  let (digits, _) = span isHexDigit (take 2 s)
      remaining = drop (length digits) s
      (rest, stop) = processEscapes remaining
   in if null digits
        then let (r, st) = processEscapes s in ('\\' : 'x' : r, st)
        else (chr (hexToInt digits) : rest, stop)
  where
    hexToInt = foldl (\acc d -> acc * 16 + digitToInt d) 0

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox echo [SHORT-OPTION]... [STRING]...",
        "  or:  haskbox echo LONG-OPTION",
        "Echo the STRING(s) to standard output.",
        "",
        "  -n             do not output the trailing newline",
        "  -e             enable interpretation of backslash escapes",
        "  -E             disable interpretation of backslash escapes (default)",
        "      --help     display this help and exit",
        "      --version  output version information and exit",
        "",
        "If -e is in effect, the following sequences are recognized:",
        "",
        "  \\\\      backslash",
        "  \\a      alert (BEL)",
        "  \\b      backspace",
        "  \\c      produce no further output",
        "  \\e      escape",
        "  \\f      form feed",
        "  \\n      new line",
        "  \\r      carriage return",
        "  \\t      horizontal tab",
        "  \\v      vertical tab",
        "  \\0NNN   byte with octal value NNN (1 to 3 digits)",
        "  \\xHH    byte with hexadecimal value HH (1 to 2 digits)"
      ]
