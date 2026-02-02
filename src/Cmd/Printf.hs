module Cmd.Printf (run) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isOctDigit)
import Numeric (showHex, showOct)
import System.Exit (exitSuccess)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "printf") >> exitSuccess
  [] -> return ()
  (fmt : rest) -> do
    let expanded = expandEscapes fmt
    printFormatted expanded rest

printFormatted :: String -> [String] -> IO ()
printFormatted = go
  where
    go [] _ = return ()
    go ('%' : '%' : rest) as = putStr "%" >> go rest as
    go ('%' : rest) as = do
      let (spec, remaining) = parseFormatSpec rest
      case as of
        [] -> printSpec spec "" >> go remaining []
        (a : as') -> printSpec spec a >> go remaining as'
    go (c : rest) as = putChar c >> go rest as

    parseFormatSpec s =
      let (flags, s1) = span (\c -> c `elem` ("-+ #0" :: String)) s
          (width, s2) = span isDigit s1
          (prec, s3) = case s2 of
            '.' : rest -> let (p, r) = span isDigit rest in ('.' : p, r)
            _ -> ("", s2)
          (spec, s4) = case s3 of
            (c : rest) | c `elem` ("diouxXeEfFgGaAcsbq" :: String) -> ([c], rest)
            _ -> ("s", s3) -- Default to string
       in (flags ++ width ++ prec ++ spec, s4)

    printSpec spec arg = case last spec of
      'd' -> putStr $ formatInt spec arg
      'i' -> putStr $ formatInt spec arg
      'o' -> putStr $ formatOct spec arg
      'u' -> putStr $ formatUInt spec arg
      'x' -> putStr $ formatHex spec arg False
      'X' -> putStr $ formatHex spec arg True
      'f' -> putStr $ formatFloat spec arg
      'F' -> putStr $ formatFloat spec arg
      'e' -> putStr $ formatFloat spec arg
      'E' -> putStr $ formatFloat spec arg
      'g' -> putStr $ formatFloat spec arg
      'G' -> putStr $ formatFloat spec arg
      'c' -> putStr $ take 1 arg
      's' -> putStr $ formatStr spec arg
      'b' -> putStr $ expandEscapes arg
      'q' -> putStr $ show arg
      _ -> putStr arg

formatInt :: String -> String -> String
formatInt _ "" = "0"
formatInt _ s = case reads s :: [(Integer, String)] of
  [(n, _)] -> show n
  _ -> "0"

formatUInt :: String -> String -> String
formatUInt _ "" = "0"
formatUInt _ s = case reads s :: [(Integer, String)] of
  [(n, _)] -> show (abs n)
  _ -> "0"

formatOct :: String -> String -> String
formatOct _ "" = "0"
formatOct _ s = case reads s :: [(Integer, String)] of
  [(n, _)] -> showOct (abs n) ""
  _ -> "0"

formatHex :: String -> String -> Bool -> String
formatHex _ "" _ = "0"
formatHex _ s upper = case reads s :: [(Integer, String)] of
  [(n, _)] -> let h = showHex (abs n) "" in if upper then map toUpperChar h else h
  _ -> "0"
  where
    toUpperChar c
      | c >= 'a' && c <= 'f' = chr (fromEnum c - 32)
      | otherwise = c

formatFloat :: String -> String -> String
formatFloat _ "" = "0.000000"
formatFloat _ s = case reads s :: [(Double, String)] of
  [(n, _)] -> show n
  _ -> "0.000000"

formatStr :: String -> String -> String
formatStr spec s =
  let width = extractWidth spec
      leftAlign = '-' `elem` spec
   in if width > 0
        then
          if leftAlign
            then s ++ replicate (width - length s) ' '
            else replicate (width - length s) ' ' ++ s
        else s
  where
    extractWidth = foldl (\acc c -> if isDigit c then acc * 10 + digitToInt c else acc) 0

expandEscapes :: String -> String
expandEscapes [] = []
expandEscapes ('\\' : 'n' : rest) = '\n' : expandEscapes rest
expandEscapes ('\\' : 't' : rest) = '\t' : expandEscapes rest
expandEscapes ('\\' : 'r' : rest) = '\r' : expandEscapes rest
expandEscapes ('\\' : 'a' : rest) = '\a' : expandEscapes rest
expandEscapes ('\\' : 'b' : rest) = '\b' : expandEscapes rest
expandEscapes ('\\' : 'f' : rest) = '\f' : expandEscapes rest
expandEscapes ('\\' : 'v' : rest) = '\v' : expandEscapes rest
expandEscapes ('\\' : '\\' : rest) = '\\' : expandEscapes rest
expandEscapes ('\\' : '"' : rest) = '"' : expandEscapes rest
expandEscapes ('\\' : '\'' : rest) = '\'' : expandEscapes rest
expandEscapes ('\\' : '0' : rest) = expandOctal rest
expandEscapes ('\\' : 'x' : rest) = expandHex rest
expandEscapes ('\\' : c : rest)
  | isOctDigit c = expandOctal (c : rest)
  | otherwise = c : expandEscapes rest
expandEscapes (c : rest) = c : expandEscapes rest

expandOctal :: String -> String
expandOctal s =
  let (digits, _rest) = span isOctDigit (take 3 s)
      remaining = drop (length digits) s
      value = foldl (\acc c -> acc * 8 + digitToInt c) 0 digits
   in chr value : expandEscapes remaining

expandHex :: String -> String
expandHex s =
  let (digits, _rest) = span isHexDigit (take 2 s)
      remaining = drop (length digits) s
      value = foldl (\acc c -> acc * 16 + hexDigitToInt c) 0 digits
   in chr value : expandEscapes remaining
  where
    hexDigitToInt c
      | isDigit c = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox printf FORMAT [ARGUMENT]...",
        "Print ARGUMENT(s) according to FORMAT.",
        "",
        "FORMAT controls the output as in C printf. Interpreted sequences:",
        "  \\\"      double quote",
        "  \\\\      backslash",
        "  \\a      alert (BEL)",
        "  \\b      backspace",
        "  \\f      form feed",
        "  \\n      new line",
        "  \\r      carriage return",
        "  \\t      horizontal tab",
        "  \\v      vertical tab",
        "  \\0NNN   octal value",
        "  \\xHH    hexadecimal value",
        "",
        "Format specifiers:",
        "  %d, %i  signed decimal integer",
        "  %o      unsigned octal",
        "  %u      unsigned decimal integer",
        "  %x, %X  unsigned hexadecimal",
        "  %f, %F  floating point",
        "  %e, %E  scientific notation",
        "  %g, %G  shortest of %f or %e",
        "  %c      single character",
        "  %s      string",
        "  %b      string with backslash escapes",
        "  %%      literal %",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
