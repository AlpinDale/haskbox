{-# LANGUAGE OverloadedStrings #-}

module Cmd.Date (run) where

import Control.Exception (IOException, catch)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (ZonedTime, getCurrentTimeZone, getZonedTime, utcToZonedTime)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, modificationTime)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox date: " ++ err
    exitFailure
  Right (opts, rest)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "date") >> exitSuccess
    | isJust (optDate opts) -> printDate opts (optDate opts) Nothing
    | isJust (optReference opts) -> printReferenceDate opts (optReference opts) (getFormat rest)
    | otherwise -> case rest of
        [] -> printCurrentDate opts
        ('+' : fmt) : _ -> printWithFormat opts fmt
        _ -> do
          hPutStrLn stderr "haskbox date: invalid operand"
          exitFailure
  where
    getFormat [] = Nothing
    getFormat (('+' : fmt) : _) = Just fmt
    getFormat (_ : xs) = getFormat xs

data Opts = Opts
  { optDate :: !(Maybe String),
    optReference :: !(Maybe FilePath),
    optUtc :: !Bool,
    optRfc2822 :: !Bool,
    optIso8601 :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optDate = Nothing,
      optReference = Nothing,
      optUtc = False,
      optRfc2822 = False,
      optIso8601 = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--utc" : rest) = parseArgs opts {optUtc = True} rest
parseArgs opts ("--universal" : rest) = parseArgs opts {optUtc = True} rest
parseArgs opts ("--rfc-2822" : rest) = parseArgs opts {optRfc2822 = True} rest
parseArgs opts ("--rfc-email" : rest) = parseArgs opts {optRfc2822 = True} rest
parseArgs opts ("--iso-8601" : rest) = parseArgs opts {optIso8601 = True} rest
parseArgs opts ("-u" : rest) = parseArgs opts {optUtc = True} rest
parseArgs opts ("-R" : rest) = parseArgs opts {optRfc2822 = True} rest
parseArgs opts ("-I" : rest) = parseArgs opts {optIso8601 = True} rest
parseArgs opts (('-' : 'r' : r) : rest)
  | null r = case rest of
      (ref : rest') -> parseArgs opts {optReference = Just ref} rest'
      [] -> Left "option requires an argument -- 'r'"
  | otherwise = parseArgs opts {optReference = Just r} rest
parseArgs opts ("--reference" : r : rest) = parseArgs opts {optReference = Just r} rest
parseArgs opts (('-' : 'd' : d) : rest)
  | null d = case rest of
      (date : rest') -> parseArgs opts {optDate = Just date} rest'
      [] -> Left "option requires an argument -- 'd'"
  | otherwise = parseArgs opts {optDate = Just d} rest
parseArgs opts ("--date" : d : rest) = parseArgs opts {optDate = Just d} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

printCurrentDate :: Opts -> IO ()
printCurrentDate opts = do
  now <- if optUtc opts then getCurrentTime <&> utcToStr else getZonedTime <&> zonedToStr opts
  putStrLn now
  where
    utcToStr = formatTime defaultTimeLocale "%a %b %e %H:%M:%S UTC %Y"

printWithFormat :: Opts -> String -> IO ()
printWithFormat opts fmt = do
  result <-
    if optUtc opts
      then formatTime defaultTimeLocale (translateFormat fmt) <$> getCurrentTime
      else formatTime defaultTimeLocale (translateFormat fmt) <$> getZonedTime
  putStrLn result

printDate :: Opts -> Maybe String -> Maybe String -> IO ()
printDate opts (Just dateStr) maybeFmt = do
  let parsed = tryParse dateStr
  case parsed of
    Just utc -> do
      result <- case maybeFmt of
        Just fmt ->
          if optUtc opts
            then return $ formatTime defaultTimeLocale (translateFormat fmt) utc
            else do
              tz <- getCurrentTimeZone
              return $ formatTime defaultTimeLocale (translateFormat fmt) (utcToZonedTime tz utc)
        Nothing ->
          if optUtc opts
            then return $ formatTime defaultTimeLocale "%a %b %e %H:%M:%S UTC %Y" utc
            else do
              tz <- getCurrentTimeZone
              return $ formatTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" (utcToZonedTime tz utc)
      putStrLn result
    Nothing -> do
      hPutStrLn stderr $ "haskbox date: invalid date '" ++ dateStr ++ "'"
      exitFailure
printDate _ Nothing _ = return ()

printReferenceDate :: Opts -> Maybe FilePath -> Maybe String -> IO ()
printReferenceDate _ Nothing _ = return ()
printReferenceDate opts (Just refPath) maybeFmt = catch doRef handler
  where
    doRef = do
      status <- getFileStatus refPath
      let mtime = modificationTime status
          utc = posixSecondsToUTCTime (realToFrac mtime)
      result <- case maybeFmt of
        Just fmt ->
          if optUtc opts
            then return $ formatTime defaultTimeLocale (translateFormat fmt) utc
            else do
              tz <- getCurrentTimeZone
              return $ formatTime defaultTimeLocale (translateFormat fmt) (utcToZonedTime tz utc)
        Nothing ->
          if optUtc opts
            then return $ formatTime defaultTimeLocale "%a %b %e %H:%M:%S UTC %Y" utc
            else do
              tz <- getCurrentTimeZone
              return $ formatTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" (utcToZonedTime tz utc)
      putStrLn result

    handler :: IOException -> IO ()
    handler _ = do
      hPutStrLn stderr $ "haskbox date: '" ++ refPath ++ "': No such file or directory"
      exitFailure

tryParse :: String -> Maybe UTCTime
tryParse s =
  firstJust
    [ parseTimeM True defaultTimeLocale "%Y-%m-%d" s,
      parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s,
      parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" s,
      parseTimeM True defaultTimeLocale "%b %d %Y" s,
      parseTimeM True defaultTimeLocale "%B %d %Y" s
    ]
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : xs) = firstJust xs

zonedToStr :: Opts -> ZonedTime -> String
zonedToStr opts zt
  | optRfc2822 opts = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z" zt
  | optIso8601 opts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" zt
  | otherwise = formatTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" zt

-- Translate GNU date format specifiers to Haskell's formatTime specifiers
-- Most are the same, but some need translation
translateFormat :: String -> String
translateFormat [] = []
translateFormat ('%' : '%' : rest) = '%' : '%' : translateFormat rest
translateFormat ('%' : c : rest) = '%' : translateChar c ++ translateFormat rest
translateFormat (c : rest) = c : translateFormat rest

translateChar :: Char -> String
translateChar c = case c of
  -- These are the same in both
  'a' -> "a" -- abbreviated weekday
  'A' -> "A" -- full weekday
  'b' -> "b" -- abbreviated month
  'B' -> "B" -- full month
  'd' -> "d" -- day of month (01-31)
  'e' -> "e" -- day of month, space padded
  'H' -> "H" -- hour (00-23)
  'I' -> "I" -- hour (01-12)
  'j' -> "j" -- day of year (001-366)
  'm' -> "m" -- month (01-12)
  'M' -> "M" -- minute (00-59)
  'p' -> "p" -- AM/PM
  'P' -> "P" -- am/pm (lowercase)
  'S' -> "S" -- second (00-60)
  'u' -> "u" -- day of week (1-7, Monday=1)
  'U' -> "U" -- week number (00-53, Sunday start)
  'V' -> "V" -- ISO week number (01-53)
  'w' -> "w" -- day of week (0-6, Sunday=0)
  'W' -> "W" -- week number (00-53, Monday start)
  'y' -> "y" -- year without century (00-99)
  'Y' -> "Y" -- year with century
  'z' -> "z" -- timezone offset (+0000)
  'Z' -> "Z" -- timezone name
  -- GNU-specific that need translation or are the same
  'c' -> "c" -- locale's date and time
  'C' -> "C" -- century (00-99)
  'D' -> "D" -- date as %m/%d/%y
  'F' -> "F" -- date as %Y-%m-%d
  'g' -> "g" -- ISO week-based year (2 digits)
  'G' -> "G" -- ISO week-based year (4 digits)
  'h' -> "h" -- same as %b
  'k' -> "k" -- hour (0-23), space padded
  'l' -> "l" -- hour (1-12), space padded
  'n' -> "\n" -- newline
  'N' -> "" -- nanoseconds (not directly supported, skip)
  'r' -> "r" -- time as 12-hour
  'R' -> "R" -- time as %H:%M
  's' -> "s" -- seconds since epoch
  't' -> "\t" -- tab
  'T' -> "T" -- time as %H:%M:%S
  'x' -> "x" -- locale's date representation
  'X' -> "X" -- locale's time representation
  '+' -> "+" -- date and time in date(1) format
  _ -> [c] -- pass through unknown

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox date [OPTION]... [+FORMAT]",
        "       haskbox date [-u|--utc|--universal] [-d DATESTRING]",
        "Display the current time in the given FORMAT, or set the system date.",
        "",
        "  -d, --date=STRING        display time described by STRING, not 'now'",
        "  -I, --iso-8601           output date/time in ISO 8601 format",
        "  -r, --reference=FILE     display the last modification time of FILE",
        "  -R, --rfc-2822           output date and time in RFC 2822 format",
        "  -u, --utc, --universal   print or set Coordinated Universal Time (UTC)",
        "      --help               display this help and exit",
        "      --version            output version information and exit",
        "",
        "FORMAT controls the output. Interpreted sequences are:",
        "  %%   a literal %",
        "  %a   locale's abbreviated weekday name (e.g., Sun)",
        "  %A   locale's full weekday name (e.g., Sunday)",
        "  %b   locale's abbreviated month name (e.g., Jan)",
        "  %B   locale's full month name (e.g., January)",
        "  %c   locale's date and time",
        "  %C   century (00..99)",
        "  %d   day of month (01..31)",
        "  %D   date; same as %m/%d/%y",
        "  %e   day of month, space padded",
        "  %F   full date; same as %Y-%m-%d",
        "  %g   last two digits of year of ISO week number",
        "  %G   year of ISO week number",
        "  %h   same as %b",
        "  %H   hour (00..23)",
        "  %I   hour (01..12)",
        "  %j   day of year (001..366)",
        "  %k   hour, space padded ( 0..23)",
        "  %l   hour, space padded ( 1..12)",
        "  %m   month (01..12)",
        "  %M   minute (00..59)",
        "  %n   a newline",
        "  %p   locale's equivalent of AM or PM",
        "  %P   like %p, but lower case",
        "  %r   locale's 12-hour clock time (e.g., 11:11:04 PM)",
        "  %R   24-hour hour and minute; same as %H:%M",
        "  %s   seconds since 1970-01-01 00:00:00 UTC",
        "  %S   second (00..60)",
        "  %t   a tab",
        "  %T   time; same as %H:%M:%S",
        "  %u   day of week (1..7); 1 is Monday",
        "  %U   week number of year, Sunday as first day (00..53)",
        "  %V   ISO week number (01..53)",
        "  %w   day of week (0..6); 0 is Sunday",
        "  %W   week number of year, Monday as first day (00..53)",
        "  %x   locale's date representation",
        "  %X   locale's time representation",
        "  %y   last two digits of year (00..99)",
        "  %Y   year",
        "  %z   +hhmm numeric time zone (e.g., -0400)",
        "  %Z   alphabetic time zone abbreviation (e.g., EDT)"
      ]
