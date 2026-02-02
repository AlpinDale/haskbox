{-# LANGUAGE OverloadedStrings #-}

module Cmd.Touch (run) where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IOMode (..), hPutStrLn, stderr, withFile)
import System.Posix.Files (accessTime, fileExist, getFileStatus, modificationTime, setFileTimes)
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox touch: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "touch") >> exitSuccess
    | null files -> do
        hPutStrLn stderr "haskbox touch: missing file operand"
        hPutStrLn stderr "Try 'haskbox touch --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (touchFile opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optNoCreate :: !Bool,
    optAccessOnly :: !Bool,
    optModifyOnly :: !Bool,
    optReference :: !(Maybe FilePath),
    optDateString :: !(Maybe String),
    optTimestamp :: !(Maybe String),
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optNoCreate = False,
      optAccessOnly = False,
      optModifyOnly = False,
      optReference = Nothing,
      optDateString = Nothing,
      optTimestamp = Nothing,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--no-create" : rest) = parseArgs opts {optNoCreate = True} rest
parseArgs opts ("-c" : rest) = parseArgs opts {optNoCreate = True} rest
parseArgs opts ("-a" : rest) = parseArgs opts {optAccessOnly = True} rest
parseArgs opts ("-m" : rest) = parseArgs opts {optModifyOnly = True} rest
parseArgs opts (('-' : 'r' : r) : rest)
  | null r = case rest of
      (ref : rest') -> parseArgs opts {optReference = Just ref} rest'
      [] -> Left "option requires an argument -- 'r'"
  | otherwise = parseArgs opts {optReference = Just r} rest
parseArgs opts ("--reference" : r : rest) = parseArgs opts {optReference = Just r} rest
parseArgs opts (('-' : 'd' : d) : rest)
  | null d = case rest of
      (date : rest') -> parseArgs opts {optDateString = Just date} rest'
      [] -> Left "option requires an argument -- 'd'"
  | otherwise = parseArgs opts {optDateString = Just d} rest
parseArgs opts ("--date" : d : rest) = parseArgs opts {optDateString = Just d} rest
parseArgs opts (('-' : 't' : t) : rest)
  | null t = case rest of
      (ts : rest') -> parseArgs opts {optTimestamp = Just ts} rest'
      [] -> Left "option requires an argument -- 't'"
  | otherwise = parseArgs opts {optTimestamp = Just t} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      'c' -> parseShortFlags o {optNoCreate = True} fs r
      'a' -> parseShortFlags o {optAccessOnly = True} fs r
      'm' -> parseShortFlags o {optModifyOnly = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

touchFile :: Opts -> FilePath -> IO Bool
touchFile opts path = do
  exists <- fileExist path
  if not exists && optNoCreate opts
    then return False
    else catch (doTouch exists) handler
  where
    doTouch exists = do
      -- Create file if it doesn't exist
      unless exists $ withFile path WriteMode (\_ -> return ())

      -- Determine what time to use
      newTime <- getNewTime

      case newTime of
        Left err -> do
          hPutStrLn stderr $ "haskbox touch: " ++ err
          return True
        Right t -> do
          -- Get current times if we need to preserve one
          (curAtime, curMtime) <-
            if optAccessOnly opts || optModifyOnly opts
              then do
                status <- getFileStatus path
                return (accessTime status, modificationTime status)
              else return (t, t)

          let atime = if optModifyOnly opts then curAtime else t
              mtime = if optAccessOnly opts then curMtime else t

          setFileTimes path atime mtime
          return False

    getNewTime :: IO (Either String EpochTime)
    getNewTime
      | Just ref <- optReference opts =
          catch
            ( do
                status <- getFileStatus ref
                return $ Right (modificationTime status)
            )
            refHandler
      | Just dateStr <- optDateString opts = return $ parseDateString dateStr
      | Just ts <- optTimestamp opts = return $ parseTimestamp ts
      | otherwise = Right <$> epochTime

    refHandler :: IOException -> IO (Either String EpochTime)
    refHandler _ = return $ Left $ "failed to get attributes of '" ++ fromMaybe "" (optReference opts) ++ "'"

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox touch: cannot touch '" ++ path ++ "': " ++ friendlyError (show e)
      return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s = "Is a directory"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

-- Parse -d DATE_STRING (flexible format like GNU)
parseDateString :: String -> Either String EpochTime
parseDateString s =
  case tryParseDate s of
    Just utc -> Right $ utcToEpoch utc
    Nothing -> Left $ "invalid date format '" ++ s ++ "'"

tryParseDate :: String -> Maybe UTCTime
tryParseDate s =
  firstJust
    [ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" s,
      parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" s,
      parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" s,
      parseTimeM True defaultTimeLocale "%Y-%m-%d" s,
      parseTimeM True defaultTimeLocale "%b %d %Y %H:%M:%S" s,
      parseTimeM True defaultTimeLocale "%b %d %Y" s,
      parseTimeM True defaultTimeLocale "%B %d %Y" s
    ]
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : xs) = firstJust xs

-- Parse -t STAMP format: [[CC]YY]MMDDhhmm[.ss]
parseTimestamp :: String -> Either String EpochTime
parseTimestamp s
  | not (all (\c -> isDigit c || c == '.') s) = Left $ "invalid timestamp '" ++ s ++ "'"
  | otherwise =
      let (main, rest) = break (== '.') s
          secs = if null rest then "00" else drop 1 rest
       in case length main of
            8 -> parseTs ("20" ++ take 2 main) (drop 2 main) secs -- MMDDhhmm -> 20YY
            10 -> parseTs ("20" ++ take 2 main) (drop 2 main) secs -- YYMMDDhhmm
            12 -> parseTs (take 4 main) (drop 4 main) secs -- CCYYMMDDhhmm
            _ -> Left $ "invalid timestamp '" ++ s ++ "'"
  where
    parseTs ccyy mmddhhmm ss =
      let mm = take 2 mmddhhmm
          dd = take 2 (drop 2 mmddhhmm)
          hh = take 2 (drop 4 mmddhhmm)
          mi = take 2 (drop 6 mmddhhmm)
          dateStr = ccyy ++ "-" ++ mm ++ "-" ++ dd ++ " " ++ hh ++ ":" ++ mi ++ ":" ++ ss
       in case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" dateStr of
            Just utc -> Right $ utcToEpoch utc
            Nothing -> Left $ "invalid timestamp '" ++ s ++ "'"

utcToEpoch :: UTCTime -> EpochTime
utcToEpoch = fromIntegral . (round :: Double -> Integer) . realToFrac . utcTimeToPOSIXSeconds

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox touch [OPTION]... FILE...",
        "Update the access and modification times of each FILE to the current time.",
        "",
        "A FILE argument that does not exist is created empty, unless -c is supplied.",
        "",
        "  -a                     change only the access time",
        "  -c, --no-create        do not create any files",
        "  -d, --date=STRING      parse STRING and use it instead of current time",
        "  -m                     change only the modification time",
        "  -r, --reference=FILE   use this file's times instead of current time",
        "  -t STAMP               use [[CC]YY]MMDDhhmm[.ss] instead of current time",
        "      --help             display this help and exit",
        "      --version          output version information and exit"
      ]
