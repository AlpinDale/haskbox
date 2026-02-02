{-# LANGUAGE OverloadedStrings #-}

module Cmd.Expand (run) where

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
    hPutStrLn stderr $ "haskbox expand: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "expand") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- expandFiles opts paths
        if hasError then exitFailure else exitSuccess

data Opts = Opts
  { optTabStops :: ![Int],
    optInitial :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optTabStops = [8],
      optInitial = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--initial" : rest) = parseArgs opts {optInitial = True} rest
parseArgs opts ("--tabs" : t : rest) = case parseTabStops t of
  Just stops -> parseArgs opts {optTabStops = stops} rest
  Nothing -> Left $ "invalid tab stop: '" ++ t ++ "'"
parseArgs opts (('-' : 't' : t) : rest)
  | null t = case rest of
      (stops : rest') -> case parseTabStops stops of
        Just s -> parseArgs opts {optTabStops = s} rest'
        Nothing -> Left $ "invalid tab stop: '" ++ stops ++ "'"
      [] -> Left "option requires an argument -- 't'"
  | otherwise = case parseTabStops t of
      Just s -> parseArgs opts {optTabStops = s} rest
      Nothing -> Left $ "invalid tab stop: '" ++ t ++ "'"
parseArgs opts ("-i" : rest) = parseArgs opts {optInitial = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : n) : rest)
  | all isDigit n && not (null n) = parseArgs opts {optTabStops = [read n]} rest
  | otherwise = Left $ "invalid option -- '" ++ take 1 n ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseTabStops :: String -> Maybe [Int]
parseTabStops s =
  let parts = splitOn ',' s
   in if all validNum parts
        then Just (map read parts)
        else Nothing
  where
    validNum p = not (null p) && all isDigit p
    splitOn _ [] = []
    splitOn c str =
      let (before, after) = break (== c) str
       in before : case after of
            [] -> []
            (_ : r) -> splitOn c r

expandFiles :: Opts -> [FilePath] -> IO Bool
expandFiles opts = go False
  where
    go hasErr [] = return hasErr
    go hasErr (p : ps) = do
      err <- expandFile opts p
      go (hasErr || err) ps

expandFile :: Opts -> FilePath -> IO Bool
expandFile opts path = catch (expandFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox expand: " ++ path ++ ": " ++ friendlyError (show e)
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

expandFile' :: Opts -> FilePath -> IO ()
expandFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let lns = C8.lines contents
  mapM_ (processLine opts) lns

processLine :: Opts -> C8.ByteString -> IO ()
processLine opts line = do
  let expanded = expandTabs opts line
  C8.hPutStrLn stdout expanded

expandTabs :: Opts -> C8.ByteString -> C8.ByteString
expandTabs opts line = C8.pack $ go 0 False (C8.unpack line)
  where
    stops = optTabStops opts
    onlyInitial = optInitial opts

    go _ _ [] = []
    go col seenNonBlank (c : cs)
      | c == '\t' && (not onlyInitial || not seenNonBlank) =
          let spaces = spacesToNextStop stops col
           in replicate spaces ' ' ++ go (col + spaces) seenNonBlank cs
      | c == ' ' = c : go (col + 1) seenNonBlank cs
      | otherwise = c : go (col + 1) True cs

spacesToNextStop :: [Int] -> Int -> Int
spacesToNextStop [] col = 8 - (col `mod` 8)
spacesToNextStop [step] col = step - (col `mod` step)
spacesToNextStop stops col =
  case filter (> col) stops of
    (next : _) -> next - col
    [] ->
      let lastStop = last stops
          step = if length stops > 1 then lastStop - (stops !! (length stops - 2)) else lastStop
       in step - ((col - lastStop) `mod` step)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox expand [OPTION]... [FILE]...",
        "Convert tabs in each FILE to spaces, writing to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -i, --initial         do not convert tabs after non blanks",
        "  -t, --tabs=N          have tabs N characters apart, not 8",
        "  -t, --tabs=LIST       use comma separated list of tab positions",
        "      --help            display this help and exit",
        "      --version         output version information and exit"
      ]
