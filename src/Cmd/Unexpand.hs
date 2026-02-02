{-# LANGUAGE OverloadedStrings #-}

module Cmd.Unexpand (run) where

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
    hPutStrLn stderr $ "haskbox unexpand: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "unexpand") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- unexpandFiles opts paths
        if hasError then exitFailure else exitSuccess

data Opts = Opts
  { optTabStops :: ![Int],
    optAllBlanks :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optTabStops = [8],
      optAllBlanks = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--all" : rest) = parseArgs opts {optAllBlanks = True} rest
parseArgs opts ("--first-only" : rest) = parseArgs opts {optAllBlanks = False} rest
parseArgs opts ("--tabs" : t : rest) = case parseTabStops t of
  Just stops -> parseArgs opts {optTabStops = stops, optAllBlanks = True} rest
  Nothing -> Left $ "invalid tab stop: '" ++ t ++ "'"
parseArgs opts (('-' : 't' : t) : rest)
  | null t = case rest of
      (stops : rest') -> case parseTabStops stops of
        Just s -> parseArgs opts {optTabStops = s, optAllBlanks = True} rest'
        Nothing -> Left $ "invalid tab stop: '" ++ stops ++ "'"
      [] -> Left "option requires an argument -- 't'"
  | otherwise = case parseTabStops t of
      Just s -> parseArgs opts {optTabStops = s, optAllBlanks = True} rest
      Nothing -> Left $ "invalid tab stop: '" ++ t ++ "'"
parseArgs opts ("-a" : rest) = parseArgs opts {optAllBlanks = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : n) : rest)
  | all isDigit n && not (null n) = parseArgs opts {optTabStops = [read n], optAllBlanks = True} rest
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

unexpandFiles :: Opts -> [FilePath] -> IO Bool
unexpandFiles opts = go False
  where
    go hasErr [] = return hasErr
    go hasErr (p : ps) = do
      err <- unexpandFile opts p
      go (hasErr || err) ps

unexpandFile :: Opts -> FilePath -> IO Bool
unexpandFile opts path = catch (unexpandFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox unexpand: " ++ path ++ ": " ++ friendlyError (show e)
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

unexpandFile' :: Opts -> FilePath -> IO ()
unexpandFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let lns = C8.lines contents
  mapM_ (processLine opts) lns

processLine :: Opts -> C8.ByteString -> IO ()
processLine opts line = do
  let unexpanded = unexpandSpaces opts line
  C8.hPutStrLn stdout unexpanded

unexpandSpaces :: Opts -> C8.ByteString -> C8.ByteString
unexpandSpaces opts line = C8.pack $ go 0 0 False (C8.unpack line)
  where
    stops = optTabStops opts
    convertAll = optAllBlanks opts

    go _ spaceCount _ [] =
      replicate spaceCount ' '
    go col spaceCount seenNonBlank (c : cs)
      | c == ' ' && (convertAll || not seenNonBlank) =
          let newCol = col + 1
              nextStop = getNextStop stops col
           in if newCol == nextStop && spaceCount > 0
                then '\t' : go newCol 0 seenNonBlank cs
                else go newCol (spaceCount + 1) seenNonBlank cs
      | c == ' ' =
          replicate spaceCount ' ' ++ c : go (col + 1) 0 True cs
      | c == '\t' =
          let nextStop = getNextStop stops col
           in '\t' : go nextStop 0 seenNonBlank cs
      | otherwise =
          replicate spaceCount ' ' ++ c : go (col + 1) 0 True cs

getNextStop :: [Int] -> Int -> Int
getNextStop [] col = ((col `div` 8) + 1) * 8
getNextStop [step] col = ((col `div` step) + 1) * step
getNextStop stops col =
  case filter (> col) stops of
    (next : _) -> next
    [] ->
      let lastStop = last stops
          step = if length stops > 1 then lastStop - (stops !! (length stops - 2)) else lastStop
       in lastStop + ((col - lastStop) `div` step + 1) * step

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox unexpand [OPTION]... [FILE]...",
        "Convert blanks in each FILE to tabs, writing to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -a, --all             convert all blanks, instead of just initial blanks",
        "      --first-only      convert only leading sequences of blanks (default)",
        "  -t, --tabs=N          have tabs N characters apart instead of 8 (implies -a)",
        "  -t, --tabs=LIST       use comma separated list of tab positions (implies -a)",
        "      --help            display this help and exit",
        "      --version         output version information and exit"
      ]
