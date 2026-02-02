{-# LANGUAGE OverloadedStrings #-}

module Cmd.Fold (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit, isSpace)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox fold: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "fold") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- foldFiles opts paths
        if hasError then exitFailure else exitSuccess

data Opts = Opts
  { optWidth :: !Int,
    optBytes :: !Bool,
    optSpaces :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optWidth = 80,
      optBytes = False,
      optSpaces = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--bytes" : rest) = parseArgs opts {optBytes = True} rest
parseArgs opts ("--spaces" : rest) = parseArgs opts {optSpaces = True} rest
parseArgs opts ("--width" : n : rest) = case parseNum n of
  Just w -> parseArgs opts {optWidth = w} rest
  Nothing -> Left $ "invalid width: '" ++ n ++ "'"
parseArgs opts ("-b" : rest) = parseArgs opts {optBytes = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optSpaces = True} rest
parseArgs opts (('-' : 'w' : n) : rest)
  | null n = case rest of
      (width : rest') -> case parseNum width of
        Just w -> parseArgs opts {optWidth = w} rest'
        Nothing -> Left $ "invalid width: '" ++ width ++ "'"
      [] -> Left "option requires an argument -- 'w'"
  | otherwise = case parseNum n of
      Just w -> parseArgs opts {optWidth = w} rest
      Nothing -> Left $ "invalid width: '" ++ n ++ "'"
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : n) : rest)
  | all isDigit n && not (null n) = parseArgs opts {optWidth = read n} rest
  | otherwise = Left $ "invalid option -- '" ++ take 1 n ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseNum :: String -> Maybe Int
parseNum s
  | all isDigit s && not (null s) = Just (read s)
  | otherwise = Nothing

foldFiles :: Opts -> [FilePath] -> IO Bool
foldFiles opts = go False
  where
    go hasErr [] = return hasErr
    go hasErr (p : ps) = do
      err <- foldFile opts p
      go (hasErr || err) ps

foldFile :: Opts -> FilePath -> IO Bool
foldFile opts path = catch (foldFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox fold: " ++ path ++ ": " ++ friendlyError (show e)
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

foldFile' :: Opts -> FilePath -> IO ()
foldFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let lns = C8.lines contents
  mapM_ (processLine opts) lns

processLine :: Opts -> C8.ByteString -> IO ()
processLine opts line
  | BS.null line = C8.hPutStrLn stdout C8.empty
  | otherwise = do
      let folded = foldLine opts line
      mapM_ (C8.hPutStrLn stdout) folded

foldLine :: Opts -> C8.ByteString -> [C8.ByteString]
foldLine opts = go
  where
    width = optWidth opts
    useSpaces = optSpaces opts

    go bs
      | BS.length bs <= width = [bs]
      | useSpaces =
          let (chunk, rest) = splitAtSpace width bs
           in chunk : go (C8.dropWhile isSpace rest)
      | otherwise =
          let (chunk, rest) = BS.splitAt width bs
           in chunk : go rest

    splitAtSpace w bs =
      let (prefix, suffix) = BS.splitAt w bs
          lastSpace = findLastSpace prefix
       in case lastSpace of
            Just idx | idx > 0 -> BS.splitAt idx prefix `combine` suffix
            _ -> (prefix, suffix)

    combine (a, b) c = (a, b <> c)

    findLastSpace bs = go' (BS.length bs - 1)
      where
        go' (-1) = Nothing
        go' i
          | isSpace (C8.index bs i) = Just i
          | otherwise = go' (i - 1)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox fold [OPTION]... [FILE]...",
        "Wrap input lines in each FILE, writing to standard output.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -b, --bytes         count bytes rather than columns",
        "  -s, --spaces        break at spaces",
        "  -w, --width=WIDTH   use WIDTH columns instead of 80",
        "      --help          display this help and exit",
        "      --version       output version information and exit"
      ]
