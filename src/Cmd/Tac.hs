{-# LANGUAGE OverloadedStrings #-}

module Cmd.Tac (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox tac: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "tac") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- tacFiles opts paths
        if hasError then exitFailure else exitSuccess

data Opts = Opts
  { optSeparator :: !(Maybe C8.ByteString),
    optBefore :: !Bool,
    optRegex :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optSeparator = Nothing,
      optBefore = False,
      optRegex = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--before" : rest) = parseArgs opts {optBefore = True} rest
parseArgs opts ("--regex" : rest) = parseArgs opts {optRegex = True} rest
parseArgs opts ("--separator" : s : rest) = parseArgs opts {optSeparator = Just (C8.pack s)} rest
parseArgs opts (('-' : 's' : s) : rest)
  | null s = case rest of
      (sep : rest') -> parseArgs opts {optSeparator = Just (C8.pack sep)} rest'
      [] -> Left "option requires an argument -- 's'"
  | otherwise = parseArgs opts {optSeparator = Just (C8.pack s)} rest
parseArgs opts ("-b" : rest) = parseArgs opts {optBefore = True} rest
parseArgs opts ("-r" : rest) = parseArgs opts {optRegex = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

tacFiles :: Opts -> [FilePath] -> IO Bool
tacFiles opts = go False
  where
    go hasErr [] = return hasErr
    go hasErr (p : ps) = do
      err <- tacFile opts p
      go (hasErr || err) ps

tacFile :: Opts -> FilePath -> IO Bool
tacFile opts path = catch (tacFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox tac: " ++ path ++ ": " ++ friendlyError (show e)
      return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s || "inappropriate type" `isIn` s = "Is a directory"
      | otherwise = dropPrefix s

    dropPrefix str = case dropWhile (/= ':') str of
      [] -> str
      (_ : r) -> dropWhile (== ' ') r

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

tacFile' :: Opts -> FilePath -> IO ()
tacFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let sep = fromMaybe (C8.singleton '\n') (optSeparator opts)
      parts = splitOn sep contents
      reversed = reverse parts
  if optBefore opts
    then mapM_ (\p -> BS.hPut stdout sep >> BS.hPut stdout p) reversed
    else do
      case reversed of
        [] -> return ()
        (first' : rest') -> do
          BS.hPut stdout first'
          mapM_ (\p -> BS.hPut stdout sep >> BS.hPut stdout p) rest'
          BS.hPut stdout sep

splitOn :: C8.ByteString -> C8.ByteString -> [C8.ByteString]
splitOn sep bs
  | BS.null sep = [bs]
  | otherwise = go bs
  where
    sepLen = BS.length sep
    go s
      | BS.null s = []
      | otherwise = case findSubstring sep s of
          Nothing -> [s]
          Just idx ->
            let (before, after) = BS.splitAt idx s
             in before : go (BS.drop sepLen after)

findSubstring :: C8.ByteString -> C8.ByteString -> Maybe Int
findSubstring needle haystack = go 0
  where
    needleLen = BS.length needle
    haystackLen = BS.length haystack
    go i
      | i + needleLen > haystackLen = Nothing
      | BS.take needleLen (BS.drop i haystack) == needle = Just i
      | otherwise = go (i + 1)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox tac [OPTION]... [FILE]...",
        "Write each FILE to standard output, last line first.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -b, --before             attach the separator before instead of after",
        "  -r, --regex              interpret the separator as a regular expression",
        "  -s, --separator=STRING   use STRING as the separator instead of newline",
        "      --help               display this help and exit",
        "      --version            output version information and exit"
      ]
