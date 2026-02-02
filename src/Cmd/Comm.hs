{-# LANGUAGE OverloadedStrings #-}

module Cmd.Comm (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox comm: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "comm") >> exitSuccess
    | otherwise -> case files of
        [file1, file2] -> do
          hasError <- commFiles opts file1 file2
          if hasError then exitFailure else exitSuccess
        _ -> do
          hPutStrLn stderr "haskbox comm: requires exactly two files"
          exitFailure

data Opts = Opts
  { optSuppress1 :: !Bool,
    optSuppress2 :: !Bool,
    optSuppress3 :: !Bool,
    optDelimiter :: !String,
    optCheckOrder :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optSuppress1 = False,
      optSuppress2 = False,
      optSuppress3 = False,
      optDelimiter = "\t",
      optCheckOrder = True,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--nocheck-order" : rest) = parseArgs opts {optCheckOrder = False} rest
parseArgs opts ("--check-order" : rest) = parseArgs opts {optCheckOrder = True} rest
parseArgs opts ("--output-delimiter" : d : rest) = parseArgs opts {optDelimiter = d} rest
parseArgs opts ("-1" : rest) = parseArgs opts {optSuppress1 = True} rest
parseArgs opts ("-2" : rest) = parseArgs opts {optSuppress2 = True} rest
parseArgs opts ("-3" : rest) = parseArgs opts {optSuppress3 = True} rest
parseArgs opts (('-' : digits) : rest)
  | all (`elem` ("123" :: String)) digits = parseArgs (applySuppression opts digits) rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

applySuppression :: Opts -> String -> Opts
applySuppression opts [] = opts
applySuppression opts ('1' : cs) = applySuppression opts {optSuppress1 = True} cs
applySuppression opts ('2' : cs) = applySuppression opts {optSuppress2 = True} cs
applySuppression opts ('3' : cs) = applySuppression opts {optSuppress3 = True} cs
applySuppression opts (_ : cs) = applySuppression opts cs

commFiles :: Opts -> FilePath -> FilePath -> IO Bool
commFiles opts file1 file2 = do
  result1 <- readInputFile file1
  result2 <- readInputFile file2
  case (result1, result2) of
    (Left err, _) -> do
      hPutStrLn stderr $ "haskbox comm: " ++ err
      return True
    (_, Left err) -> do
      hPutStrLn stderr $ "haskbox comm: " ++ err
      return True
    (Right contents1, Right contents2) -> do
      let lines1 = C8.lines contents1
          lines2 = C8.lines contents2
      compareLists opts lines1 lines2
      return False

compareLists :: Opts -> [C8.ByteString] -> [C8.ByteString] -> IO ()
compareLists opts = go
  where
    delim = C8.pack (optDelimiter opts)

    go [] [] = return ()
    go (x : xs) [] = do
      outputCol1 x
      go xs []
    go [] (y : ys) = do
      outputCol2 y
      go [] ys
    go (x : xs) (y : ys) = case compare x y of
      LT -> do
        outputCol1 x
        go xs (y : ys)
      GT -> do
        outputCol2 y
        go (x : xs) ys
      EQ -> do
        outputCol3 x
        go xs ys

    outputCol1 line
      | optSuppress1 opts = return ()
      | otherwise = C8.hPutStrLn stdout line

    outputCol2 line
      | optSuppress2 opts = return ()
      | otherwise = do
          let prefix = if optSuppress1 opts then C8.empty else delim
          C8.hPutStrLn stdout (prefix <> line)

    outputCol3 line
      | optSuppress3 opts = return ()
      | otherwise = do
          let prefix1 = if optSuppress1 opts then C8.empty else delim
              prefix2 = if optSuppress2 opts then C8.empty else delim
          C8.hPutStrLn stdout (prefix1 <> prefix2 <> line)

readInputFile :: FilePath -> IO (Either String C8.ByteString)
readInputFile path = catch (Right <$> readFile') handler
  where
    readFile' =
      if path == "-"
        then BS.hGetContents stdin
        else BS.readFile path

    handler :: IOException -> IO (Either String C8.ByteString)
    handler e = return $ Left $ path ++ ": " ++ friendlyError (show e)

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

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox comm [OPTION]... FILE1 FILE2",
        "Compare sorted files FILE1 and FILE2 line by line.",
        "",
        "When FILE1 or FILE2 (not both) is -, read standard input.",
        "",
        "With no options, produce three-column output. Column one contains",
        "lines unique to FILE1, column two contains lines unique to FILE2,",
        "and column three contains lines common to both files.",
        "",
        "  -1              suppress column 1 (lines unique to FILE1)",
        "  -2              suppress column 2 (lines unique to FILE2)",
        "  -3              suppress column 3 (lines that appear in both)",
        "      --check-order     check input is sorted (default)",
        "      --nocheck-order   do not check input is sorted",
        "      --output-delimiter=STR  separate columns with STR",
        "      --help            display this help and exit",
        "      --version         output version information and exit"
      ]
