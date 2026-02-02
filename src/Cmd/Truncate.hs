{-# LANGUAGE OverloadedStrings #-}

module Cmd.Truncate (run) where

import Control.Exception (IOException, catch)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IOMode (..), hFileSize, hPutStrLn, hSetFileSize, stderr, withFile)
import System.Posix.Files (fileExist)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox truncate: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "truncate") >> exitSuccess
    | isNothing (optSize opts) && isNothing (optReference opts) -> do
        hPutStrLn stderr "haskbox truncate: you must specify either --size or --reference"
        exitFailure
    | null files -> do
        hPutStrLn stderr "haskbox truncate: missing file operand"
        hPutStrLn stderr "Try 'haskbox truncate --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (truncateFile opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optSize :: !(Maybe SizeSpec),
    optReference :: !(Maybe FilePath),
    optNoCreate :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

data SizeSpec
  = Absolute !Integer
  | Relative !Integer -- +N or -N
  | Modulo !Integer -- %N (round down to multiple)
  | AtLeast !Integer -- <N
  | AtMost !Integer -- >N
  deriving (Show, Eq)

defaultOpts :: Opts
defaultOpts =
  Opts
    { optSize = Nothing,
      optReference = Nothing,
      optNoCreate = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--no-create" : rest) = parseArgs opts {optNoCreate = True} rest
parseArgs opts ("-c" : rest) = parseArgs opts {optNoCreate = True} rest
parseArgs opts (('-' : 's' : s) : rest)
  | null s = case rest of
      (size : rest') -> case parseSize size of
        Just sz -> parseArgs opts {optSize = Just sz} rest'
        Nothing -> Left $ "invalid size: '" ++ size ++ "'"
      [] -> Left "option requires an argument -- 's'"
  | otherwise = case parseSize s of
      Just sz -> parseArgs opts {optSize = Just sz} rest
      Nothing -> Left $ "invalid size: '" ++ s ++ "'"
parseArgs opts ("--size" : s : rest) = case parseSize s of
  Just sz -> parseArgs opts {optSize = Just sz} rest
  Nothing -> Left $ "invalid size: '" ++ s ++ "'"
parseArgs opts (('-' : 'r' : r) : rest)
  | null r = case rest of
      (ref : rest') -> parseArgs opts {optReference = Just ref} rest'
      [] -> Left "option requires an argument -- 'r'"
  | otherwise = parseArgs opts {optReference = Just r} rest
parseArgs opts ("--reference" : r : rest) = parseArgs opts {optReference = Just r} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

parseSize :: String -> Maybe SizeSpec
parseSize ('+' : rest) = Relative . fromIntegral <$> parseSizeNum rest
parseSize ('-' : rest) = Relative . negate . fromIntegral <$> parseSizeNum rest
parseSize ('%' : rest) = Modulo . fromIntegral <$> parseSizeNum rest
parseSize ('<' : rest) = AtLeast . fromIntegral <$> parseSizeNum rest
parseSize ('>' : rest) = AtMost . fromIntegral <$> parseSizeNum rest
parseSize s = Absolute . fromIntegral <$> parseSizeNum s

parseSizeNum :: String -> Maybe Integer
parseSizeNum s =
  let (numPart, suffix) = span isDigit s
   in if null numPart
        then Nothing
        else case reads numPart of
          [(n, "")] -> Just $ n * multiplier suffix
          _ -> Nothing
  where
    multiplier "" = 1
    multiplier "K" = 1024
    multiplier "KB" = 1000
    multiplier "M" = 1024 * 1024
    multiplier "MB" = 1000 * 1000
    multiplier "G" = 1024 * 1024 * 1024
    multiplier "GB" = 1000 * 1000 * 1000
    multiplier "T" = 1024 * 1024 * 1024 * 1024
    multiplier "TB" = 1000 * 1000 * 1000 * 1000
    multiplier _ = 1

truncateFile :: Opts -> FilePath -> IO Bool
truncateFile opts path = do
  exists <- fileExist path
  if not exists && optNoCreate opts
    then return False
    else catch (doTruncate exists) handler
  where
    doTruncate exists = do
      -- Get reference size if needed
      refSize <- case optReference opts of
        Just ref -> withFile ref ReadMode hFileSize
        Nothing -> return 0

      -- Calculate target size
      currentSize <-
        if exists
          then withFile path ReadMode hFileSize
          else return 0

      let baseSize = case optReference opts of
            Just _ -> refSize
            Nothing -> 0

      targetSize <- case optSize opts of
        Just (Absolute n) -> return n
        Just (Relative n) -> return $ currentSize + n
        Just (Modulo n) -> return $ (currentSize `div` n) * n
        Just (AtLeast n) -> return $ max currentSize n
        Just (AtMost n) -> return $ min currentSize n
        Nothing -> return baseSize

      -- Truncate/extend the file
      withFile path ReadWriteMode $ \h -> hSetFileSize h targetSize
      return False

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox truncate: cannot truncate '" ++ path ++ "': " ++ friendlyError (show e)
      return True

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

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox truncate [OPTION]... FILE...",
        "Shrink or extend the size of each FILE to the specified size.",
        "",
        "  -c, --no-create        do not create any files",
        "  -r, --reference=RFILE  base size on RFILE",
        "  -s, --size=SIZE        set or adjust the file size by SIZE bytes",
        "      --help             display this help and exit",
        "      --version          output version information and exit",
        "",
        "SIZE may have a suffix: K (1024), M (1024^2), G (1024^3), T (1024^4)",
        "SIZE may also be prefixed by one of:",
        "  +  extend by SIZE",
        "  -  reduce by SIZE",
        "  <  at most SIZE",
        "  >  at least SIZE",
        "  %  round down to multiple of SIZE"
      ]
