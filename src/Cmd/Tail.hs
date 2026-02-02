{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Tail (run) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Word (Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IOMode (..), SeekMode (..), hFileSize, hPutStrLn, hSeek, openFile, stderr, stdin, stdout)
import System.Posix.Files (fileExist, getFileStatus)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox tail: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "tail") >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
            showHeaders = optVerbose opts || (not (optQuiet opts) && length paths > 1)
        hasError <- tailFiles opts showHeaders paths
        when hasError exitFailure

data Opts = Opts
  { optLines :: !Int,
    optBytes :: !(Maybe Int),
    optFromStart :: !Bool,
    optFollow :: !Bool,
    optFollowName :: !Bool,
    optRetry :: !Bool,
    optSleepInterval :: !Double,
    optQuiet :: !Bool,
    optVerbose :: !Bool,
    optZeroTerminated :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optLines = 10,
      optBytes = Nothing,
      optFromStart = False,
      optFollow = False,
      optFollowName = False,
      optRetry = False,
      optSleepInterval = 1.0,
      optQuiet = False,
      optVerbose = False,
      optZeroTerminated = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--quiet" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--silent" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("--follow" : rest) = parseArgs opts {optFollow = True} rest
parseArgs opts ("--follow=name" : rest) = parseArgs opts {optFollow = True, optFollowName = True} rest
parseArgs opts ("--follow=descriptor" : rest) = parseArgs opts {optFollow = True, optFollowName = False} rest
parseArgs opts ("--retry" : rest) = parseArgs opts {optRetry = True} rest
parseArgs opts ("--zero-terminated" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--lines" : n : rest) = case parseNumWithPlus n of
  Just (num, fromStart) -> parseArgs opts {optLines = num, optFromStart = fromStart} rest
  Nothing -> Left $ "invalid number of lines: '" ++ n ++ "'"
parseArgs opts ("--bytes" : n : rest) = case parseNumWithPlus n of
  Just (num, fromStart) -> parseArgs opts {optBytes = Just num, optFromStart = fromStart} rest
  Nothing -> Left $ "invalid number of bytes: '" ++ n ++ "'"
parseArgs opts (('-' : 's' : s) : rest)
  | null s = case rest of
      (num : rest') -> case reads num of
        [(n, "")] -> parseArgs opts {optSleepInterval = n} rest'
        _ -> Left $ "invalid sleep interval: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 's'"
  | otherwise = case reads s of
      [(n, "")] -> parseArgs opts {optSleepInterval = n} rest
      _ -> Left $ "invalid sleep interval: '" ++ s ++ "'"
parseArgs opts (('-' : 'n' : n) : rest)
  | null n = case rest of
      (num : rest') -> case parseNumWithPlus num of
        Just (x, fromStart) -> parseArgs opts {optLines = x, optFromStart = fromStart} rest'
        Nothing -> Left $ "invalid number of lines: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'n'"
  | otherwise = case parseNumWithPlus n of
      Just (num, fromStart) -> parseArgs opts {optLines = num, optFromStart = fromStart} rest
      Nothing -> Left $ "invalid number of lines: '" ++ n ++ "'"
parseArgs opts (('-' : 'c' : n) : rest)
  | null n = case rest of
      (num : rest') -> case parseNumWithPlus num of
        Just (x, fromStart) -> parseArgs opts {optBytes = Just x, optFromStart = fromStart} rest'
        Nothing -> Left $ "invalid number of bytes: '" ++ num ++ "'"
      [] -> Left "option requires an argument -- 'c'"
  | otherwise = case parseNumWithPlus n of
      Just (num, fromStart) -> parseArgs opts {optBytes = Just num, optFromStart = fromStart} rest
      Nothing -> Left $ "invalid number of bytes: '" ++ n ++ "'"
parseArgs opts ("-f" : rest) = parseArgs opts {optFollow = True} rest
parseArgs opts ("-F" : rest) = parseArgs opts {optFollow = True, optFollowName = True, optRetry = True} rest
parseArgs opts ("-q" : rest) = parseArgs opts {optQuiet = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZeroTerminated = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (c : cs) r = case c of
      'f' -> parseShortFlags o {optFollow = True} cs r
      'F' -> parseShortFlags o {optFollow = True, optFollowName = True, optRetry = True} cs r
      'q' -> parseShortFlags o {optQuiet = True} cs r
      'v' -> parseShortFlags o {optVerbose = True} cs r
      'z' -> parseShortFlags o {optZeroTerminated = True} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (file : rest) = do
  (o, files) <- parseArgs opts rest
  Right (o, file : files)

parseNumWithPlus :: String -> Maybe (Int, Bool)
parseNumWithPlus ('+' : s)
  | all isDigit s && not (null s) = Just (read s, True)
  | otherwise = Nothing
parseNumWithPlus s
  | all isDigit s && not (null s) = Just (read s, False)
  | otherwise = Nothing

tailFiles :: Opts -> Bool -> [FilePath] -> IO Bool
tailFiles opts showHeaders paths = do
  let go _ [] = return False
      go !isFirst (p : ps) = do
        when showHeaders $ do
          unless isFirst $ C8.hPut stdout "\n"
          C8.hPut stdout $ "==> " <> C8.pack p <> " <==\n"
        err <- tailFile opts p
        errRest <- go False ps
        return (err || errRest)
  hasError <- go True paths
  -- If following and there's exactly one file, follow it
  when (optFollow opts && not hasError) $ case paths of
    [singlePath] -> followFile opts singlePath
    _ -> followMultipleFiles opts showHeaders paths
  return hasError

tailFile :: Opts -> FilePath -> IO Bool
tailFile opts path = catch (tailFile' opts path >> return False) handler
  where
    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox tail: " ++ path ++ ": " ++ friendlyError (show e)
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

tailFile' :: Opts -> FilePath -> IO ()
tailFile' opts path = do
  contents <-
    if path == "-"
      then BS.hGetContents stdin
      else BS.readFile path
  let delim = if optZeroTerminated opts then 0 else 10
  case optBytes opts of
    Just n
      | optFromStart opts -> BS.hPut stdout (BS.drop (n - 1) contents)
      | otherwise -> BS.hPut stdout (BS.drop (BS.length contents - n) contents)
    Nothing -> do
      let !n = optLines opts
      if optFromStart opts
        then BS.hPut stdout (dropLines delim (n - 1) contents)
        else BS.hPut stdout (takeLastLines delim n contents)

dropLines :: Word8 -> Int -> BS.ByteString -> BS.ByteString
dropLines _ 0 bs = bs
dropLines delim n bs = case BS.elemIndex delim bs of
  Nothing -> BS.empty
  Just i -> dropLines delim (n - 1) (BS.drop (i + 1) bs)

takeLastLines :: Word8 -> Int -> BS.ByteString -> BS.ByteString
takeLastLines _ n _ | n <= 0 = BS.empty
takeLastLines delim n bs =
  let positions = findDelimPositions delim bs
      total = length positions
   in if total <= n
        then bs
        else BS.drop (positions !! (total - n - 1) + 1) bs

findDelimPositions :: Word8 -> BS.ByteString -> [Int]
findDelimPositions delim bs = go 0
  where
    go !pos
      | pos >= BS.length bs = []
      | otherwise = case BS.elemIndex delim (BS.drop pos bs) of
          Nothing -> []
          Just i -> (pos + i) : go (pos + i + 1)

-- Follow a single file for new content
followFile :: Opts -> FilePath -> IO ()
followFile opts path
  | path == "-" = return () -- Cannot follow stdin
  | otherwise = do
      let sleepMicros = round (optSleepInterval opts * 1000000)
      catch (followLoop sleepMicros Nothing) handler
  where
    followLoop sleepMicros lastSize = do
      exists <- fileExist path
      if exists
        then do
          h <- openFile path ReadMode
          currentSize <- hFileSize h
          case lastSize of
            Nothing -> do
              -- First iteration after initial tail, seek to end
              hSeek h SeekFromEnd 0
            Just prevSize ->
              if currentSize < prevSize
                then do
                  -- File was truncated, start from beginning
                  hPutStrLn stderr $ "haskbox tail: " ++ path ++ ": file truncated"
                  hSeek h AbsoluteSeek 0
                else hSeek h AbsoluteSeek prevSize
          -- Read and output new content
          newContent <- BS.hGetContents h
          unless (BS.null newContent) $ BS.hPut stdout newContent
          threadDelay sleepMicros
          followLoop sleepMicros (Just currentSize)
        else do
          when (optRetry opts) $ do
            threadDelay sleepMicros
            followLoop sleepMicros lastSize

    handler :: IOException -> IO ()
    handler _ = when (optRetry opts) $ do
      threadDelay (round (optSleepInterval opts * 1000000))
      followFile opts path

-- Follow multiple files
followMultipleFiles :: Opts -> Bool -> [FilePath] -> IO ()
followMultipleFiles opts showHeaders paths = do
  let sleepMicros = round (optSleepInterval opts * 1000000)
      initialSizes = replicate (length paths) Nothing
  followLoop sleepMicros Nothing initialSizes
  where
    followLoop sleepMicros lastShown sizes = do
      (newSizes, newLastShown) <- checkFiles lastShown sizes
      threadDelay sleepMicros
      followLoop sleepMicros newLastShown newSizes

    checkFiles lastShown sizes
      | null paths = return (sizes, lastShown)
      | otherwise = go lastShown sizes (zip3 paths sizes ([0 ..] :: [Int]))

    go lastShown sizes [] = return (sizes, lastShown)
    go lastShown sizes ((path, prevSize, i) : rest) = do
      exists <- catch (fileExist path) (\(_ :: IOException) -> return False)
      if exists
        then do
          status <- catch (Just <$> getFileStatus path) (\(_ :: IOException) -> return Nothing)
          case status of
            Nothing -> go lastShown sizes rest
            Just _st -> do
              h <- catch (Just <$> openFile path ReadMode) (\(_ :: IOException) -> return Nothing)
              case h of
                Nothing -> go lastShown sizes rest
                Just handle -> do
                  currentSize <- hFileSize handle
                  let shouldRead = case prevSize of
                        Nothing -> False
                        Just prev -> currentSize > prev
                  newContent <-
                    if shouldRead
                      then do
                        forM_ prevSize (hSeek handle AbsoluteSeek)
                        BS.hGetContents handle
                      else return BS.empty
                  unless (BS.null newContent) $ do
                    when (showHeaders && lastShown /= Just path) $ do
                      when (isJust lastShown) $ C8.hPut stdout "\n"
                      C8.hPut stdout $ "==> " <> C8.pack path <> " <==\n"
                    BS.hPut stdout newContent
                  let newSizes = take i sizes ++ [Just currentSize] ++ drop (i + 1) sizes
                      newLastShown = if BS.null newContent then lastShown else Just path
                  go newLastShown newSizes rest
        else go lastShown sizes rest

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox tail [OPTION]... [FILE]...",
        "Print the last 10 lines of each FILE to standard output.",
        "With more than one FILE, precede each with a header giving the file name.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "  -c, --bytes=[+]NUM       output the last NUM bytes; or use -c +NUM to",
        "                             output starting with byte NUM of each file",
        "  -f, --follow[={name|descriptor}]",
        "                           output appended data as the file grows;",
        "                             an absent option argument means 'descriptor'",
        "  -F                       same as --follow=name --retry",
        "  -n, --lines=[+]NUM       output the last NUM lines, instead of the last 10;",
        "                             or use -n +NUM to skip NUM-1 lines at the start",
        "      --retry              keep trying to open a file if it is inaccessible",
        "  -s, --sleep-interval=N   with -f, sleep for approximately N seconds",
        "                             (default 1.0) between iterations",
        "  -q, --quiet, --silent    never output headers giving file names",
        "  -v, --verbose            always output headers giving file names",
        "  -z, --zero-terminated    line delimiter is NUL, not newline",
        "      --help               display this help and exit",
        "      --version            output version information and exit"
      ]
