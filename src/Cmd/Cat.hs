{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Cat (run) where

import Control.Exception (IOException, catch, finally)
import Control.Monad (foldM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.IORef
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.IO.FD qualified as FD
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd (..))
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs args of
  Left err -> do
    hPutStrLn stderr $ "haskbox cat: " ++ err
    exitFailure
  Right (opts, files)
    | optShowVersion opts -> putStrLn (versionString "cat") >> exitSuccess
    | optShowHelp opts -> printHelp >> exitSuccess
    | otherwise -> do
        let paths = if null files then ["-"] else files
        hasError <- catFilesWithErrors opts paths
        when hasError exitFailure

data Options = Options
  { optNumberNonblank :: !Bool,
    optShowEnds :: !Bool,
    optNumber :: !Bool,
    optSqueezeBlank :: !Bool,
    optShowTabs :: !Bool,
    optShowNonprint :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool,
    optFiles :: ![FilePath]
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optNumberNonblank = False,
      optShowEnds = False,
      optNumber = False,
      optSqueezeBlank = False,
      optShowTabs = False,
      optShowNonprint = False,
      optShowHelp = False,
      optShowVersion = False,
      optFiles = []
    }

parseArgs :: [String] -> Either String (Options, [FilePath])
parseArgs = go defaultOptions
  where
    go opts [] = Right (opts, reverse $ optFiles opts)
    go opts ("--" : rest) = Right (opts {optFiles = reverse rest ++ optFiles opts}, [])
    go opts ("--help" : rest) = go opts {optShowHelp = True} rest
    go opts ("--version" : rest) = go opts {optShowVersion = True} rest
    go opts ("--show-all" : rest) = go (setA opts) rest
    go opts ("--number-nonblank" : rest) = go (setB opts) rest
    go opts ("--show-ends" : rest) = go (setE opts) rest
    go opts ("--number" : rest) = go (setN opts) rest
    go opts ("--squeeze-blank" : rest) = go (setS opts) rest
    go opts ("--show-tabs" : rest) = go (setT opts) rest
    go opts ("--show-nonprinting" : rest) = go (setV opts) rest
    go _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
    go opts (('-' : shorts) : rest)
      | null shorts = go opts {optFiles = "-" : optFiles opts} rest
      | otherwise = case parseShorts opts shorts of
          Left e -> Left e
          Right opts' -> go opts' rest
    go opts (file : rest) = go opts {optFiles = file : optFiles opts} rest

    parseShorts opts [] = Right opts
    parseShorts opts (c : cs) = case c of
      'A' -> parseShorts (setA opts) cs
      'b' -> parseShorts (setB opts) cs
      'e' -> parseShorts (setVE opts) cs
      'E' -> parseShorts (setE opts) cs
      'n' -> parseShorts (setN opts) cs
      's' -> parseShorts (setS opts) cs
      't' -> parseShorts (setVT opts) cs
      'T' -> parseShorts (setT opts) cs
      'u' -> parseShorts opts cs
      'v' -> parseShorts (setV opts) cs
      'h' -> parseShorts opts {optShowHelp = True} cs
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"

    setA o = o {optShowEnds = True, optShowTabs = True, optShowNonprint = True}
    setB o = o {optNumberNonblank = True}
    setE o = o {optShowEnds = True}
    setN o = o {optNumber = True}
    setS o = o {optSqueezeBlank = True}
    setT o = o {optShowTabs = True}
    setV o = o {optShowNonprint = True}
    setVE o = setV (setE o)
    setVT o = setV (setT o)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox cat [OPTION]... [FILE]...",
        "Concatenate FILE(s) to standard output.",
        "",
        "  -A, --show-all           equivalent to -vET",
        "  -b, --number-nonblank    number nonempty output lines",
        "  -e                       equivalent to -vE",
        "  -E, --show-ends          display $ at end of each line",
        "  -n, --number             number all output lines",
        "  -s, --squeeze-blank      suppress repeated empty output lines",
        "  -t                       equivalent to -vT",
        "  -T, --show-tabs          display TAB characters as ^I",
        "  -u                       (ignored)",
        "  -v, --show-nonprinting   use ^ and M- notation",
        "      --help               display this help and exit",
        "      --version            output version information and exit",
        "",
        "With no FILE, or when FILE is -, read standard input."
      ]

-- Core implementation

data CatState = CatState
  { lineNumber :: !Int,
    prevWasBlank :: !Bool,
    atLineStart :: !Bool,
    hadError :: !Bool
  }

{-# INLINE initialState #-}
initialState :: CatState
initialState = CatState 1 False True False

catFilesWithErrors :: Options -> [FilePath] -> IO Bool
catFilesWithErrors opts files = hadError <$> foldM (processFileWithError opts) initialState files

{-# INLINE needsProcessing #-}
needsProcessing :: Options -> Bool
needsProcessing opts =
  optNumberNonblank opts
    || optShowEnds opts
    || optNumber opts
    || optSqueezeBlank opts
    || optShowTabs opts
    || optShowNonprint opts

processFile :: Options -> CatState -> FilePath -> IO CatState
processFile opts state path
  | not (needsProcessing opts) = simpleCat path >> pure state
  | otherwise = processWithOptions opts state path

bufSize :: Int
bufSize = 131072

simpleCat :: FilePath -> IO ()
simpleCat "-" = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  copyHandleRaw stdin stdout
simpleCat path = do
  hSetBinaryMode stdout True
  fd <- openFd path ReadOnly defaultFileFlags
  copyFdToStdout fd `finally` closeFd fd

copyFdToStdout :: Fd -> IO ()
copyFdToStdout (Fd srcFd) = do
  let !srcFD = FD.FD {FD.fdFD = srcFd, FD.fdIsNonBlocking = 0}
      !dstFD = FD.FD {FD.fdFD = 1, FD.fdIsNonBlocking = 0}
  allocaBytes bufSize $ \buf -> copyLoop srcFD dstFD buf

copyLoop :: FD.FD -> FD.FD -> Ptr Word8 -> IO ()
copyLoop !srcFD !dstFD !buf = go
  where
    go = do
      n <- FD.readRawBufferPtr "read" srcFD buf 0 (fromIntegral bufSize)
      when (n > 0) $ do
        writeAll dstFD buf n
        go

writeAll :: FD.FD -> Ptr Word8 -> Int -> IO ()
writeAll !fd !buf !len = go buf (fromIntegral len)
  where
    go !p !remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          written <- FD.writeRawBufferPtr "write" fd p 0 (fromIntegral remaining)
          go (p `plusPtr` fromIntegral written) (remaining - written)

copyHandleRaw :: Handle -> Handle -> IO ()
copyHandleRaw src dst =
  allocaBytes bufSize $ \buf -> do
    let go = do
          n <- hGetBufSome src buf bufSize
          when (n > 0) $ do
            hPutBuf dst buf n
            go
    go

processWithOptions :: Options -> CatState -> FilePath -> IO CatState
processWithOptions opts state path = do
  contents <-
    if path == "-"
      then do
        hSetBinaryMode stdin True
        BS.hGetContents stdin
      else BS.readFile path
  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering (Just 65536))
  processStrict opts state contents

processStrict :: Options -> CatState -> BS.ByteString -> IO CatState
processStrict opts state0 bs0 = do
  let !doNum = optNumber opts
      !doNumNonblank = optNumberNonblank opts
      !doSqueeze = optSqueezeBlank opts
      !doEnds = optShowEnds opts
      !doTabs = optShowTabs opts
      !doNonprint = optShowNonprint opts
      !needTransform = doTabs || doNonprint

  lineNumRef <- newIORef (lineNumber state0)
  prevBlankRef <- newIORef (prevWasBlank state0)
  atStartRef <- newIORef (atLineStart state0)

  let go !bs
        | BS.null bs = pure ()
        | otherwise = do
            let !nlIdx = BS.elemIndex 10 bs
            case nlIdx of
              Nothing -> do
                atStart <- readIORef atStartRef
                when atStart $ do
                  let !isBlank = BS.null bs
                  when (doNum || (doNumNonblank && not isBlank)) $ do
                    n <- readIORef lineNumRef
                    BS.hPut stdout (formatLineNum n)
                    writeIORef lineNumRef (n + 1)
                if needTransform
                  then BS.hPut stdout (transformBS doTabs doNonprint bs)
                  else BS.hPut stdout bs
                writeIORef atStartRef False
                writeIORef prevBlankRef (BS.null bs)
              Just !i -> do
                let !line = BU.unsafeTake i bs
                    !rest = BU.unsafeDrop (i + 1) bs
                    !isBlank = i == 0

                prevBlank <- readIORef prevBlankRef
                let !skip = doSqueeze && isBlank && prevBlank

                if skip
                  then writeIORef prevBlankRef True
                  else do
                    atStart <- readIORef atStartRef
                    when (atStart && (doNum || (doNumNonblank && not isBlank))) $ do
                      n <- readIORef lineNumRef
                      BS.hPut stdout (formatLineNum n)
                      writeIORef lineNumRef (n + 1)

                    if needTransform
                      then BS.hPut stdout (transformBS doTabs doNonprint line)
                      else BS.hPut stdout line

                    when doEnds $ BS.hPut stdout "$"
                    BS.hPut stdout "\n"
                    writeIORef prevBlankRef isBlank
                    writeIORef atStartRef True

                go rest

  go bs0
  hFlush stdout

  finalLineNum <- readIORef lineNumRef
  finalPrevBlank <- readIORef prevBlankRef
  finalAtStart <- readIORef atStartRef
  pure $
    CatState
      { lineNumber = finalLineNum,
        prevWasBlank = finalPrevBlank,
        atLineStart = finalAtStart,
        hadError = hadError state0
      }

{-# INLINE formatLineNum #-}
formatLineNum :: Int -> BS.ByteString
formatLineNum !n
  | n < 10 = padded1 n
  | n < 100 = padded2 n
  | n < 1000 = padded3 n
  | n < 10000 = padded4 n
  | n < 100000 = padded5 n
  | n < 1000000 = padded6 n
  | otherwise = BS.pack (padding ++ digits ++ [0x09])
  where
    padded1 x = BS.pack [0x20, 0x20, 0x20, 0x20, 0x20, d0 x, 0x09]
    padded2 x = BS.pack [0x20, 0x20, 0x20, 0x20, d1 x, d0 x, 0x09]
    padded3 x = BS.pack [0x20, 0x20, 0x20, d2 x, d1 x, d0 x, 0x09]
    padded4 x = BS.pack [0x20, 0x20, d3 x, d2 x, d1 x, d0 x, 0x09]
    padded5 x = BS.pack [0x20, d4 x, d3 x, d2 x, d1 x, d0 x, 0x09]
    padded6 x = BS.pack [d5 x, d4 x, d3 x, d2 x, d1 x, d0 x, 0x09]

    d0 x = 0x30 + fromIntegral (x `rem` 10)
    d1 x = 0x30 + fromIntegral ((x `quot` 10) `rem` 10)
    d2 x = 0x30 + fromIntegral ((x `quot` 100) `rem` 10)
    d3 x = 0x30 + fromIntegral ((x `quot` 1000) `rem` 10)
    d4 x = 0x30 + fromIntegral ((x `quot` 10000) `rem` 10)
    d5 x = 0x30 + fromIntegral ((x `quot` 100000) `rem` 10)

    digits = map (fromIntegral . (+ 48)) $ toDigits n
    padding = replicate (6 - length digits) 0x20
    toDigits 0 = [0]
    toDigits x = toDigitsGo x []
      where
        toDigitsGo 0 acc = acc
        toDigitsGo m acc = toDigitsGo (m `quot` 10) (m `rem` 10 : acc)

{-# INLINE transformBS #-}
transformBS :: Bool -> Bool -> BS.ByteString -> BS.ByteString
transformBS !doTabs !doNonprint bs
  | not doTabs && not doNonprint = bs
  | otherwise = BS.concatMap (transformByte doTabs doNonprint) bs

{-# INLINE transformByte #-}
transformByte :: Bool -> Bool -> Word8 -> BS.ByteString
transformByte !doTabs !doNonprint !b
  | b == 9 && doTabs = "^I"
  | doNonprint = showNonprinting b
  | otherwise = BS.singleton b

{-# INLINE showNonprinting #-}
showNonprinting :: Word8 -> BS.ByteString
showNonprinting !b
  | b >= 32 && b < 127 = BS.singleton b
  | b == 9 || b == 10 = BS.singleton b
  | b == 127 = "^?"
  | b < 32 = BS.pack [0x5E, b + 64]
  | otherwise =
      let !low = b - 128
       in if low < 32
            then BS.pack [0x4D, 0x2D, 0x5E, low + 64]
            else
              if low == 127
                then "M-^?"
                else BS.pack [0x4D, 0x2D, low]

processFileWithError :: Options -> CatState -> FilePath -> IO CatState
processFileWithError opts state path =
  catch (processFile opts state path) handler
  where
    handler :: IOException -> IO CatState
    handler e = do
      hPutStrLn stderr $ "haskbox cat: " ++ path ++ ": " ++ friendlyError (show e)
      pure state {hadError = True}

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s || "inappropriate type" `isIn` s = "Is a directory"
      | otherwise = dropPrefix s

    dropPrefix s = case dropWhile (/= ':') s of
      [] -> s
      (_ : rest) -> dropWhile (== ' ') rest

    isIn needle haystack = any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails s@(_ : xs) = s : tails xs
