{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cat
  ( CatOptions (..),
    defaultOptions,
    catFiles,
    catFilesWithErrors,
  )
where

import Control.Exception (IOException, catch, finally)
import Control.Monad (foldM, foldM_, when)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.IORef
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import GHC.IO.FD (FD (..))
import GHC.IO.Handle.FD (handleToFd)
import System.IO
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd (..))

data CatOptions = CatOptions
  { optNumberNonblank :: !Bool,
    optShowEnds :: !Bool,
    optNumber :: !Bool,
    optSqueezeBlank :: !Bool,
    optShowTabs :: !Bool,
    optShowNonprint :: !Bool
  }
  deriving (Show, Eq)

defaultOptions :: CatOptions
defaultOptions =
  CatOptions
    { optNumberNonblank = False,
      optShowEnds = False,
      optNumber = False,
      optSqueezeBlank = False,
      optShowTabs = False,
      optShowNonprint = False
    }

data CatState = CatState
  { lineNumber :: !Int,
    prevWasBlank :: !Bool,
    atLineStart :: !Bool,
    hadError :: !Bool
  }

{-# INLINE initialState #-}
initialState :: CatState
initialState = CatState 1 False True False

catFiles :: CatOptions -> [FilePath] -> IO ()
catFiles opts = foldM_ (processFile opts) initialState

catFilesWithErrors :: CatOptions -> [FilePath] -> IO Bool
catFilesWithErrors opts files = hadError <$> foldM (processFileWithError opts) initialState files

{-# INLINE needsProcessing #-}
needsProcessing :: CatOptions -> Bool
needsProcessing opts =
  optNumberNonblank opts
    || optShowEnds opts
    || optNumber opts
    || optSqueezeBlank opts
    || optShowTabs opts
    || optShowNonprint opts

processFile :: CatOptions -> CatState -> FilePath -> IO CatState
processFile opts state path
  | not (needsProcessing opts) = simpleCat path >> pure state
  | otherwise = processWithOptions opts state path

simpleCat :: FilePath -> IO ()
simpleCat "-" = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  copyHandle stdin stdout
simpleCat path = do
  hSetBinaryMode stdout True
  fd <- openFd path ReadOnly defaultFileFlags
  sendfileToStdout fd `finally` closeFd fd

foreign import ccall unsafe "copyfile.h direct_copy"
  c_direct_copy :: CInt -> CInt -> IO CInt

sendfileToStdout :: Fd -> IO ()
sendfileToStdout (Fd srcFd) = do
  FD {fdFD = dstFd} <- handleToFd stdout
  throwErrnoIfMinus1_ "direct_copy" $ c_direct_copy srcFd dstFd

copyHandle :: Handle -> Handle -> IO ()
copyHandle src dst = go
  where
    go = do
      chunk <- BS.hGetSome src 131072
      if BS.null chunk
        then pure ()
        else BS.hPut dst chunk >> go

processWithOptions :: CatOptions -> CatState -> FilePath -> IO CatState
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

processStrict :: CatOptions -> CatState -> BS.ByteString -> IO CatState
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
    toDigits x = go x []
      where
        go 0 acc = acc
        go m acc = go (m `quot` 10) (m `rem` 10 : acc)

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

processFileWithError :: CatOptions -> CatState -> FilePath -> IO CatState
processFileWithError opts state path =
  catch (processFile opts state path) handler
  where
    handler :: IOException -> IO CatState
    handler e = do
      hPutStrLn stderr $ "haskat: " ++ path ++ ": " ++ friendlyError (show e)
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
