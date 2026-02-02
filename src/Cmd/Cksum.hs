{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Cksum (run) where

import Control.Exception (IOException, catch)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.ByteString qualified as BS
import Data.Word (Word32, Word8)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "cksum") >> exitSuccess
  [] -> checksumFile "-"
  files -> mapM_ checksumFile files

checksumFile :: FilePath -> IO ()
checksumFile path = catch doChecksum handler
  where
    doChecksum = do
      contents <-
        if path == "-"
          then BS.hGetContents stdin
          else BS.readFile path
      let len = BS.length contents
          crc = crc32 contents
      if path == "-"
        then putStrLn $ show crc ++ " " ++ show len
        else putStrLn $ show crc ++ " " ++ show len ++ " " ++ path

    handler :: IOException -> IO ()
    handler e = do
      hPutStrLn stderr $ "haskbox cksum: " ++ path ++ ": " ++ friendlyError (show e)
      exitFailure

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

-- | CRC-32 implementation (POSIX cksum algorithm)
crc32 :: BS.ByteString -> Word32
crc32 bs =
  let -- Process all bytes
      crc1 = BS.foldl' updateCrc 0 bs
      -- Process length
      len = BS.length bs
      crc2 = processLength crc1 len
   in -- Final XOR
      crc2 `xor` 0xFFFFFFFF

updateCrc :: Word32 -> Word8 -> Word32
updateCrc crc byte =
  let idx = fromIntegral ((crc `shiftR` 24) `xor` fromIntegral byte) :: Int
   in (crc `shiftL` 8) `xor` (crcTable ! idx)

processLength :: Word32 -> Int -> Word32
processLength = go
  where
    go !c 0 = c
    go !c n =
      let byte = fromIntegral (n .&. 0xFF)
          idx = fromIntegral ((c `shiftR` 24) `xor` byte) :: Int
          c' = (c `shiftL` 8) `xor` (crcTable ! idx)
       in go c' (n `shiftR` 8)

-- | CRC-32 lookup table (POSIX polynomial)
crcTable :: UArray Int Word32
crcTable = listArray (0, 255) [makeCrcEntry i | i <- [0 .. 255]]
  where
    makeCrcEntry :: Int -> Word32
    makeCrcEntry n = loop (8 :: Int) (fromIntegral n `shiftL` 24)
      where
        loop 0 !c = c
        loop i !c
          | c .&. 0x80000000 /= 0 = loop (i - 1) ((c `shiftL` 1) `xor` 0x04C11DB7)
          | otherwise = loop (i - 1) (c `shiftL` 1)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox cksum [FILE]...",
        "Print CRC checksum and byte counts of each FILE.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
