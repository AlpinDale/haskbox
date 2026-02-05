{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Cmd.Df.Mount
  ( getMountedFilesystems,
    getFilesystemInfo,
  )
where

import Cmd.Df.Types
import Control.Exception (IOException, try)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peekByteOff)

#if defined(darwin_HOST_OS)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CLong (..), CUInt (..), CULong (..))
import Foreign.Ptr (Ptr, plusPtr)
import System.Exit (ExitCode (..))
import System.Process qualified
#else
import Data.Char (isOctDigit)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CULong (..))
import Foreign.Ptr (Ptr)
#endif

#if defined(darwin_HOST_OS)

-- macOS: Use statfs

-- | Size of struct statfs on macOS arm64
statFsSize :: Int
statFsSize = 2168

foreign import ccall unsafe "sys/mount.h statfs"
  c_statfs :: CString -> Ptr () -> IO CInt

-- | Get all mounted filesystems (macOS)
-- Parse output of mount command for simplicity
getMountedFilesystems :: IO [FsInfo]
getMountedFilesystems = do
  result <- try $ readProcess "mount" [] "" :: IO (Either IOException String)
  case result of
    Left _ -> return []
    Right output -> do
      let entries = mapMaybe parseMountLine (lines output)
      infos <- mapM (\(dev, mp, ft) -> getFilesystemInfoWithMeta dev ft mp) entries
      return $ rights infos
  where
    -- Parse: /dev/disk1s1 on / (apfs, local, ...)
    parseMountLine :: String -> Maybe (String, String, String)
    parseMountLine line = case words line of
      (dev : "on" : rest) -> case break (== '(') (unwords rest) of
        (mp, '(' : fsRest) -> case break (== ',') fsRest of
          (ft, _) -> Just (dev, trimTrailing mp, filter (/= ')') ft)
        _ -> Nothing
      _ -> Nothing

    trimTrailing = reverse . dropWhile (== ' ') . reverse

-- | Get filesystem info with device and type already known
getFilesystemInfoWithMeta :: String -> String -> FilePath -> IO (Either String FsInfo)
getFilesystemInfoWithMeta dev fstype path = allocaBytes statFsSize $ \ptr -> do
  result <- withCString path $ \cpath -> c_statfs cpath ptr
  if result /= 0
    then return $ Left $ "statfs failed for " ++ path
    else do
      bsize <- peekByteOff ptr 0 :: IO CUInt  -- f_bsize is uint32_t (4 bytes)
      blocks <- peekByteOff ptr 8 :: IO CULong  -- offset 8
      bfree <- peekByteOff ptr 16 :: IO CULong  -- offset 16
      bavail <- peekByteOff ptr 24 :: IO CLong  -- offset 24
      files <- peekByteOff ptr 32 :: IO CULong  -- offset 32
      ffree <- peekByteOff ptr 40 :: IO CLong   -- offset 40
      return $ Right $ FsInfo
        { fsDevice = dev,
          fsType = fstype,
          fsMountPoint = path,
          fsBlockSize = fromIntegral bsize,
          fsTotalBlocks = fromIntegral blocks,
          fsFreeBlocks = fromIntegral bfree,
          fsAvailBlocks = fromIntegral bavail,
          fsTotalInodes = fromIntegral files,
          fsFreeInodes = fromIntegral ffree
        }

-- | Get filesystem info for a specific path (macOS)
getFilesystemInfo :: FilePath -> IO (Either String FsInfo)
getFilesystemInfo path = allocaBytes statFsSize $ \ptr -> do
  result <- withCString path $ \cpath -> c_statfs cpath ptr
  if result /= 0
    then return $ Left $ "statfs failed for " ++ path
    else do
      bsize <- peekByteOff ptr 0 :: IO CUInt  -- f_bsize is uint32_t (4 bytes)
      blocks <- peekByteOff ptr 8 :: IO CULong  -- offset 8
      bfree <- peekByteOff ptr 16 :: IO CULong  -- offset 16
      bavail <- peekByteOff ptr 24 :: IO CLong  -- offset 24
      files <- peekByteOff ptr 32 :: IO CULong  -- offset 32
      ffree <- peekByteOff ptr 40 :: IO CLong   -- offset 40
      -- Read fstypename at offset 72 (16 bytes)
      fstypename <- peekCStringOff ptr 72
      -- Read mntonname at offset 88 (1024 bytes)
      mntonname <- peekCStringOff ptr 88
      -- Read mntfromname at offset 1112 (1024 bytes)
      mntfromname <- peekCStringOff ptr 1112
      return $ Right $ FsInfo
        { fsDevice = mntfromname,
          fsType = fstypename,
          fsMountPoint = mntonname,
          fsBlockSize = fromIntegral bsize,
          fsTotalBlocks = fromIntegral blocks,
          fsFreeBlocks = fromIntegral bfree,
          fsAvailBlocks = fromIntegral bavail,
          fsTotalInodes = fromIntegral files,
          fsFreeInodes = fromIntegral ffree
        }
  where
    peekCStringOff :: Ptr () -> Int -> IO String
    peekCStringOff p off = peekCString (p `plusPtr` off)

readProcess :: FilePath -> [String] -> String -> IO String
readProcess cmd args input = do
  readResult <- try $ System.Process.readProcessWithExitCode cmd args input :: IO (Either IOException (ExitCode, String, String))
  case readResult of
    Right (_, out, _) -> return out
    Left _ -> return ""

#else

-- Linux: Use statvfs and parse /proc/mounts

-- | Size of struct statvfs on Linux x86_64
statVfsSize :: Int
statVfsSize = 112

foreign import ccall unsafe "sys/statvfs.h statvfs"
  c_statvfs :: CString -> Ptr () -> IO CInt

-- | Get all mounted filesystems (Linux)
getMountedFilesystems :: IO [FsInfo]
getMountedFilesystems = do
  result <- try $ readFile "/proc/mounts" :: IO (Either IOException String)
  case result of
    Left _ -> tryMtab
    Right contents -> parseAndGetInfo contents
  where
    tryMtab = do
      mtabResult <- try $ readFile "/etc/mtab" :: IO (Either IOException String)
      case mtabResult of
        Left _ -> return []
        Right contents -> parseAndGetInfo contents

    parseAndGetInfo contents = do
      let entries = mapMaybe parseMountLine (lines contents)
      infos <- mapM getInfoForEntry entries
      return $ rights infos

    parseMountLine :: String -> Maybe (String, String, String)
    parseMountLine line = case words line of
      (dev : mount : fstype : _) -> Just (unescapePath dev, unescapePath mount, fstype)
      _ -> Nothing

    -- Unescape octal sequences in paths (e.g., \040 for space)
    unescapePath :: String -> String
    unescapePath [] = []
    unescapePath ('\\' : d1 : d2 : d3 : rest)
      | all isOctDigit [d1, d2, d3] =
          toEnum (read ['0', 'o', d1, d2, d3]) : unescapePath rest
    unescapePath (c : rest) = c : unescapePath rest

    getInfoForEntry (dev, mount, fstype) = do
      infoResult <- getFilesystemInfo mount
      case infoResult of
        Right info -> return $ Right info {fsDevice = dev, fsType = fstype}
        Left err -> return $ Left err

-- | Get filesystem info for a specific path (Linux)
getFilesystemInfo :: FilePath -> IO (Either String FsInfo)
getFilesystemInfo path = allocaBytes statVfsSize $ \ptr -> do
  result <- withCString path $ \cpath -> c_statvfs cpath ptr
  if result /= 0
    then return $ Left $ "statvfs failed for " ++ path
    else do
      bsize <- peekByteOff ptr 0 :: IO CULong
      frsize <- peekByteOff ptr 8 :: IO CULong
      blocks <- peekByteOff ptr 16 :: IO CULong
      bfree <- peekByteOff ptr 24 :: IO CULong
      bavail <- peekByteOff ptr 32 :: IO CULong
      files <- peekByteOff ptr 40 :: IO CULong
      ffree <- peekByteOff ptr 48 :: IO CULong
      return $ Right $ FsInfo
        { fsDevice = "",
          fsType = "",
          fsMountPoint = path,
          fsBlockSize = fromIntegral (if frsize > 0 then frsize else bsize),
          fsTotalBlocks = fromIntegral blocks,
          fsFreeBlocks = fromIntegral bfree,
          fsAvailBlocks = fromIntegral bavail,
          fsTotalInodes = fromIntegral files,
          fsFreeInodes = fromIntegral ffree
        }

#endif

-- Helper functions

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

rights :: [Either a b] -> [b]
rights [] = []
rights (Left _ : xs) = rights xs
rights (Right x : xs) = x : rights xs
