{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Cmd.Ls.Stat
  ( getBirthTime,
  )
where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign
import Foreign.C

#if defined(linux_HOST_OS)

-- | statx structure on Linux
-- We only care about stx_btime (birth time)

-- statx syscall number varies by arch:
-- aarch64: 291
-- x86_64: 332
#if defined(__aarch64__) || defined(aarch64_HOST_ARCH)
foreign import ccall unsafe "syscall"
  c_syscall_statx :: CLong -> CInt -> CString -> CInt -> CUInt -> Ptr StatxBuf -> IO CInt

statx_syscall :: CLong
statx_syscall = 291
#else
foreign import ccall unsafe "syscall"
  c_syscall_statx :: CLong -> CInt -> CString -> CInt -> CUInt -> Ptr StatxBuf -> IO CInt

statx_syscall :: CLong
statx_syscall = 332
#endif

-- STATX_BTIME mask
statx_btime_mask :: CUInt
statx_btime_mask = 0x800

-- AT_FDCWD for relative paths
at_fdcwd :: CInt
at_fdcwd = -100

-- AT_SYMLINK_NOFOLLOW to not follow symlinks
at_symlink_nofollow :: CInt
at_symlink_nofollow = 0x100

-- Structure to receive statx data
-- We only need the birth time fields
-- Full structure is 256 bytes, but we only care about specific offsets:
-- Offset 72: stx_btime.tv_sec (8 bytes)
-- Offset 80: stx_btime.tv_nsec (4 bytes)
data StatxBuf = StatxBuf

instance Storable StatxBuf where
  sizeOf _ = 256
  alignment _ = 8
  peek _ = return StatxBuf
  poke _ _ = return ()

-- | Get birth time using statx syscall on Linux
getBirthTime :: FilePath -> IO (Maybe UTCTime)
getBirthTime path = do
  withCString path $ \cpath -> do
    allocaBytes 256 $ \buf -> do
      rc <- c_syscall_statx statx_syscall at_fdcwd cpath at_symlink_nofollow statx_btime_mask buf
      if rc == 0
        then do
          -- Read stx_btime.tv_sec at offset 80
          tvSec <- peekByteOff buf 80 :: IO Int64
          -- Read stx_btime.tv_nsec at offset 88
          tvNsec <- peekByteOff buf 88 :: IO Word32
          -- Check if birth time is valid (non-zero)
          if tvSec > 0
            then do
              let posixTime = fromIntegral tvSec + fromIntegral tvNsec / 1e9
              return $ Just $ posixSecondsToUTCTime posixTime
            else return Nothing
        else return Nothing

#elif defined(darwin_HOST_OS)

-- On macOS, stat() includes st_birthtimespec
-- The unix package's FileStatus doesn't expose it, so we use FFI
-- struct stat on macOS (arm64):
-- Offset 32: st_atimespec (16 bytes)
-- Offset 48: st_mtimespec (16 bytes)
-- Offset 64: st_ctimespec (16 bytes)
-- Offset 80: st_birthtimespec (16 bytes)
-- Total size: 144 bytes

foreign import ccall unsafe "stat"
  c_stat :: CString -> Ptr () -> IO CInt

-- | Get birth time using stat on macOS
getBirthTime :: FilePath -> IO (Maybe UTCTime)
getBirthTime path = do
  withCString path $ \cpath -> do
    allocaBytes 144 $ \buf -> do  -- stat structure size on macOS
      rc <- c_stat cpath buf
      if rc == 0
        then do
          -- Read st_birthtimespec at offset 80 on arm64
          -- tv_sec is first 8 bytes, tv_nsec is next 8 bytes
          tvSec <- peekByteOff buf 80 :: IO Int64
          tvNsec <- peekByteOff buf 88 :: IO Int64
          if tvSec > 0
            then do
              let posixTime = fromIntegral tvSec + fromIntegral tvNsec / 1e9
              return $ Just $ posixSecondsToUTCTime posixTime
            else return Nothing
        else return Nothing

#else

-- Fallback for other platforms
getBirthTime :: FilePath -> IO (Maybe UTCTime)
getBirthTime _ = return Nothing

#endif
