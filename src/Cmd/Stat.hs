{-# LANGUAGE OverloadedStrings #-}

module Cmd.Stat (run) where

import Control.Exception (IOException, catch)
import Data.Bits ((.&.))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric (showHex, showOct)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files
  ( FileStatus,
    accessTime,
    deviceID,
    fileGroup,
    fileID,
    fileMode,
    fileOwner,
    fileSize,
    getFileStatus,
    getSymbolicLinkStatus,
    isBlockDevice,
    isCharacterDevice,
    isDirectory,
    isNamedPipe,
    isRegularFile,
    isSocket,
    isSymbolicLink,
    linkCount,
    modificationTime,
    readSymbolicLink,
    specialDeviceID,
    statusChangeTime,
  )
import System.Posix.Types (EpochTime, FileMode)
import System.Posix.User (getGroupEntryForID, getUserEntryForID, groupName, userName)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox stat: " ++ err
    exitFailure
  Right (opts, files)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "stat") >> exitSuccess
    | null files -> do
        hPutStrLn stderr "haskbox stat: missing operand"
        hPutStrLn stderr "Try 'haskbox stat --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (statFile opts) files
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optDereference :: !Bool,
    optFileSystem :: !Bool,
    optTerse :: !Bool,
    optFormat :: !(Maybe String),
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optDereference = False,
      optFileSystem = False,
      optTerse = False,
      optFormat = Nothing,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--dereference" : rest) = parseArgs opts {optDereference = True} rest
parseArgs opts ("--file-system" : rest) = parseArgs opts {optFileSystem = True} rest
parseArgs opts ("--terse" : rest) = parseArgs opts {optTerse = True} rest
parseArgs opts ("--format" : fmt : rest) = parseArgs opts {optFormat = Just fmt} rest
parseArgs opts ("-L" : rest) = parseArgs opts {optDereference = True} rest
parseArgs opts ("-f" : rest) = parseArgs opts {optFileSystem = True} rest
parseArgs opts ("-t" : rest) = parseArgs opts {optTerse = True} rest
parseArgs opts (('-' : 'c' : fmt) : rest)
  | null fmt = case rest of
      (f : rest') -> parseArgs opts {optFormat = Just f} rest'
      [] -> Left "option requires an argument -- 'c'"
  | otherwise = parseArgs opts {optFormat = Just fmt} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (c : cs) r = case c of
      'L' -> parseShortFlags o {optDereference = True} cs r
      'f' -> parseShortFlags o {optFileSystem = True} cs r
      't' -> parseShortFlags o {optTerse = True} cs r
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

statFile :: Opts -> FilePath -> IO Bool
statFile opts path = catch doStat handler
  where
    doStat = do
      status <-
        if optDereference opts
          then getFileStatus path
          else getSymbolicLinkStatus path

      case optFormat opts of
        Just fmt -> printFormat fmt path status
        Nothing
          | optTerse opts -> printTerse path status
          | otherwise -> printFull path status
      return False

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox stat: cannot stat '" ++ path ++ "': " ++ friendlyError (show e)
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

printFormat :: String -> FilePath -> FileStatus -> IO ()
printFormat fmt path status = do
  result <- processFormat fmt
  putStrLn result
  where
    processFormat [] = return []
    processFormat ('%' : '%' : rest) = ('%' :) <$> processFormat rest
    processFormat ('%' : c : rest) = do
      spec <- formatSpec c
      restResult <- processFormat rest
      return $ spec ++ restResult
    processFormat ('\\' : 'n' : rest) = ('\n' :) <$> processFormat rest
    processFormat ('\\' : 't' : rest) = ('\t' :) <$> processFormat rest
    processFormat ('\\' : '\\' : rest) = ('\\' :) <$> processFormat rest
    processFormat (c : rest) = (c :) <$> processFormat rest

    formatSpec :: Char -> IO String
    formatSpec 'a' = return $ showOct (fileMode status .&. 0o7777) "" -- Access rights in octal
    formatSpec 'A' = return $ modeString (fileMode status) -- Access rights in human readable form
    formatSpec 'b' = return $ show (fileSize status `div` 512) -- Blocks allocated
    formatSpec 'B' = return "512" -- Block size for %b
    formatSpec 'd' = return $ show (deviceID status) -- Device number in decimal
    formatSpec 'D' = return $ showHex (deviceID status) "" -- Device number in hex
    formatSpec 'f' = return $ showHex (fileMode status) "" -- Raw mode in hex
    formatSpec 'F' = return $ fileTypeStr status -- File type
    formatSpec 'g' = return $ show (fileGroup status) -- Group ID
    formatSpec 'G' =
      -- Group name
      catch
        (groupName <$> getGroupEntryForID (fileGroup status))
        (const (return $ show (fileGroup status)) :: IOException -> IO String)
    formatSpec 'h' = return $ show (linkCount status) -- Number of hard links
    formatSpec 'i' = return $ show (fileID status) -- Inode number
    formatSpec 'n' = return path -- File name
    formatSpec 'N' = do
      -- Quoted file name with dereference if symlink
      if isSymbolicLink status
        then do
          target <- catch (readSymbolicLink path) (const (return "") :: IOException -> IO String)
          return $ "'" ++ path ++ "' -> '" ++ target ++ "'"
        else return $ "'" ++ path ++ "'"
    formatSpec 'o' = return "4096" -- Optimal I/O transfer size (typical)
    formatSpec 'r' = return $ show (specialDeviceID status) -- Device type in decimal
    formatSpec 'R' = return $ showHex (specialDeviceID status) "" -- Device type in hex
    formatSpec 's' = return $ show (fileSize status) -- Total size in bytes
    formatSpec 't' = return $ showHex (specialDeviceID status `div` 256) "" -- Major device type in hex
    formatSpec 'T' = return $ showHex (specialDeviceID status `mod` 256) "" -- Minor device type in hex
    formatSpec 'u' = return $ show (fileOwner status) -- User ID
    formatSpec 'U' =
      -- User name
      catch
        (userName <$> getUserEntryForID (fileOwner status))
        (const (return $ show (fileOwner status)) :: IOException -> IO String)
    formatSpec 'w' = return "-" -- Birth time (not available on most systems)
    formatSpec 'W' = return "0" -- Birth time in seconds
    formatSpec 'x' = return $ formatTimestamp (accessTime status) -- Access time, human-readable
    formatSpec 'X' = return $ show (accessTime status) -- Access time, seconds since epoch
    formatSpec 'y' = return $ formatTimestamp (modificationTime status) -- Modify time, human-readable
    formatSpec 'Y' = return $ show (modificationTime status) -- Modify time, seconds since epoch
    formatSpec 'z' = return $ formatTimestamp (statusChangeTime status) -- Change time, human-readable
    formatSpec 'Z' = return $ show (statusChangeTime status) -- Change time, seconds since epoch
    formatSpec c = return ['%', c] -- Unknown format spec, keep as-is

printTerse :: FilePath -> FileStatus -> IO ()
printTerse path status = do
  let size = fileSize status
      mode = fileMode status
      uid = fileOwner status
      gid = fileGroup status
      dev = deviceID status
      ino = fileID status
      nlink = linkCount status
      atime = accessTime status
      mtime = modificationTime status
      ctime = statusChangeTime status
  putStrLn $
    path
      ++ " "
      ++ show size
      ++ " 0 "
      ++ showOct mode ""
      ++ " "
      ++ show uid
      ++ " "
      ++ show gid
      ++ " "
      ++ show dev
      ++ " "
      ++ show ino
      ++ " "
      ++ show nlink
      ++ " "
      ++ show atime
      ++ " "
      ++ show mtime
      ++ " "
      ++ show ctime
      ++ " 0 0"

printFull :: FilePath -> FileStatus -> IO ()
printFull path status = do
  let size = fileSize status
      mode = fileMode status
      uid = fileOwner status
      gid = fileGroup status
      dev = deviceID status
      ino = fileID status
      nlink = linkCount status
      atime = accessTime status
      mtime = modificationTime status
      ctime = statusChangeTime status

  uname <- catch (userName <$> getUserEntryForID uid) (const (return $ show uid) :: IOException -> IO String)
  gname <- catch (groupName <$> getGroupEntryForID gid) (const (return $ show gid) :: IOException -> IO String)

  putStrLn $ "  File: " ++ path
  putStrLn $ "  Size: " ++ show size ++ "\t\tBlocks: " ++ show (size `div` 512) ++ "\t\t" ++ fileTypeStr status
  putStrLn $ "Device: " ++ show dev ++ "\tInode: " ++ show ino ++ "\tLinks: " ++ show nlink
  putStrLn $ "Access: (" ++ showOct (mode .&. 0o7777) "" ++ "/" ++ modeString mode ++ ")  Uid: (" ++ show uid ++ "/" ++ uname ++ ")  Gid: (" ++ show gid ++ "/" ++ gname ++ ")"
  putStrLn $ "Access: " ++ formatTimestamp atime
  putStrLn $ "Modify: " ++ formatTimestamp mtime
  putStrLn $ "Change: " ++ formatTimestamp ctime

fileTypeStr :: FileStatus -> String
fileTypeStr status
  | isRegularFile status = "regular file"
  | isDirectory status = "directory"
  | isSymbolicLink status = "symbolic link"
  | isBlockDevice status = "block device"
  | isCharacterDevice status = "character device"
  | isNamedPipe status = "fifo"
  | isSocket status = "socket"
  | otherwise = "unknown"

modeString :: FileMode -> String
modeString mode =
  let owner = permBits ((mode `div` 64) .&. 7)
      group = permBits ((mode `div` 8) .&. 7)
      other = permBits (mode .&. 7)
      ftype
        | mode .&. 0o40000 /= 0 = 'd'
        | mode .&. 0o120000 == 0o120000 = 'l'
        | otherwise = '-'
   in ftype : owner ++ group ++ other
  where
    permBits n =
      [ if n .&. 4 /= 0 then 'r' else '-',
        if n .&. 2 /= 0 then 'w' else '-',
        if n .&. 1 /= 0 then 'x' else '-'
      ]

formatTimestamp :: EpochTime -> String
formatTimestamp t =
  let utc = posixSecondsToUTCTime (realToFrac t)
   in formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.000000000 %z" utc

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox stat [OPTION]... FILE...",
        "Display file or file system status.",
        "",
        "  -L, --dereference     follow links",
        "  -f, --file-system     display file system status instead of file status",
        "  -c, --format=FORMAT   use the specified FORMAT instead of the default",
        "  -t, --terse           print the information in terse form",
        "      --help            display this help and exit",
        "      --version         output version information and exit",
        "",
        "The valid format sequences for files (without --file-system):",
        "  %a   permission bits in octal",
        "  %A   permission bits and file type in human readable form",
        "  %b   number of blocks allocated",
        "  %B   the size in bytes of each block reported by %b",
        "  %d   device number in decimal",
        "  %D   device number in hex",
        "  %f   raw mode in hex",
        "  %F   file type",
        "  %g   group ID of owner",
        "  %G   group name of owner",
        "  %h   number of hard links",
        "  %i   inode number",
        "  %n   file name",
        "  %N   quoted file name with dereference if symbolic link",
        "  %o   optimal I/O transfer size hint",
        "  %r   device type in decimal (st_rdev)",
        "  %R   device type in hex (st_rdev)",
        "  %s   total size, in bytes",
        "  %t   major device type in hex",
        "  %T   minor device type in hex",
        "  %u   user ID of owner",
        "  %U   user name of owner",
        "  %w   time of file birth, human-readable; - if unknown",
        "  %W   time of file birth, seconds since Epoch; 0 if unknown",
        "  %x   time of last access, human-readable",
        "  %X   time of last access, seconds since Epoch",
        "  %y   time of last data modification, human-readable",
        "  %Y   time of last data modification, seconds since Epoch",
        "  %z   time of last status change, human-readable",
        "  %Z   time of last status change, seconds since Epoch"
      ]
