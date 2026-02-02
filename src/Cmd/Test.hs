{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{-# HLINT ignore "Redundant evaluate" #-}

module Cmd.Test (run) where

import Control.Exception (IOException, catch)
import Data.Bits ((.&.))
import System.Exit (exitFailure, exitSuccess)
import System.Posix.Files
  ( FileStatus,
    deviceID,
    fileExist,
    fileID,
    fileMode,
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
    modificationTime,
  )
import System.Posix.IO (stdError, stdInput, stdOutput)
import System.Posix.Terminal (queryTerminal)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "test") >> exitSuccess
  [] -> exitFailure -- Empty test is false
  _ -> do
    result <- evaluate args
    if result then exitSuccess else exitFailure

evaluate :: [String] -> IO Bool
evaluate [] = return False
evaluate ["!"] = return False
evaluate ("!" : rest) = not <$> evaluate rest
evaluate ["-n", s] = return $ not (null s)
evaluate ["-z", s] = return $ null s
evaluate [s1, "=", s2] = return $ s1 == s2
evaluate [s1, "==", s2] = return $ s1 == s2
evaluate [s1, "!=", s2] = return $ s1 /= s2
evaluate [n1, "-eq", n2] = return $ readInt n1 == readInt n2
evaluate [n1, "-ne", n2] = return $ readInt n1 /= readInt n2
evaluate [n1, "-lt", n2] = return $ readInt n1 < readInt n2
evaluate [n1, "-le", n2] = return $ readInt n1 <= readInt n2
evaluate [n1, "-gt", n2] = return $ readInt n1 > readInt n2
evaluate [n1, "-ge", n2] = return $ readInt n1 >= readInt n2
evaluate ["-e", path] = fileExist path
evaluate ["-f", path] = testFile isRegularFile path
evaluate ["-d", path] = testFile isDirectory path
evaluate ["-b", path] = testFile isBlockDevice path
evaluate ["-c", path] = testFile isCharacterDevice path
evaluate ["-p", path] = testFile isNamedPipe path
evaluate ["-S", path] = testFile isSocket path
evaluate ["-L", path] = testLink path
evaluate ["-h", path] = testLink path
evaluate ["-s", path] = testSize path
evaluate ["-r", path] = testReadable path
evaluate ["-w", path] = testWritable path
evaluate ["-x", path] = testExecutable path
evaluate ["-t", fdStr] = testTerminal fdStr
evaluate [f1, "-nt", f2] = testNewer f1 f2
evaluate [f1, "-ot", f2] = testOlder f1 f2
evaluate [f1, "-ef", f2] = testSameFile f1 f2
evaluate [s] = return $ not (null s) -- Non-empty string is true
evaluate (e1 : "-a" : rest) = (&&) <$> evaluate [e1] <*> evaluate rest
evaluate (e1 : "-o" : rest) = (||) <$> evaluate [e1] <*> evaluate rest
evaluate ("(" : rest) = evaluateParens rest
evaluate _ = return False

evaluateParens :: [String] -> IO Bool
evaluateParens args =
  let (inner, _remaining) = break (== ")") args
   in evaluate inner

testFile :: (FileStatus -> Bool) -> FilePath -> IO Bool
testFile predicate path = catch doTest handler
  where
    doTest = do
      status <- getFileStatus path
      return $ predicate status
    handler :: IOException -> IO Bool
    handler _ = return False

testLink :: FilePath -> IO Bool
testLink path = catch doTest handler
  where
    doTest = do
      status <- getSymbolicLinkStatus path
      return $ isSymbolicLink status
    handler :: IOException -> IO Bool
    handler _ = return False

testSize :: FilePath -> IO Bool
testSize path = catch doTest handler
  where
    doTest = do
      status <- getFileStatus path
      return $ fileSize status > 0
    handler :: IOException -> IO Bool
    handler _ = return False

testReadable :: FilePath -> IO Bool
testReadable path = catch doTest handler
  where
    doTest = do
      status <- getFileStatus path
      let mode = fileMode status
      return $ (mode .&. 0o444) /= 0
    handler :: IOException -> IO Bool
    handler _ = return False

testWritable :: FilePath -> IO Bool
testWritable path = catch doTest handler
  where
    doTest = do
      status <- getFileStatus path
      let mode = fileMode status
      return $ (mode .&. 0o222) /= 0
    handler :: IOException -> IO Bool
    handler _ = return False

testExecutable :: FilePath -> IO Bool
testExecutable path = catch doTest handler
  where
    doTest = do
      status <- getFileStatus path
      let mode = fileMode status
      return $ (mode .&. 0o111) /= 0
    handler :: IOException -> IO Bool
    handler _ = return False

testNewer :: FilePath -> FilePath -> IO Bool
testNewer f1 f2 = catch doTest handler
  where
    doTest = do
      s1 <- getFileStatus f1
      s2 <- getFileStatus f2
      return $ modificationTime s1 > modificationTime s2
    handler :: IOException -> IO Bool
    handler _ = return False

testOlder :: FilePath -> FilePath -> IO Bool
testOlder f1 f2 = catch doTest handler
  where
    doTest = do
      s1 <- getFileStatus f1
      s2 <- getFileStatus f2
      return $ modificationTime s1 < modificationTime s2
    handler :: IOException -> IO Bool
    handler _ = return False

testSameFile :: FilePath -> FilePath -> IO Bool
testSameFile f1 f2 = catch doTest handler
  where
    doTest = do
      s1 <- getFileStatus f1
      s2 <- getFileStatus f2
      return $
        deviceID s1 == deviceID s2
          && fileID s1 == fileID s2
    handler :: IOException -> IO Bool
    handler _ = return False

readInt :: String -> Integer
readInt s = case reads s of
  [(n, _)] -> n
  _ -> 0

-- | Test if a file descriptor is associated with a terminal
testTerminal :: String -> IO Bool
testTerminal fdStr = catch doTest handler
  where
    doTest = case reads fdStr :: [(Int, String)] of
      [(fd, "")] -> case fd of
        0 -> queryTerminal stdInput
        1 -> queryTerminal stdOutput
        2 -> queryTerminal stdError
        _ -> return False -- Other FDs not supported
      _ -> return False

    handler :: IOException -> IO Bool
    handler _ = return False

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox test EXPRESSION",
        "       haskbox [ EXPRESSION ]",
        "Evaluate EXPRESSION and return exit status 0 if true, 1 if false.",
        "",
        "String tests:",
        "  -n STRING            STRING has nonzero length",
        "  -z STRING            STRING has zero length",
        "  STRING1 = STRING2    strings are equal",
        "  STRING1 != STRING2   strings are not equal",
        "",
        "Integer comparisons:",
        "  N1 -eq N2            N1 equals N2",
        "  N1 -ne N2            N1 not equal to N2",
        "  N1 -lt N2            N1 less than N2",
        "  N1 -le N2            N1 less than or equal to N2",
        "  N1 -gt N2            N1 greater than N2",
        "  N1 -ge N2            N1 greater than or equal to N2",
        "",
        "File tests:",
        "  -e FILE              FILE exists",
        "  -f FILE              FILE is a regular file",
        "  -d FILE              FILE is a directory",
        "  -b FILE              FILE is a block device",
        "  -c FILE              FILE is a character device",
        "  -p FILE              FILE is a named pipe",
        "  -S FILE              FILE is a socket",
        "  -L FILE              FILE is a symbolic link",
        "  -s FILE              FILE has size greater than zero",
        "  -r FILE              FILE is readable",
        "  -w FILE              FILE is writable",
        "  -x FILE              FILE is executable",
        "",
        "File comparisons:",
        "  FILE1 -nt FILE2      FILE1 is newer than FILE2",
        "  FILE1 -ot FILE2      FILE1 is older than FILE2",
        "  FILE1 -ef FILE2      FILE1 and FILE2 are the same file",
        "",
        "Logical operators:",
        "  ! EXPRESSION         EXPRESSION is false",
        "  EXPR1 -a EXPR2       both expressions are true",
        "  EXPR1 -o EXPR2       either expression is true",
        "",
        "      --help           display this help and exit",
        "      --version        output version information and exit"
      ]
