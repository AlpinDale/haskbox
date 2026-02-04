{-# LANGUAGE OverloadedStrings #-}

module Test.Utils
  ( -- * Test environment
    TestEnv (..),
    withTestEnv,

    -- * Running commands
    runHaskbox,
    runGnu,
    runCmd,
    runCmdStdin,
    runCmdWithEnv,
    runCmdStdinWithEnv,
    runCmdTtyWithEnv,
    findScript,

    -- * Comparison helpers
    compareOutput,
    compareOutputStdin,
    normalizeErr,

    -- * File setup
    setupTestFiles,

    -- * Re-exports for convenience
    assertBool,
    assertEqual,
    (@?=),
    (@=?),
  )
where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )
import Test.Tasty.HUnit

data TestEnv = TestEnv
  { envHaskbox :: FilePath,
    envGnuPrefix :: String,
    envTmpDir :: FilePath
  }

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action = do
  haskbox <- findHaskbox
  gnuPrefix <- findGnuPrefix
  bracket setupTmpDir cleanupTmpDir $ \tmpDir -> do
    let env = TestEnv haskbox gnuPrefix tmpDir
    setupTestFiles tmpDir
    action env
  where
    setupTmpDir = do
      sysTmp <- getCanonicalTemporaryDirectory
      createTempDirectory sysTmp "haskbox-test"
    cleanupTmpDir _ = return () -- Let the OS clean up temp files

findHaskbox :: IO FilePath
findHaskbox = do
  (_, out, _) <- runCmd "cabal" ["list-bin", "haskbox"]
  let path = filter (/= '\n') (C8.unpack out)
  exists <- doesFileExist path
  if exists
    then return path
    else error "haskbox not built. Run 'cabal build' first."

findGnuPrefix :: IO String
findGnuPrefix = do
  isMac <- doesFileExist "/usr/bin/sw_vers"
  if isMac
    then do
      gcat <- findExecutable "gcat"
      return $ maybe "" (const "g") gcat
    else return ""

runHaskbox :: TestEnv -> String -> [String] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runHaskbox env cmd args = runCmd (envHaskbox env) (cmd : args)

runGnu :: TestEnv -> String -> [String] -> IO (Maybe (ExitCode, BS.ByteString, BS.ByteString))
runGnu env cmd args = do
  let gnuCmd = envGnuPrefix env ++ cmd
  exists <- findExecutable gnuCmd
  case exists of
    Nothing -> return Nothing
    Just path -> Just <$> runCmd path args

runCmd :: FilePath -> [String] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmd cmd args = do
  env <- getEnvironment
  runCmdWithEnv' cmd args env

runCmdWithEnv :: FilePath -> [String] -> [(String, String)] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmdWithEnv cmd args overrides = do
  env <- mergeEnv overrides
  runCmdWithEnv' cmd args env

runCmdWithEnv' :: FilePath -> [String] -> [(String, String)] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmdWithEnv' cmd args env = do
  (_, Just hOut, Just hErr, ph) <-
    createProcess
      (proc cmd args)
        { std_out = CreatePipe,
          std_err = CreatePipe,
          env = Just env
        }
  out <- BS.hGetContents hOut
  err <- BS.hGetContents hErr
  rc <- waitForProcess ph
  hClose hOut
  hClose hErr
  return (rc, out, err)

runCmdStdin :: FilePath -> [String] -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmdStdin cmd args input = do
  env <- getEnvironment
  runCmdStdinWithEnv' cmd args input env

runCmdStdinWithEnv :: FilePath -> [String] -> BS.ByteString -> [(String, String)] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmdStdinWithEnv cmd args input overrides = do
  env <- mergeEnv overrides
  runCmdStdinWithEnv' cmd args input env

runCmdStdinWithEnv' :: FilePath -> [String] -> BS.ByteString -> [(String, String)] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmdStdinWithEnv' cmd args input env = do
  (Just hIn, Just hOut, Just hErr, ph) <-
    createProcess
      (proc cmd args)
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          env = Just env
        }
  BS.hPut hIn input
  hClose hIn
  out <- BS.hGetContents hOut
  err <- BS.hGetContents hErr
  rc <- waitForProcess ph
  hClose hOut
  hClose hErr
  return (rc, out, err)

findScript :: IO (Maybe FilePath)
findScript = findExecutable "script"

runCmdTtyWithEnv :: FilePath -> FilePath -> [String] -> [(String, String)] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmdTtyWithEnv scriptPath cmd args overrides = do
  env <- mergeEnv overrides
  isMac <- doesFileExist "/usr/bin/sw_vers"
  let (scriptCmd, scriptArgs) =
        if isMac
          then (scriptPath, ["-q", "/dev/null", cmd] ++ args)
          else (scriptPath, ["-q", "-c", shellJoin (cmd : args), "/dev/null"])
  (_, Just hOut, Just hErr, ph) <-
    createProcess
      (proc scriptCmd scriptArgs)
        { std_out = CreatePipe,
          std_err = CreatePipe,
          env = Just env
        }
  out <- BS.hGetContents hOut
  err <- BS.hGetContents hErr
  rc <- waitForProcess ph
  hClose hOut
  hClose hErr
  let combined = out <> err
  return (rc, combined, BS.empty)

shellJoin :: [String] -> String
shellJoin = unwords . map shellQuote

shellQuote :: String -> String
shellQuote s = "'" ++ concatMap escapeChar s ++ "'"
  where
    escapeChar '\'' = "'\\''"
    escapeChar c = [c]

mergeEnv :: [(String, String)] -> IO [(String, String)]
mergeEnv overrides = do
  base <- getEnvironment
  let baseMap = Map.fromList base
      merged = foldl' (\m (k, v) -> Map.insert k v m) baseMap overrides
  return (Map.toList merged)

compareOutput :: TestEnv -> String -> [String] -> IO ()
compareOutput env cmd args = do
  gnuResult <- runGnu env cmd args
  case gnuResult of
    Nothing -> assertFailure $ "GNU " ++ cmd ++ " not found"
    Just (rcRef, outRef, errRef) -> do
      (rcTest, outTest, errTest) <- runHaskbox env cmd args
      let errRefNorm = normalizeErr cmd (envGnuPrefix env) errRef
          errTestNorm = normalizeErr cmd (envGnuPrefix env) errTest
      outRef @=? outTest
      errRefNorm @=? errTestNorm
      rcRef @=? rcTest

compareOutputStdin :: TestEnv -> String -> [String] -> BS.ByteString -> IO ()
compareOutputStdin env cmd args input = do
  let gnuCmd = envGnuPrefix env ++ cmd
  gnuExists <- findExecutable gnuCmd
  case gnuExists of
    Nothing -> assertFailure $ "GNU " ++ cmd ++ " not found"
    Just gnuPath -> do
      (rcRef, outRef, errRef) <- runCmdStdin gnuPath args input
      (rcTest, outTest, errTest) <- runCmdStdin (envHaskbox env) (cmd : args) input
      let errRefNorm = normalizeErr cmd (envGnuPrefix env) errRef
          errTestNorm = normalizeErr cmd (envGnuPrefix env) errTest
      outRef @=? outTest
      errRefNorm @=? errTestNorm
      rcRef @=? rcTest

normalizeErr :: String -> String -> BS.ByteString -> BS.ByteString
normalizeErr cmd gnuPfx s = C8.unlines $ map normLine $ C8.lines s
  where
    gnuCmd = C8.pack $ gnuPfx ++ cmd
    hbCmd = C8.pack $ "haskbox " ++ cmd
    progBS = C8.pack "PROG"
    progColonBS = C8.pack "PROG:"
    normLine line
      | hbCmd `BS.isInfixOf` line = replaceBS hbCmd progBS line
      | gnuCmd `BS.isInfixOf` line = stripPathBeforeProg gnuCmd line
      | C8.pack cmd `BS.isInfixOf` line = replaceBS (C8.pack $ cmd ++ ":") progColonBS line
      | otherwise = line
    replaceBS old new str =
      case BS.breakSubstring old str of
        (before, after)
          | BS.null after -> str
          | otherwise -> before <> new <> BS.drop (BS.length old) after
    stripPathBeforeProg cmdName str =
      case BS.breakSubstring cmdName str of
        (_, after)
          | BS.null after -> str
          | otherwise -> progBS <> BS.drop (BS.length cmdName) after

setupTestFiles :: FilePath -> IO ()
setupTestFiles dir = do
  BS.writeFile (dir </> "empty") BS.empty
  BS.writeFile (dir </> "text") "hello\nworld\n"
  BS.writeFile (dir </> "no_newline") "no newline"
  BS.writeFile (dir </> "blanks") "line1\n\n\n\nline2\n"
  BS.writeFile (dir </> "tabs") "col1\tcol2\tcol3\n"
  BS.writeFile (dir </> "control") "normal\x01\x02\x1f\x7fend\n"
  BS.writeFile (dir </> "highbit") "hi\x80\x9f\xffend\n"
  BS.writeFile (dir </> "many_lines") $ C8.unlines $ map (C8.pack . show) [1 .. 1000 :: Int]
  BS.writeFile (dir </> "binary") $ BS.pack [0 .. 255]
  BS.writeFile (dir </> "file with spaces") "spaces\n"

  BS.writeFile (dir </> "unsorted") "banana\napple\ncherry\napple\nbanana\n"
  BS.writeFile (dir </> "sorted") "apple\napple\nbanana\nbanana\ncherry\n"
  BS.writeFile (dir </> "numbers") "10\n2\n1\n20\n3\n"
  BS.writeFile (dir </> "reverse") "cherry\nbanana\napple\n"
  BS.writeFile (dir </> "duplicates") "a\na\nb\nb\nb\nc\n"
  BS.writeFile (dir </> "mixed_case") "Apple\napple\nBanana\nbanana\n"

  BS.writeFile (dir </> "fields") "one:two:three\nfour:five:six\n"
  BS.writeFile (dir </> "csv") "a,b,c\nd,e,f\n"
  BS.writeFile (dir </> "col1") "a\nb\nc\n"
  BS.writeFile (dir </> "col2") "1\n2\n3\n"

  BS.writeFile (dir </> "comm1") "a\nb\nc\nd\n"
  BS.writeFile (dir </> "comm2") "b\nc\ne\nf\n"

  BS.writeFile (dir </> "for_nl") "first\n\nsecond\nthird\n"

  BS.writeFile (dir </> "lowercase") "hello world\n"
  BS.writeFile (dir </> "uppercase") "HELLO WORLD\n"

  BS.writeFile (dir </> "wc_test") "one two three\nfour five\n"

  BS.writeFile (dir </> "tenlines") $ C8.unlines $ map (C8.pack . show) [1 .. 10 :: Int]

  BS.writeFile (dir </> "tabbed") "a\tb\tc\n"
  BS.writeFile (dir </> "spaced") "a       b       c\n"

  BS.writeFile (dir </> "longline") "abcdefghijklmnopqrstuvwxyz\n"

  BS.writeFile (dir </> "short_binary") $ BS.pack [0, 1, 2, 3, 255]

  BS.writeFile (dir </> "lines3") "first\nsecond\nthird\n"

  BS.writeFile (dir </> "od_test") "ABC\n"

  BS.writeFile (dir </> "cksum_file") "test content\n"

  BS.writeFile (dir </> "for_split") "line1\nline2\nline3\nline4\nline5\nline6\n"

  BS.writeFile (dir </> "tsort_input") "a b\nb c\nc d\n"

  BS.writeFile (dir </> "crlf_file") "line1\r\nline2\r\n"
