{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

data TestResult = Pass | Fail String
  deriving (Eq)

data TestStats = TestStats {passed :: Int, failed :: Int}

main :: IO ()
main = do
  refCat <- findReferenceCat
  haskbox <- findHaskbox

  putStrLn $ "Reference cat: " ++ refCat
  putStrLn $ "Testing: " ++ haskbox ++ " cat"
  putStrLn ""

  withSystemTempDirectory "haskbox-test" $ \tmpDir -> do
    setupTestFiles tmpDir
    stats <- runTests refCat haskbox tmpDir
    putStrLn ""
    putStrLn "=============================="
    putStrLn $ "PASS: " ++ show (passed stats)
    putStrLn $ "FAIL: " ++ show (failed stats)
    putStrLn "=============================="
    when (failed stats > 0) exitFailure

findReferenceCat :: IO FilePath
findReferenceCat = do
  isMac <- doesFileExist "/usr/bin/sw_vers"
  if isMac
    then do
      gcat <- findExecutable "gcat"
      case gcat of
        Just path -> return path
        Nothing -> do
          hPutStrLn stderr "Error: GNU cat (gcat) not found."
          hPutStrLn stderr "Please install coreutils: brew install coreutils"
          exitFailure
    else do
      cat <- findExecutable "cat"
      case cat of
        Just path -> return path
        Nothing -> do
          hPutStrLn stderr "Error: cat not found in PATH"
          exitFailure

findHaskbox :: IO FilePath
findHaskbox = do
  (_, out, _) <- readProcessWithExitCode "cabal" ["list-bin", "haskbox"] ""
  let path = filter (/= '\n') out
  exists <- doesFileExist path
  if exists
    then return path
    else do
      hPutStrLn stderr "Error: haskbox not built. Run 'cabal build' first."
      exitFailure

setupTestFiles :: FilePath -> IO ()
setupTestFiles dir = do
  -- Empty file
  BS.writeFile (dir </> "empty") BS.empty

  -- Simple text
  BS.writeFile (dir </> "text") "hello\nworld\n"

  -- No trailing newline
  BS.writeFile (dir </> "no_newline") "no newline"

  -- Multiple blank lines
  BS.writeFile (dir </> "blanks") "line1\n\n\n\nline2\n"

  -- Tabs
  BS.writeFile (dir </> "tabs") "col1\tcol2\tcol3\n"

  -- Control characters
  BS.writeFile (dir </> "control") "normal\x01\x02\x1f\x7fend\n"

  -- High-bit characters
  BS.writeFile (dir </> "highbit") "hi\x80\x9f\xffend\n"

  -- Many lines
  BS.writeFile (dir </> "many_lines") $ C8.unlines $ map (C8.pack . show) [1 .. 1000 :: Int]

  -- Binary data
  BS.writeFile (dir </> "binary") $ BS.pack [0 .. 255]

  -- File with spaces in name
  BS.writeFile (dir </> "file with spaces") "spaces\n"

  -- Unicode filename
  BS.writeFile (dir </> "ünïcödé") "unicode\n"

runTests :: FilePath -> FilePath -> FilePath -> IO TestStats
runTests ref haskbox dir = do
  let runTest name args = do
        result <- compareOutput ref haskbox dir args
        case result of
          Pass -> do
            putStrLn $ "PASS: " ++ name
            return (TestStats 1 0)
          Fail msg -> do
            putStrLn $ "FAIL: " ++ name
            putStrLn $ "      " ++ msg
            return (TestStats 0 1)

      file f = dir </> f

  results <-
    sequence
      -- Basic functionality
      [ runTest "empty file" [file "empty"],
        runTest "simple text" [file "text"],
        runTest "no trailing newline" [file "no_newline"],
        runTest "multiple blank lines" [file "blanks"],
        runTest "tabs" [file "tabs"],
        runTest "control chars" [file "control"],
        runTest "high-bit chars" [file "highbit"],
        runTest "many lines" [file "many_lines"],
        runTest "binary" [file "binary"],
        runTest "spaces in filename" [file "file with spaces"],
        runTest "unicode filename" [file "ünïcödé"],
        -- Multiple files
        runTest "two files" [file "text", file "no_newline"],
        runTest "three files" [file "text", file "empty", file "text"],
        -- Options
        runTest "-n" ["-n", file "text"],
        runTest "-n multi" ["-n", file "text", file "text"],
        runTest "-b" ["-b", file "blanks"],
        runTest "-s" ["-s", file "blanks"],
        runTest "-E" ["-E", file "text"],
        runTest "-T" ["-T", file "tabs"],
        runTest "-v" ["-v", file "control"],
        runTest "-v highbit" ["-v", file "highbit"],
        runTest "-A" ["-A", file "tabs"],
        -- Combined options
        runTest "-n -E" ["-n", "-E", file "text"],
        runTest "-n -s" ["-n", "-s", file "blanks"],
        runTest "-b -s" ["-b", "-s", file "blanks"],
        runTest "-A -n" ["-A", "-n", file "text"],
        -- Error cases
        runTest "nonexistent file" [dir </> "does_not_exist"],
        runTest "error + valid" [dir </> "does_not_exist", file "text"]
      ]

  return $ foldl (\a b -> TestStats (passed a + passed b) (failed a + failed b)) (TestStats 0 0) results

compareOutput :: FilePath -> FilePath -> FilePath -> [String] -> IO TestResult
compareOutput ref haskbox _ args = do
  (rcRef, outRef, errRef) <- runCmd ref args
  (rcTest, outTest, errTest) <- runCmd haskbox ("cat" : args)

  let errRefNorm = normalizeProgName errRef
      errTestNorm = normalizeProgName errTest

  if outRef /= outTest
    then return $ Fail $ "stdout differs (len " ++ show (BS.length outRef) ++ " vs " ++ show (BS.length outTest) ++ ")"
    else
      if errRefNorm /= errTestNorm
        then return $ Fail "stderr differs"
        else
          if rcRef /= rcTest
            then return $ Fail $ "exit code differs: " ++ show rcRef ++ " vs " ++ show rcTest
            else return Pass

runCmd :: FilePath -> [String] -> IO (ExitCode, BS.ByteString, BS.ByteString)
runCmd cmd args = do
  let cp =
        (proc cmd args)
          { std_out = CreatePipe,
            std_err = CreatePipe,
            std_in = NoStream
          }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  hSetBinaryMode hOut True
  hSetBinaryMode hErr True
  out <- BS.hGetContents hOut
  err <- BS.hGetContents hErr
  rc <- waitForProcess ph
  return (rc, out, err)

normalizeProgName :: BS.ByteString -> BS.ByteString
normalizeProgName s = C8.unlines $ map normLine $ C8.lines s
  where
    normLine line
      | "haskbox cat:" `BS.isInfixOf` line = replaceBS "haskbox cat:" "PROG:" line
      | "gcat:" `BS.isInfixOf` line = replaceBS "gcat:" "PROG:" line
      | "cat:" `BS.isInfixOf` line = replaceBS "cat:" "PROG:" line
      | otherwise = line

    replaceBS old new str =
      case BS.breakSubstring old str of
        (before, after)
          | BS.null after -> str
          | otherwise -> before <> new <> BS.drop (BS.length old) after
