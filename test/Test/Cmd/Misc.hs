{-# LANGUAGE OverloadedStrings #-}

-- | Tests for miscellaneous commands: echo, yes, true, false, sleep, seq, factor, printf, etc.
module Test.Cmd.Misc (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Directory (createDirectory, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "Miscellaneous"
    [ echoTests env,
      yesTests env,
      trueTests env,
      falseTests env,
      sleepTests env,
      seqTests env,
      factorTests env,
      printfTests env,
      testTests env,
      odTests env,
      cksumTests env,
      splitTests env,
      tsortTests env,
      timeoutTests env,
      mkfifoTests env,
      dos2unixTests env,
      teeTests env,
      mktempTests env,
      syncTests env
    ]

echoTests :: TestEnv -> TestTree
echoTests env =
  testGroup
    "echo"
    [ testCase "simple" $ compareOutput env "echo" ["hello", "world"],
      testCase "-n (no newline)" $ compareOutput env "echo" ["-n", "hello"],
      testCase "-e (escapes)" $ compareOutput env "echo" ["-e", "hello\\nworld"]
    ]

yesTests :: TestEnv -> TestTree
yesTests env =
  testGroup
    "yes"
    [ testCase "produces output (limited)" $ do
        -- yes runs forever, so we need to kill it after getting some output
        result <- race (threadDelay 100000) $ do
          (_, out, _) <- runHaskbox env "yes" []
          return out
        case result of
          Left () -> return () -- Timeout is expected
          Right out -> assertBool "should produce y's" (BS.length out > 0)
    ]

trueTests :: TestEnv -> TestTree
trueTests env =
  testGroup
    "true"
    [ testCase "exits 0" $ do
        (rc, _, _) <- runHaskbox env "true" []
        rc @?= ExitSuccess
    ]

falseTests :: TestEnv -> TestTree
falseTests env =
  testGroup
    "false"
    [ testCase "exits 1" $ do
        (rc, _, _) <- runHaskbox env "false" []
        rc @?= ExitFailure 1
    ]

sleepTests :: TestEnv -> TestTree
sleepTests env =
  testGroup
    "sleep"
    [ testCase "sleeps briefly" $ do
        (rc, _, _) <- runHaskbox env "sleep" ["0.1"]
        rc @?= ExitSuccess
    ]

seqTests :: TestEnv -> TestTree
seqTests env =
  testGroup
    "seq"
    [ testCase "1 to 5" $ compareOutput env "seq" ["5"],
      testCase "2 to 10" $ compareOutput env "seq" ["2", "10"],
      testCase "with step" $ compareOutput env "seq" ["1", "2", "10"],
      testCase "-w (equal width)" $ compareOutput env "seq" ["-w", "8", "12"]
    ]

factorTests :: TestEnv -> TestTree
factorTests env =
  testGroup
    "factor"
    [ testCase "prime" $ compareOutput env "factor" ["17"],
      testCase "composite" $ compareOutput env "factor" ["100"],
      testCase "multiple numbers" $ compareOutput env "factor" ["12", "15", "20"]
    ]

printfTests :: TestEnv -> TestTree
printfTests env =
  testGroup
    "printf"
    [ testCase "simple string" $ compareOutput env "printf" ["%s\\n", "hello"],
      testCase "integer" $ compareOutput env "printf" ["%d\\n", "42"],
      testCase "multiple args" $ compareOutput env "printf" ["%s is %d\\n", "answer", "42"]
    ]

testTests :: TestEnv -> TestTree
testTests env =
  testGroup
    "test"
    [ testCase "-f (file exists)" $ do
        (rc, _, _) <- runHaskbox env "test" ["-f", envTmpDir env </> "text"]
        rc @?= ExitSuccess,
      testCase "-d (directory)" $ do
        (rc, _, _) <- runHaskbox env "test" ["-d", envTmpDir env]
        rc @?= ExitSuccess,
      testCase "-z (empty string)" $ do
        (rc, _, _) <- runHaskbox env "test" ["-z", ""]
        rc @?= ExitSuccess,
      testCase "-n (non-empty string)" $ do
        (rc, _, _) <- runHaskbox env "test" ["-n", "hello"]
        rc @?= ExitSuccess,
      testCase "string equality" $ do
        (rc, _, _) <- runHaskbox env "test" ["hello", "=", "hello"]
        rc @?= ExitSuccess,
      testCase "numeric comparison" $ do
        (rc, _, _) <- runHaskbox env "test" ["5", "-lt", "10"]
        rc @?= ExitSuccess
    ]

odTests :: TestEnv -> TestTree
odTests env =
  testGroup
    "od"
    [ testCase "-x (hex)" $ compareOutput env "od" ["-x", envTmpDir env </> "od_test"],
      testCase "-c (char)" $ compareOutput env "od" ["-c", envTmpDir env </> "od_test"],
      testCase "-b (octal bytes)" $ compareOutput env "od" ["-b", envTmpDir env </> "od_test"]
    ]

cksumTests :: TestEnv -> TestTree
cksumTests env =
  testGroup
    "cksum"
    [ testCase "single file" $ compareOutput env "cksum" [envTmpDir env </> "cksum_file"],
      testCase "multiple files" $
        compareOutput env "cksum" [envTmpDir env </> "text", envTmpDir env </> "cksum_file"]
    ]

splitTests :: TestEnv -> TestTree
splitTests env =
  testGroup
    "split"
    [ testCase "-l 3 (lines)" $ do
        let outDir = envTmpDir env </> "split_out"
        createDirectory outDir
        (rc, _, _) <-
          runHaskbox
            env
            "split"
            ["-l", "3", envTmpDir env </> "for_split", outDir </> "part_"]
        rc @?= ExitSuccess
        -- Check that at least one output file was created
        exists <- doesFileExist (outDir </> "part_aa")
        assertBool "should create output files" exists
    ]

tsortTests :: TestEnv -> TestTree
tsortTests env =
  testGroup
    "tsort"
    [ testCase "basic sort" $ compareOutput env "tsort" [envTmpDir env </> "tsort_input"]
    ]

timeoutTests :: TestEnv -> TestTree
timeoutTests env =
  testGroup
    "timeout"
    [ testCase "quick command succeeds" $ do
        (rc, _, _) <- runHaskbox env "timeout" ["5", "true"]
        rc @?= ExitSuccess,
      testCase "timeout returns 124" $ do
        (rc, _, _) <- runHaskbox env "timeout" ["0.1", "sleep", "10"]
        rc @?= ExitFailure 124
    ]

mkfifoTests :: TestEnv -> TestTree
mkfifoTests env =
  testGroup
    "mkfifo"
    [ testCase "creates fifo" $ do
        let fifo = envTmpDir env </> "test_fifo"
        (rc, _, _) <- runHaskbox env "mkfifo" [fifo]
        rc @?= ExitSuccess
        exists <- doesFileExist fifo
        assertBool "fifo should exist" exists
    ]

dos2unixTests :: TestEnv -> TestTree
dos2unixTests env =
  testGroup
    "dos2unix"
    [ testCase "converts CRLF to LF" $ do
        let file = envTmpDir env </> "crlf_copy"
        BS.writeFile file "line1\r\nline2\r\n"
        (rc, _, _) <- runHaskbox env "dos2unix" [file]
        rc @?= ExitSuccess
        content <- BS.readFile file
        content @?= "line1\nline2\n"
    ]

teeTests :: TestEnv -> TestTree
teeTests env =
  testGroup
    "tee"
    [ testCase "writes to file and stdout" $ do
        let outFile = envTmpDir env </> "tee_out"
        (rc, out, _) <- runCmdStdin (envHaskbox env) ["tee", outFile] "hello\n"
        rc @?= ExitSuccess
        out @?= "hello\n"
        content <- BS.readFile outFile
        content @?= "hello\n"
    ]

mktempTests :: TestEnv -> TestTree
mktempTests env =
  testGroup
    "mktemp"
    [ testCase "creates temp file" $ do
        (rc, out, _) <- runHaskbox env "mktemp" []
        rc @?= ExitSuccess
        let path = C8.unpack $ C8.init out -- Remove trailing newline
        exists <- doesFileExist path
        assertBool "temp file should exist" exists
    ]

syncTests :: TestEnv -> TestTree
syncTests env =
  testGroup
    "sync"
    [ testCase "completes without error" $ do
        (rc, _, _) <- runHaskbox env "sync" []
        rc @?= ExitSuccess
    ]
