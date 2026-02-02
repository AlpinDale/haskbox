{-# LANGUAGE OverloadedStrings #-}

-- | Tests for system info commands: uname, id, whoami, hostname, nproc, date, env, stat, etc.
module Test.Cmd.System (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "System Information"
    [ unameTests env,
      idTests env,
      whoamiTests env,
      lognameTests env,
      nprocTests env,
      dateTests env,
      envTests env,
      printenvTests env,
      ttyTests env,
      hostidTests env,
      statTests env
    ]

unameTests :: TestEnv -> TestTree
unameTests env =
  testGroup
    "uname"
    [ testCase "default" $ compareOutput env "uname" [],
      testCase "-a (all)" $ compareOutput env "uname" ["-a"],
      testCase "-s (kernel name)" $ compareOutput env "uname" ["-s"],
      testCase "-n (nodename)" $ compareOutput env "uname" ["-n"],
      testCase "-r (release)" $ compareOutput env "uname" ["-r"],
      testCase "-m (machine)" $ compareOutput env "uname" ["-m"]
    ]

idTests :: TestEnv -> TestTree
idTests env =
  testGroup
    "id"
    [ testCase "default" $ do
        (rc, out, _) <- runHaskbox env "id" []
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "-u (user id)" $ compareOutput env "id" ["-u"],
      testCase "-g (group id)" $ compareOutput env "id" ["-g"],
      testCase "-un (username)" $ compareOutput env "id" ["-un"],
      testCase "-gn (group name)" $ compareOutput env "id" ["-gn"]
    ]

whoamiTests :: TestEnv -> TestTree
whoamiTests env =
  testGroup
    "whoami"
    [ testCase "prints username" $ compareOutput env "whoami" []
    ]

lognameTests :: TestEnv -> TestTree
lognameTests env =
  testGroup
    "logname"
    [ testCase "prints login name" $ do
        (rc, _out, _) <- runHaskbox env "logname" []
        -- logname may fail if not run from a login shell
        assertBool "should complete" (rc == ExitSuccess || rc == ExitFailure 1)
    ]

nprocTests :: TestEnv -> TestTree
nprocTests env =
  testGroup
    "nproc"
    [ testCase "prints processor count" $ compareOutput env "nproc" []
    ]

dateTests :: TestEnv -> TestTree
dateTests env =
  testGroup
    "date"
    [ testCase "produces output" $ do
        (rc, out, _) <- runHaskbox env "date" []
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "+%Y (year)" $ do
        (rc, out, _) <- runHaskbox env "date" ["+%Y"]
        rc @?= ExitSuccess
        -- Just check it produces a 4-digit year
        BS.length out @?= 5 -- "2024\n" or similar
    ]

envTests :: TestEnv -> TestTree
envTests env =
  testGroup
    "env"
    [ testCase "prints environment" $ do
        (rc, out, _) <- runHaskbox env "env" []
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out)
    ]

printenvTests :: TestEnv -> TestTree
printenvTests env =
  testGroup
    "printenv"
    [ testCase "prints all" $ do
        (rc, out, _) <- runHaskbox env "printenv" []
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "specific variable" $ compareOutput env "printenv" ["PATH"]
    ]

ttyTests :: TestEnv -> TestTree
ttyTests env =
  testGroup
    "tty"
    [ testCase "produces output" $ do
        (rc, out, _) <- runHaskbox env "tty" []
        -- tty exits 1 if not a terminal, which is expected in tests
        assertBool "should complete" (rc == ExitSuccess || rc == ExitFailure 1)
        assertBool "should produce output" (not $ BS.null out)
    ]

hostidTests :: TestEnv -> TestTree
hostidTests env =
  testGroup
    "hostid"
    [ testCase "produces output" $ do
        (rc, out, _) <- runHaskbox env "hostid" []
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out)
    ]

statTests :: TestEnv -> TestTree
statTests env =
  testGroup
    "stat"
    [ testCase "default output" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" [testFile]
        rc @?= ExitSuccess
        assertBool "should contain File:" (C8.pack "File:" `BS.isInfixOf` out)
        assertBool "should contain Size:" (C8.pack "Size:" `BS.isInfixOf` out)
        assertBool "should contain Access:" (C8.pack "Access:" `BS.isInfixOf` out),
      testCase "-c %n (filename)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%n", testFile]
        rc @?= ExitSuccess
        assertBool "should contain filename" (C8.pack "text" `BS.isInfixOf` out),
      testCase "-c %s (size)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%s", testFile]
        rc @?= ExitSuccess
        -- "hello\nworld\n" = 12 bytes
        C8.strip out @?= "12",
      testCase "-c %a (permissions octal)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%a", testFile]
        rc @?= ExitSuccess
        -- Should be something like 644 or 664
        assertBool "should be octal permissions" (BS.length (C8.strip out) >= 3),
      testCase "-c %A (permissions human)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%A", testFile]
        rc @?= ExitSuccess
        -- Should start with - (regular file)
        assertBool "should start with -" (C8.head (C8.strip out) == '-'),
      testCase "-c %F (file type)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%F", testFile]
        rc @?= ExitSuccess
        C8.strip out @?= "regular file",
      testCase "-c %F directory" $ do
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%F", envTmpDir env]
        rc @?= ExitSuccess
        C8.strip out @?= "directory",
      testCase "-c %u %g (uid gid)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%u %g", testFile]
        rc @?= ExitSuccess
        -- Should have two numbers
        let parts = C8.words (C8.strip out)
        length parts @?= 2,
      testCase "-c %U %G (user group names)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%U %G", testFile]
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "-c %i (inode)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%i", testFile]
        rc @?= ExitSuccess
        -- Should be a number
        assertBool "should be a number" (C8.all isDigit (C8.strip out)),
      testCase "-c %h (hard links)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%h", testFile]
        rc @?= ExitSuccess
        -- Regular file should have at least 1 link
        assertBool "should be >= 1" (read (C8.unpack $ C8.strip out) >= (1 :: Int)),
      testCase "-c %X %Y %Z (timestamps)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%X %Y %Z", testFile]
        rc @?= ExitSuccess
        let parts = C8.words (C8.strip out)
        length parts @?= 3,
      testCase "-c %x %y %z (human timestamps)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%y", testFile]
        rc @?= ExitSuccess
        -- Should contain date-like output
        assertBool "should contain -" (C8.pack "-" `BS.isInfixOf` out),
      testCase "-c with escapes" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%n\\t%s\\n%F", testFile]
        rc @?= ExitSuccess
        assertBool "should contain tab" (C8.pack "\t" `BS.isInfixOf` out)
        assertBool "should contain newline" (C8.count '\n' out >= 2),
      testCase "-c %d %D (device)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%d", testFile]
        rc @?= ExitSuccess
        assertBool "should be a number" (C8.all isDigit (C8.strip out)),
      testCase "-c %N (quoted name)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-c", "%N", testFile]
        rc @?= ExitSuccess
        assertBool "should be quoted" (C8.head (C8.strip out) == '\''),
      testCase "-t (terse)" $ do
        let testFile = envTmpDir env </> "text"
        (rc, out, _) <- runHaskbox env "stat" ["-t", testFile]
        rc @?= ExitSuccess
        -- Terse format has many space-separated fields
        let parts = C8.words (C8.strip out)
        assertBool "should have many fields" (length parts >= 10),
      testCase "nonexistent file" $ do
        (rc, _, err) <- runHaskbox env "stat" ["/nonexistent/file/path"]
        rc @?= ExitFailure 1
        assertBool "should have error" (not $ BS.null err),
      testCase "multiple files" $ do
        let file1 = envTmpDir env </> "text"
            file2 = envTmpDir env </> "empty"
        (rc, out, _) <- runHaskbox env "stat" [file1, file2]
        rc @?= ExitSuccess
        -- Should have output for both files (lines contain "File:")
        let fileLines = filter (C8.pack "File:" `BS.isInfixOf`) (C8.lines out)
        assertBool "should have multiple File: lines" (length fileLines >= 2),
      testCase "compare with GNU -c %s" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%s", testFile]
        case gnuResult of
          Nothing -> return () -- GNU stat not available
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%s", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb,
      testCase "compare with GNU -c %a" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%a", testFile]
        case gnuResult of
          Nothing -> return ()
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%a", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb,
      testCase "compare with GNU -c %F" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%F", testFile]
        case gnuResult of
          Nothing -> return ()
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%F", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb,
      testCase "compare with GNU -c %i" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%i", testFile]
        case gnuResult of
          Nothing -> return ()
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%i", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb,
      testCase "compare with GNU -c %h" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%h", testFile]
        case gnuResult of
          Nothing -> return ()
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%h", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb,
      testCase "compare with GNU -c %u" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%u", testFile]
        case gnuResult of
          Nothing -> return ()
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%u", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb,
      testCase "compare with GNU -c %g" $ do
        let testFile = envTmpDir env </> "text"
        gnuResult <- runGnu env "stat" ["-c", "%g", testFile]
        case gnuResult of
          Nothing -> return ()
          Just (rcGnu, outGnu, _) -> do
            (rcHb, outHb, _) <- runHaskbox env "stat" ["-c", "%g", testFile]
            rcGnu @?= rcHb
            C8.strip outGnu @?= C8.strip outHb
    ]
