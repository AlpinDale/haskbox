{-# LANGUAGE OverloadedStrings #-}

-- | Tests for path manipulation commands: basename, dirname, pwd, realpath, readlink
module Test.Cmd.Path (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "Path Manipulation"
    [ basenameTests env,
      dirnameTests env,
      pwdTests env,
      realpathTests env,
      readlinkTests env
    ]

basenameTests :: TestEnv -> TestTree
basenameTests env =
  testGroup
    "basename"
    [ testCase "simple path" $ compareOutput env "basename" ["/usr/bin/test"],
      testCase "trailing slash" $ compareOutput env "basename" ["/path/to/dir/"]
    ]

dirnameTests :: TestEnv -> TestTree
dirnameTests env =
  testGroup
    "dirname"
    [ testCase "simple path" $ compareOutput env "dirname" ["/usr/bin/test"],
      testCase "file only" $ compareOutput env "dirname" ["file.txt"],
      testCase "root" $ compareOutput env "dirname" ["/"]
    ]

pwdTests :: TestEnv -> TestTree
pwdTests env =
  testGroup
    "pwd"
    [ testCase "prints current directory" $ do
        (rc, out, _) <- runHaskbox env "pwd" []
        rc @?= ExitSuccess
        cwd <- getCurrentDirectory
        C8.unpack (C8.init out) @?= cwd -- Remove trailing newline
    ]

realpathTests :: TestEnv -> TestTree
realpathTests env =
  testGroup
    "realpath"
    [ testCase "existing file" $ do
        (rc, out, _) <- runHaskbox env "realpath" [envTmpDir env </> "text"]
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out)
    ]

readlinkTests :: TestEnv -> TestTree
readlinkTests env =
  testGroup
    "readlink"
    [ testCase "-f (canonicalize)" $ do
        (rc, out, _) <- runHaskbox env "readlink" ["-f", envTmpDir env </> "text"]
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out)
    ]
