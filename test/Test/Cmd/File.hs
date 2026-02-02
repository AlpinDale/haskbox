{-# LANGUAGE OverloadedStrings #-}

-- | Tests for file operations: cp, mv, rm, mkdir, rmdir, ln, touch, stat, etc.
module Test.Cmd.File (tests) where

import Control.Monad (when)
import Data.ByteString qualified as BS
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (readSymbolicLink)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "File Operations"
    [ mkdirTests env,
      rmdirTests env,
      touchTests env,
      cpTests env,
      mvTests env,
      rmTests env,
      lnTests env,
      linkTests env,
      unlinkTests env,
      statTests env,
      truncateTests env
    ]

mkdirTests :: TestEnv -> TestTree
mkdirTests env =
  testGroup
    "mkdir"
    [ testCase "create directory" $ do
        let dir = envTmpDir env </> "newdir"
        (rc, _, _) <- runHaskbox env "mkdir" [dir]
        rc @?= ExitSuccess
        exists <- doesDirectoryExist dir
        assertBool "directory should exist" exists,
      testCase "-p (create parents)" $ do
        let dir = envTmpDir env </> "parent" </> "child" </> "grandchild"
        (rc, _, _) <- runHaskbox env "mkdir" ["-p", dir]
        rc @?= ExitSuccess
        exists <- doesDirectoryExist dir
        assertBool "directory should exist" exists
    ]

rmdirTests :: TestEnv -> TestTree
rmdirTests env =
  testGroup
    "rmdir"
    [ testCase "remove empty directory" $ do
        let dir = envTmpDir env </> "emptydir"
        createDirectory dir
        (rc, _, _) <- runHaskbox env "rmdir" [dir]
        rc @?= ExitSuccess
        exists <- doesDirectoryExist dir
        assertBool "directory should not exist" (not exists)
    ]

touchTests :: TestEnv -> TestTree
touchTests env =
  testGroup
    "touch"
    [ testCase "create file" $ do
        let file = envTmpDir env </> "touched"
        (rc, _, _) <- runHaskbox env "touch" [file]
        rc @?= ExitSuccess
        exists <- doesFileExist file
        assertBool "file should exist" exists,
      testCase "update existing file" $ do
        let file = envTmpDir env </> "text"
        (rc, _, _) <- runHaskbox env "touch" [file]
        rc @?= ExitSuccess
    ]

cpTests :: TestEnv -> TestTree
cpTests env =
  testGroup
    "cp"
    [ testCase "copy file" $ do
        let src = envTmpDir env </> "text"
            dst = envTmpDir env </> "text_copy"
        (rc, _, _) <- runHaskbox env "cp" [src, dst]
        rc @?= ExitSuccess
        exists <- doesFileExist dst
        assertBool "copy should exist" exists
        srcContent <- BS.readFile src
        dstContent <- BS.readFile dst
        srcContent @?= dstContent
    ]

mvTests :: TestEnv -> TestTree
mvTests env =
  testGroup
    "mv"
    [ testCase "move file" $ do
        let src = envTmpDir env </> "for_move"
            dst = envTmpDir env </> "moved"
        BS.writeFile src "move me\n"
        (rc, _, _) <- runHaskbox env "mv" [src, dst]
        rc @?= ExitSuccess
        srcExists <- doesFileExist src
        dstExists <- doesFileExist dst
        assertBool "source should not exist" (not srcExists)
        assertBool "destination should exist" dstExists
    ]

rmTests :: TestEnv -> TestTree
rmTests env =
  testGroup
    "rm"
    [ testCase "remove file" $ do
        let file = envTmpDir env </> "to_remove"
        BS.writeFile file "delete me\n"
        (rc, _, _) <- runHaskbox env "rm" [file]
        rc @?= ExitSuccess
        exists <- doesFileExist file
        assertBool "file should not exist" (not exists),
      testCase "-f (force, no error on missing)" $ do
        let file = envTmpDir env </> "nonexistent"
        (rc, _, _) <- runHaskbox env "rm" ["-f", file]
        rc @?= ExitSuccess
    ]

lnTests :: TestEnv -> TestTree
lnTests env =
  testGroup
    "ln"
    [ testCase "-s (symbolic link)" $ do
        let target = envTmpDir env </> "text"
            link = envTmpDir env </> "text_symlink"
        -- Clean up if exists from previous run
        linkExists <- doesFileExist link
        when linkExists $ removeFile link
        (rc, _, _) <- runHaskbox env "ln" ["-s", target, link]
        rc @?= ExitSuccess
        linkTarget <- readSymbolicLink link
        linkTarget @?= target
    ]

linkTests :: TestEnv -> TestTree
linkTests env =
  testGroup
    "link"
    [ testCase "create hard link" $ do
        let src = envTmpDir env </> "text"
            dst = envTmpDir env </> "text_hardlink"
        (rc, _, _) <- runHaskbox env "link" [src, dst]
        rc @?= ExitSuccess
        exists <- doesFileExist dst
        assertBool "hard link should exist" exists
    ]

unlinkTests :: TestEnv -> TestTree
unlinkTests env =
  testGroup
    "unlink"
    [ testCase "remove file" $ do
        let file = envTmpDir env </> "to_unlink"
        BS.writeFile file "unlink me\n"
        (rc, _, _) <- runHaskbox env "unlink" [file]
        rc @?= ExitSuccess
        exists <- doesFileExist file
        assertBool "file should not exist" (not exists)
    ]

statTests :: TestEnv -> TestTree
statTests env =
  testGroup
    "stat"
    [ testCase "file stats" $ do
        (rc, out, _) <- runHaskbox env "stat" [envTmpDir env </> "text"]
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "-c %s (size)" $ compareOutput env "stat" ["-c", "%s", envTmpDir env </> "text"],
      testCase "-c %n (name)" $ compareOutput env "stat" ["-c", "%n", envTmpDir env </> "text"]
    ]

truncateTests :: TestEnv -> TestTree
truncateTests env =
  testGroup
    "truncate"
    [ testCase "-s 5" $ do
        let file = envTmpDir env </> "to_truncate"
        BS.writeFile file "hello world\n"
        (rc, _, _) <- runHaskbox env "truncate" ["-s", "5", file]
        rc @?= ExitSuccess
        content <- BS.readFile file
        BS.length content @?= 5
    ]
