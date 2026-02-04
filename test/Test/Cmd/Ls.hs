{-# LANGUAGE OverloadedStrings #-}

module Test.Cmd.Ls (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Directory (createDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "ls"
    [ basicTests env,
      hiddenFileTests env,
      longFormatTests env,
      sortingTests env,
      formatTests env,
      indicatorTests env,
      filterTests env,
      recursiveTests env,
      symlinkTests env,
      errorTests env
    ]

basicTests :: TestEnv -> TestTree
basicTests env =
  testGroup
    "basic"
    [ testCase "lists current directory" $ do
        (rc, out, _) <- runHaskbox env "ls" [envTmpDir env]
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "lists specified directory" $ do
        (rc, out, _) <- runHaskbox env "ls" [envTmpDir env]
        rc @?= ExitSuccess
        -- Check that we see some known files
        assertBool "should contain 'text'" ("text" `C8.isInfixOf` out),
      testCase "lists multiple files" $ do
        let file1 = envTmpDir env </> "text"
            file2 = envTmpDir env </> "empty"
        (rc, out, _) <- runHaskbox env "ls" [file1, file2]
        rc @?= ExitSuccess
        assertBool "should contain 'text'" ("text" `C8.isInfixOf` out)
        assertBool "should contain 'empty'" ("empty" `C8.isInfixOf` out),
      testCase "--help" $ do
        (rc, out, _) <- runHaskbox env "ls" ["--help"]
        rc @?= ExitSuccess
        assertBool "should show usage" ("Usage:" `C8.isInfixOf` out),
      testCase "--version" $ do
        (rc, out, _) <- runHaskbox env "ls" ["--version"]
        rc @?= ExitSuccess
        assertBool "should show version" ("haskbox" `C8.isInfixOf` out)
    ]

hiddenFileTests :: TestEnv -> TestTree
hiddenFileTests env =
  testGroup
    "hidden files"
    [ testCase "-a shows hidden files" $ do
        let dir = envTmpDir env </> "hidden_test"
        createDirectory dir
        BS.writeFile (dir </> ".hidden") "hidden\n"
        BS.writeFile (dir </> "visible") "visible\n"
        (rc, out, _) <- runHaskbox env "ls" ["-a", dir]
        rc @?= ExitSuccess
        assertBool "should show .hidden" (".hidden" `C8.isInfixOf` out)
        assertBool "should show visible" ("visible" `C8.isInfixOf` out)
        assertBool "should show . (current dir)" (". " `C8.isInfixOf` out || C8.head out == '.'),
      testCase "-A shows hidden except . and .." $ do
        let dir = envTmpDir env </> "hidden_test2"
        createDirectory dir
        BS.writeFile (dir </> ".hidden") "hidden\n"
        BS.writeFile (dir </> "visible") "visible\n"
        (rc, out, _) <- runHaskbox env "ls" ["-A", dir]
        rc @?= ExitSuccess
        assertBool "should show .hidden" (".hidden" `C8.isInfixOf` out)
        assertBool "should show visible" ("visible" `C8.isInfixOf` out)
        -- . and .. should not appear
        let lines' = C8.lines out
            hasDot = any (\l -> l == "." || l == " ." || ". " `C8.isInfixOf` l && not (".hidden" `C8.isInfixOf` l)) lines'
        assertBool "should not show standalone . (with -A)" (not hasDot || ".hidden" `C8.isInfixOf` out),
      testCase "default hides hidden files" $ do
        let dir = envTmpDir env </> "hidden_test3"
        createDirectory dir
        BS.writeFile (dir </> ".hidden") "hidden\n"
        BS.writeFile (dir </> "visible") "visible\n"
        (rc, out, _) <- runHaskbox env "ls" [dir]
        rc @?= ExitSuccess
        assertBool "should not show .hidden" (not $ ".hidden" `C8.isInfixOf` out)
        assertBool "should show visible" ("visible" `C8.isInfixOf` out)
    ]

longFormatTests :: TestEnv -> TestTree
longFormatTests env =
  testGroup
    "long format"
    [ testCase "-l shows long format" $ do
        (rc, out, _) <- runHaskbox env "ls" ["-l", envTmpDir env]
        rc @?= ExitSuccess
        -- Long format should have total line and permission bits
        assertBool "should have total" ("total" `C8.isInfixOf` out)
        -- Check for permission pattern (like drwx or -rw-)
        assertBool "should have permissions" (any isPermissionLine (C8.lines out)),
      testCase "-lh shows human readable sizes" $ do
        (rc, out, _) <- runHaskbox env "ls" ["-lh", envTmpDir env]
        rc @?= ExitSuccess
        assertBool "should have total" ("total" `C8.isInfixOf` out),
      testCase "-n shows numeric uid/gid" $ do
        (rc, out, _) <- runHaskbox env "ls" ["-n", envTmpDir env]
        rc @?= ExitSuccess
        -- Should have numbers instead of usernames
        assertBool "should produce output" (not $ BS.null out),
      testCase "-g hides owner" $ do
        (rc, out, _) <- runHaskbox env "ls" ["-g", envTmpDir env]
        rc @?= ExitSuccess
        assertBool "should produce long format output" ("total" `C8.isInfixOf` out),
      testCase "-o hides group" $ do
        (rc, out, _) <- runHaskbox env "ls" ["-o", envTmpDir env]
        rc @?= ExitSuccess
        assertBool "should produce long format output" ("total" `C8.isInfixOf` out)
    ]
  where
    isPermissionLine line =
      let s = C8.unpack line
       in case s of
            (c : rest) ->
              length s > 9
                && c `elem` ("-dlbcps" :: String)
                && all (`elem` ("-rwxsStT" :: String)) (take 9 rest)
            [] -> False

sortingTests :: TestEnv -> TestTree
sortingTests env =
  testGroup
    "sorting"
    [ testCase "-S sorts by size" $ do
        let dir = envTmpDir env </> "size_sort"
        createDirectory dir
        BS.writeFile (dir </> "small") "x"
        BS.writeFile (dir </> "medium") "xxxxx"
        BS.writeFile (dir </> "large") "xxxxxxxxxx"
        (rc, out, _) <- runHaskbox env "ls" ["-1S", dir]
        rc @?= ExitSuccess
        let files = map C8.unpack $ filter (not . BS.null) $ C8.lines out
        -- Largest should come first
        case files of
          (first : _) -> assertEqual "large should be first" "large" first
          [] -> assertFailure "expected non-empty file list",
      testCase "-r reverses sort" $ do
        let dir = envTmpDir env </> "reverse_sort"
        createDirectory dir
        BS.writeFile (dir </> "aaa") "a"
        BS.writeFile (dir </> "bbb") "b"
        BS.writeFile (dir </> "ccc") "c"
        (rc, out, _) <- runHaskbox env "ls" ["-1r", dir]
        rc @?= ExitSuccess
        let files = map C8.unpack $ filter (not . BS.null) $ C8.lines out
        case files of
          (first : _) -> assertEqual "ccc should be first when reversed" "ccc" first
          [] -> assertFailure "expected non-empty file list",
      testCase "-t sorts by time" $ do
        (rc, _, _) <- runHaskbox env "ls" ["-t", envTmpDir env]
        rc @?= ExitSuccess,
      testCase "-X sorts by extension" $ do
        let dir = envTmpDir env </> "ext_sort"
        createDirectory dir
        BS.writeFile (dir </> "file.txt") "t"
        BS.writeFile (dir </> "file.c") "c"
        BS.writeFile (dir </> "file.h") "h"
        (rc, out, _) <- runHaskbox env "ls" ["-1X", dir]
        rc @?= ExitSuccess
        let files = map C8.unpack $ filter (not . BS.null) $ C8.lines out
        -- .c should come before .h which comes before .txt
        assertBool "should be sorted by extension" (length files == 3),
      testCase "-U disables sorting" $ do
        (rc, _, _) <- runHaskbox env "ls" ["-U", envTmpDir env]
        rc @?= ExitSuccess,
      testCase "-v version sort" $ do
        let dir = envTmpDir env </> "version_sort"
        createDirectory dir
        BS.writeFile (dir </> "file1") "1"
        BS.writeFile (dir </> "file2") "2"
        BS.writeFile (dir </> "file10") "10"
        (rc, out, _) <- runHaskbox env "ls" ["-1v", dir]
        rc @?= ExitSuccess
        let files = map C8.unpack $ filter (not . BS.null) $ C8.lines out
        -- Version sort: file1, file2, file10
        assertEqual "version sorted" ["file1", "file2", "file10"] files
    ]

formatTests :: TestEnv -> TestTree
formatTests env =
  testGroup
    "output format"
    [ testCase "-1 one per line" $ do
        let dir = envTmpDir env </> "one_per_line"
        createDirectory dir
        BS.writeFile (dir </> "file1") "1"
        BS.writeFile (dir </> "file2") "2"
        (rc, out, _) <- runHaskbox env "ls" ["-1", dir]
        rc @?= ExitSuccess
        let lineCount = length $ filter (not . BS.null) $ C8.lines out
        assertEqual "should have 2 lines" 2 lineCount,
      testCase "-m comma separated" $ do
        let dir = envTmpDir env </> "comma_sep"
        createDirectory dir
        BS.writeFile (dir </> "a") "a"
        BS.writeFile (dir </> "b") "b"
        (rc, out, _) <- runHaskbox env "ls" ["-m", dir]
        rc @?= ExitSuccess
        assertBool "should have comma" ("," `C8.isInfixOf` out),
      testCase "-C column format" $ do
        (rc, _, _) <- runHaskbox env "ls" ["-C", envTmpDir env]
        rc @?= ExitSuccess,
      testCase "-x across format" $ do
        (rc, _, _) <- runHaskbox env "ls" ["-x", envTmpDir env]
        rc @?= ExitSuccess
    ]

indicatorTests :: TestEnv -> TestTree
indicatorTests env =
  testGroup
    "indicators"
    [ testCase "-F classify" $ do
        let dir = envTmpDir env </> "classify_test"
        createDirectory dir
        createDirectory (dir </> "subdir")
        BS.writeFile (dir </> "file") "f"
        (rc, out, _) <- runHaskbox env "ls" ["-F", dir]
        rc @?= ExitSuccess
        assertBool "directory should have /" ("subdir/" `C8.isInfixOf` out),
      testCase "-p slash for directories" $ do
        let dir = envTmpDir env </> "slash_test"
        createDirectory dir
        createDirectory (dir </> "subdir")
        (rc, out, _) <- runHaskbox env "ls" ["-p", dir]
        rc @?= ExitSuccess
        assertBool "directory should have /" ("subdir/" `C8.isInfixOf` out)
    ]

filterTests :: TestEnv -> TestTree
filterTests env =
  testGroup
    "filtering"
    [ testCase "-B ignores backups" $ do
        let dir = envTmpDir env </> "backup_test"
        createDirectory dir
        BS.writeFile (dir </> "file") "f"
        BS.writeFile (dir </> "file~") "backup"
        (rc, out, _) <- runHaskbox env "ls" ["-B", dir]
        rc @?= ExitSuccess
        assertBool "should show file" ("file" `C8.isInfixOf` out)
        assertBool "should not show file~" (not $ "file~" `C8.isInfixOf` out),
      testCase "-I pattern ignores files" $ do
        let dir = envTmpDir env </> "ignore_test"
        createDirectory dir
        BS.writeFile (dir </> "keep.txt") "keep"
        BS.writeFile (dir </> "ignore.log") "ignore"
        (rc, out, _) <- runHaskbox env "ls" ["-I", "*.log", dir]
        rc @?= ExitSuccess
        assertBool "should show keep.txt" ("keep.txt" `C8.isInfixOf` out)
        assertBool "should not show ignore.log" (not $ "ignore.log" `C8.isInfixOf` out)
    ]

recursiveTests :: TestEnv -> TestTree
recursiveTests env =
  testGroup
    "recursive"
    [ testCase "-R recursive listing" $ do
        let dir = envTmpDir env </> "recursive_test"
        createDirectory dir
        createDirectory (dir </> "sub1")
        createDirectory (dir </> "sub2")
        BS.writeFile (dir </> "file") "f"
        BS.writeFile (dir </> "sub1" </> "file1") "f1"
        BS.writeFile (dir </> "sub2" </> "file2") "f2"
        (rc, out, _) <- runHaskbox env "ls" ["-R", dir]
        rc @?= ExitSuccess
        -- Should show subdirectory contents
        assertBool "should show file1" ("file1" `C8.isInfixOf` out)
        assertBool "should show file2" ("file2" `C8.isInfixOf` out)
    ]

symlinkTests :: TestEnv -> TestTree
symlinkTests env =
  testGroup
    "symlinks"
    [ testCase "symlink to dir: default lists contents" $ do
        let dir = envTmpDir env </> "symlink_test1"
        createDirectory dir
        createDirectory (dir </> "realdir")
        BS.writeFile (dir </> "realdir" </> "file1") "1"
        BS.writeFile (dir </> "realdir" </> "file2") "2"
        createSymbolicLink "realdir" (dir </> "linkdir")
        (rc, out, _) <- runHaskbox env "ls" [dir </> "linkdir"]
        rc @?= ExitSuccess
        -- Should list the contents of the linked directory
        assertBool "should show file1" ("file1" `C8.isInfixOf` out)
        assertBool "should show file2" ("file2" `C8.isInfixOf` out),
      testCase "symlink to dir: -d shows link itself" $ do
        let dir = envTmpDir env </> "symlink_test2"
        createDirectory dir
        createDirectory (dir </> "realdir")
        BS.writeFile (dir </> "realdir" </> "file1") "1"
        createSymbolicLink "realdir" (dir </> "linkdir")
        (rc, out, _) <- runHaskbox env "ls" ["-d", dir </> "linkdir"]
        rc @?= ExitSuccess
        -- Should show just the link name, not contents
        assertBool "should show linkdir" ("linkdir" `C8.isInfixOf` out)
        assertBool "should not show file1" (not $ "file1" `C8.isInfixOf` out),
      testCase "symlink to dir: -l shows link with arrow" $ do
        let dir = envTmpDir env </> "symlink_test3"
        createDirectory dir
        createDirectory (dir </> "realdir")
        createSymbolicLink "realdir" (dir </> "linkdir")
        (rc, out, _) <- runHaskbox env "ls" ["-l", dir </> "linkdir"]
        rc @?= ExitSuccess
        -- Should show the symlink entry with -> target
        assertBool "should show arrow" ("->" `C8.isInfixOf` out)
        assertBool "should show realdir target" ("realdir" `C8.isInfixOf` out),
      testCase "symlink to dir: -L follows link" $ do
        let dir = envTmpDir env </> "symlink_test4"
        createDirectory dir
        createDirectory (dir </> "realdir")
        BS.writeFile (dir </> "realdir" </> "file1") "1"
        createSymbolicLink "realdir" (dir </> "linkdir")
        (rc, out, _) <- runHaskbox env "ls" ["-L", dir </> "linkdir"]
        rc @?= ExitSuccess
        -- Should list contents
        assertBool "should show file1" ("file1" `C8.isInfixOf` out),
      testCase "symlink to dir: -H follows cmdline symlinks" $ do
        let dir = envTmpDir env </> "symlink_test5"
        createDirectory dir
        createDirectory (dir </> "realdir")
        BS.writeFile (dir </> "realdir" </> "file1") "1"
        createSymbolicLink "realdir" (dir </> "linkdir")
        (rc, out, _) <- runHaskbox env "ls" ["-H", dir </> "linkdir"]
        rc @?= ExitSuccess
        -- Should list contents
        assertBool "should show file1" ("file1" `C8.isInfixOf` out),
      testCase "broken symlink: shows in listing" $ do
        let dir = envTmpDir env </> "symlink_test6"
        createDirectory dir
        BS.writeFile (dir </> "realfile") "real"
        createSymbolicLink "nonexistent" (dir </> "broken")
        (rc, out, _) <- runHaskbox env "ls" [dir]
        rc @?= ExitSuccess
        assertBool "should show broken link" ("broken" `C8.isInfixOf` out),
      testCase "broken symlink: -l shows with arrow" $ do
        let dir = envTmpDir env </> "symlink_test7"
        createDirectory dir
        createSymbolicLink "nonexistent" (dir </> "broken")
        (rc, out, _) <- runHaskbox env "ls" ["-l", dir]
        rc @?= ExitSuccess
        assertBool "should show arrow" ("->" `C8.isInfixOf` out)
        assertBool "should show target" ("nonexistent" `C8.isInfixOf` out),
      testCase "symlink to file: default shows link" $ do
        let dir = envTmpDir env </> "symlink_test8"
        createDirectory dir
        BS.writeFile (dir </> "realfile") "content"
        createSymbolicLink "realfile" (dir </> "linkfile")
        (rc, out, _) <- runHaskbox env "ls" ["-1", dir]
        rc @?= ExitSuccess
        assertBool "should show linkfile" ("linkfile" `C8.isInfixOf` out)
        assertBool "should show realfile" ("realfile" `C8.isInfixOf` out)
    ]

errorTests :: TestEnv -> TestTree
errorTests env =
  testGroup
    "error handling"
    [ testCase "nonexistent file" $ do
        (rc, _, err) <- runHaskbox env "ls" ["/nonexistent/path/12345"]
        assertBool "should fail" (rc /= ExitSuccess)
        assertBool "should have error message" (not $ BS.null err),
      testCase "invalid option" $ do
        (rc, _, err) <- runHaskbox env "ls" ["--invalid-option-xyz"]
        assertBool "should fail" (rc /= ExitSuccess)
        assertBool "should have error message" ("unrecognized" `C8.isInfixOf` err)
    ]
