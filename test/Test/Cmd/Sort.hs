{-# LANGUAGE OverloadedStrings #-}

-- | Tests for sorting and filtering commands: sort, uniq, comm, cut, paste, tr
module Test.Cmd.Sort (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "Sorting & Filtering"
    [ sortTests env,
      uniqTests env,
      commTests env,
      cutTests env,
      pasteTests env,
      trTests env,
      shufTests env
    ]

sortTests :: TestEnv -> TestTree
sortTests env =
  testGroup
    "sort"
    [ testCase "default" $ compareOutput env "sort" [envTmpDir env </> "unsorted"],
      testCase "-r (reverse)" $ compareOutput env "sort" ["-r", envTmpDir env </> "unsorted"],
      testCase "-n (numeric)" $ compareOutput env "sort" ["-n", envTmpDir env </> "numbers"],
      testCase "-u (unique)" $ compareOutput env "sort" ["-u", envTmpDir env </> "unsorted"],
      testCase "-c (check sorted)" $ compareOutput env "sort" ["-c", envTmpDir env </> "sorted"],
      testCase "-c (check unsorted)" $ compareOutput env "sort" ["-c", envTmpDir env </> "unsorted"]
    ]

uniqTests :: TestEnv -> TestTree
uniqTests env =
  testGroup
    "uniq"
    [ testCase "default" $ compareOutput env "uniq" [envTmpDir env </> "duplicates"],
      testCase "-c (count)" $ compareOutput env "uniq" ["-c", envTmpDir env </> "duplicates"],
      testCase "-d (duplicates only)" $ compareOutput env "uniq" ["-d", envTmpDir env </> "duplicates"],
      testCase "-u (unique only)" $ compareOutput env "uniq" ["-u", envTmpDir env </> "duplicates"],
      testCase "-i (ignore case)" $ compareOutputStdin env "uniq" ["-i"] "AAA\naaa\nBBB\n"
    ]

commTests :: TestEnv -> TestTree
commTests env =
  testGroup
    "comm"
    [ testCase "default" $
        compareOutput env "comm" [envTmpDir env </> "comm1", envTmpDir env </> "comm2"],
      testCase "-1 (suppress col 1)" $
        compareOutput env "comm" ["-1", envTmpDir env </> "comm1", envTmpDir env </> "comm2"],
      testCase "-12 (only col 3)" $
        compareOutput env "comm" ["-12", envTmpDir env </> "comm1", envTmpDir env </> "comm2"]
    ]

cutTests :: TestEnv -> TestTree
cutTests env =
  testGroup
    "cut"
    [ testCase "-c1-5" $ compareOutput env "cut" ["-c1-5", envTmpDir env </> "text"],
      testCase "-f1 -d:" $ compareOutput env "cut" ["-f1", "-d:", envTmpDir env </> "fields"],
      testCase "-f1,3 -d:" $ compareOutput env "cut" ["-f1,3", "-d:", envTmpDir env </> "fields"],
      testCase "-f2- -d:" $ compareOutput env "cut" ["-f2-", "-d:", envTmpDir env </> "fields"],
      testCase "csv field" $ compareOutput env "cut" ["-f2", "-d,", envTmpDir env </> "csv"]
    ]

pasteTests :: TestEnv -> TestTree
pasteTests env =
  testGroup
    "paste"
    [ testCase "two files" $
        compareOutput env "paste" [envTmpDir env </> "col1", envTmpDir env </> "col2"],
      testCase "-d, (delimiter)" $
        compareOutput env "paste" ["-d,", envTmpDir env </> "col1", envTmpDir env </> "col2"],
      testCase "-s (serial)" $
        compareOutput env "paste" ["-s", envTmpDir env </> "col1", envTmpDir env </> "col2"]
    ]

trTests :: TestEnv -> TestTree
trTests env =
  testGroup
    "tr"
    [ testCase "lowercase to uppercase" $
        compareOutputStdin env "tr" ["a-z", "A-Z"] "hello world\n",
      testCase "-d (delete)" $
        compareOutputStdin env "tr" ["-d", "aeiou"] "hello world\n",
      testCase "-s (squeeze)" $
        compareOutputStdin env "tr" ["-s", " "] "hello   world\n",
      testCase "-c (complement)" $
        compareOutputStdin env "tr" ["-cd", "a-z\n"] "hello123world\n"
    ]

shufTests :: TestEnv -> TestTree
shufTests env =
  testGroup
    "shuf"
    [ testCase "produces output" $ do
        (rc, out, _) <- runHaskbox env "shuf" ["-n", "3", envTmpDir env </> "unsorted"]
        rc @?= ExitSuccess
        assertBool "should produce output" (not $ BS.null out),
      testCase "-e (echo args)" $ do
        (rc, out, _) <- runHaskbox env "shuf" ["-e", "a", "b", "c"]
        rc @?= ExitSuccess
        let lineCount = length $ filter (not . BS.null) $ C8.lines out
        lineCount @?= 3
    ]
