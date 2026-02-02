{-# LANGUAGE OverloadedStrings #-}

-- | Tests for text processing commands: cat, head, tail, tac, wc, nl, fold, etc.
module Test.Cmd.Text (tests) where

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "Text Processing"
    [ catTests env,
      headTests env,
      tailTests env,
      tacTests env,
      wcTests env,
      nlTests env,
      foldTests env,
      expandTests env,
      unexpandTests env
    ]

catTests :: TestEnv -> TestTree
catTests env =
  testGroup
    "cat"
    [ testCase "simple file" $ compareOutput env "cat" [envTmpDir env </> "text"],
      testCase "empty file" $ compareOutput env "cat" [envTmpDir env </> "empty"],
      testCase "multiple files" $
        compareOutput env "cat" [envTmpDir env </> "text", envTmpDir env </> "text"],
      testCase "-n (number lines)" $ compareOutput env "cat" ["-n", envTmpDir env </> "text"],
      testCase "-b (number non-blank)" $ compareOutput env "cat" ["-b", envTmpDir env </> "blanks"],
      testCase "-s (squeeze blank)" $ compareOutput env "cat" ["-s", envTmpDir env </> "blanks"],
      testCase "-E (show ends)" $ compareOutput env "cat" ["-E", envTmpDir env </> "text"],
      testCase "-T (show tabs)" $ compareOutput env "cat" ["-T", envTmpDir env </> "tabs"]
    ]

headTests :: TestEnv -> TestTree
headTests env =
  testGroup
    "head"
    [ testCase "default (10 lines)" $ compareOutput env "head" [envTmpDir env </> "many_lines"],
      testCase "-n 5" $ compareOutput env "head" ["-n", "5", envTmpDir env </> "tenlines"],
      testCase "-c 10" $ compareOutput env "head" ["-c", "10", envTmpDir env </> "text"]
    ]

tailTests :: TestEnv -> TestTree
tailTests env =
  testGroup
    "tail"
    [ testCase "default (10 lines)" $ compareOutput env "tail" [envTmpDir env </> "many_lines"],
      testCase "-n 3" $ compareOutput env "tail" ["-n", "3", envTmpDir env </> "tenlines"],
      testCase "-c 5" $ compareOutput env "tail" ["-c", "5", envTmpDir env </> "text"]
    ]

tacTests :: TestEnv -> TestTree
tacTests env =
  testGroup
    "tac"
    [ testCase "reverse lines" $ compareOutput env "tac" [envTmpDir env </> "lines3"]
    ]

wcTests :: TestEnv -> TestTree
wcTests env =
  testGroup
    "wc"
    [ testCase "default" $ compareOutput env "wc" [envTmpDir env </> "wc_test"],
      testCase "-l (lines)" $ compareOutput env "wc" ["-l", envTmpDir env </> "wc_test"],
      testCase "-w (words)" $ compareOutput env "wc" ["-w", envTmpDir env </> "wc_test"],
      testCase "-c (bytes)" $ compareOutput env "wc" ["-c", envTmpDir env </> "wc_test"],
      testCase "-L (max line length)" $ compareOutput env "wc" ["-L", envTmpDir env </> "wc_test"],
      testCase "multiple files" $
        compareOutput env "wc" [envTmpDir env </> "text", envTmpDir env </> "wc_test"]
    ]

nlTests :: TestEnv -> TestTree
nlTests env =
  testGroup
    "nl"
    [ testCase "default" $ compareOutput env "nl" [envTmpDir env </> "for_nl"],
      testCase "-ba (number all)" $ compareOutput env "nl" ["-ba", envTmpDir env </> "for_nl"],
      testCase "-bt (number non-empty)" $ compareOutput env "nl" ["-bt", envTmpDir env </> "for_nl"]
    ]

foldTests :: TestEnv -> TestTree
foldTests env =
  testGroup
    "fold"
    [ testCase "-w 10" $ compareOutput env "fold" ["-w", "10", envTmpDir env </> "longline"]
    ]

expandTests :: TestEnv -> TestTree
expandTests env =
  testGroup
    "expand"
    [ testCase "default" $ compareOutput env "expand" [envTmpDir env </> "tabbed"],
      testCase "-t 4" $ compareOutput env "expand" ["-t", "4", envTmpDir env </> "tabbed"]
    ]

unexpandTests :: TestEnv -> TestTree
unexpandTests env =
  testGroup
    "unexpand"
    [ testCase "-a" $ compareOutput env "unexpand" ["-a", envTmpDir env </> "spaced"]
    ]
