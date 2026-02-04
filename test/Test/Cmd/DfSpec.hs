{-# LANGUAGE OverloadedStrings #-}

-- | Spec tests for df command - compares output against GNU df
module Test.Cmd.DfSpec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import System.Directory
  ( doesFileExist,
    findExecutable,
  )
import System.FilePath (takeFileName)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

data SpecCase = SpecCase
  { scName :: String,
    scArgs :: [String],
    scEnv :: [(String, String)],
    scCompareMode :: CompareMode
  }

data CompareMode
  = -- | Compare exact output (after normalization)
    CompareExact
  | -- | Compare only exit code and header structure
    CompareStructure
  | -- | Compare exit code only
    CompareExitOnly

tests :: TestEnv -> TestTree
tests env =
  withResource findGnuDf (\_ -> pure ()) $ \getGnuDf ->
    testGroup
      "df-spec"
      (map (mkTest env getGnuDf) specCases)

mkTest ::
  TestEnv ->
  IO (Maybe FilePath) ->
  SpecCase ->
  TestTree
mkTest env getGnuDf sc =
  testCase (scName sc) $ do
    mGnuDf <- getGnuDf
    case mGnuDf of
      Nothing -> assertFailure "GNU df not found (install coreutils)"
      Just gnuDf -> do
        let args = scArgs sc
            envVars = baseEnv ++ scEnv sc

        (rcRef, outRef, errRef) <- runCmdWithEnv gnuDf args envVars
        (rcTest, outTest, errTest) <- runCmdWithEnv (envHaskbox env) ("df" : args) envVars

        let gnuPrefix = if takeFileName gnuDf == "gdf" then "g" else ""
            errRefNorm = normalizeErr "df" gnuPrefix errRef
            errTestNorm = normalizeErr "df" gnuPrefix errTest

        case scCompareMode sc of
          CompareExact -> do
            rcRef @?= rcTest
            normalizeOutput outRef @=? normalizeOutput outTest
            errRefNorm @=? errTestNorm
          CompareStructure -> do
            rcRef @?= rcTest
            compareStructure outRef outTest
          CompareExitOnly -> do
            rcRef @?= rcTest

baseEnv :: [(String, String)]
baseEnv =
  [ ("LC_ALL", "C"),
    ("LANG", "C"),
    ("TZ", "UTC"),
    ("POSIXLY_CORRECT", "1")
  ]

-- | Normalize df output for comparison
-- - Normalize whitespace in each line
-- - Don't compare actual values (they change), just structure
normalizeOutput :: BS.ByteString -> BS.ByteString
normalizeOutput = C8.unlines . map normalizeLine . C8.lines
  where
    normalizeLine = C8.unwords . C8.words

-- | Compare structure: same number of lines, same number of columns
compareStructure :: BS.ByteString -> BS.ByteString -> Assertion
compareStructure ref test = do
  let refLines = filter (not . BS.null) $ C8.lines ref
      testLines = filter (not . BS.null) $ C8.lines test
  -- Both should have at least a header
  assertBool "reference has lines" (not (null refLines))
  assertBool "test has lines" (not (null testLines))
  -- Header should have same number of columns
  case (refLines, testLines) of
    (refHeader : _, testHeader : _) -> do
      let refCols = length $ C8.words refHeader
          testCols = length $ C8.words testHeader
      assertEqual "same number of header columns" refCols testCols
    _ -> assertFailure "empty lines"

-- | Find GNU df (gdf on macOS, df on Linux)
findGnuDf :: IO (Maybe FilePath)
findGnuDf = do
  isMac <- doesFileExist "/usr/bin/sw_vers"
  if isMac
    then do
      mGdf <- findExecutable "gdf"
      case mGdf of
        Just path -> return (Just path)
        Nothing -> do
          -- Try Homebrew path directly
          let brewPath = "/opt/homebrew/bin/gdf"
          exists <- doesFileExist brewPath
          return $ if exists then Just brewPath else Nothing
    else findExecutable "df"

specCases :: [SpecCase]
specCases =
  concat
    [ basicCases,
      helpCases,
      formatCases,
      filterCases,
      errorCases
    ]

basicCases :: [SpecCase]
basicCases =
  [ SpecCase "default output" [] [] CompareStructure,
    SpecCase "current directory" ["."] [] CompareStructure,
    SpecCase "root directory" ["/"] [] CompareStructure
  ]

helpCases :: [SpecCase]
helpCases =
  [ SpecCase "--help shows usage" ["--help"] [] CompareExitOnly,
    SpecCase "--version shows version" ["--version"] [] CompareExitOnly
  ]

formatCases :: [SpecCase]
formatCases =
  [ SpecCase "-P POSIX format" ["-P"] [] CompareStructure,
    SpecCase "-P ignores BLOCK_SIZE" ["-P"] [("BLOCK_SIZE", "1M")] CompareStructure,
    SpecCase "-h human readable" ["-h"] [] CompareStructure,
    SpecCase "-H SI units" ["-H"] [] CompareStructure,
    SpecCase "-k 1K blocks" ["-k"] [] CompareStructure,
    SpecCase "-T show type" ["-T"] [] CompareStructure,
    SpecCase "-i inodes" ["-i"] [] CompareStructure,
    SpecCase "-P -T combined" ["-P", "-T"] [] CompareStructure
  ]

filterCases :: [SpecCase]
filterCases =
  [ SpecCase "-a include pseudo" ["-a"] [] CompareStructure,
    SpecCase "-l local only" ["-l"] [] CompareStructure
  ]

errorCases :: [SpecCase]
errorCases =
  [ SpecCase "nonexistent path" ["/nonexistent/df-test-path-12345"] [] CompareExitOnly,
    SpecCase "-i --output mutual exclusion" ["-i", "--output"] [] CompareExitOnly,
    SpecCase "-P --output mutual exclusion" ["-P", "--output"] [] CompareExitOnly
  ]
