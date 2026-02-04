{-# LANGUAGE OverloadedStrings #-}

module Test.Cmd.LsSpec (tests) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (isInfixOf)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    removePathForcibly,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Posix.Files
  ( createLink,
    createNamedPipe,
    createSymbolicLink,
    setFileMode,
    setFileTimes,
  )
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

data RunMode
  = RunNonTty
  | RunTty

data SpecCase = SpecCase
  { scName :: String,
    scArgs :: FilePath -> [String],
    scMode :: RunMode,
    scEnv :: [(String, String)]
  }

tests :: TestEnv -> TestTree
tests env =
  withResource (setupLsSpecFixtures env) (\_ -> pure ()) $ \getRoot ->
    withResource findGnuLs (\_ -> pure ()) $ \getGnuLs ->
      withResource findScript (\_ -> pure ()) $ \getScript ->
        testGroup
          "ls-spec"
          (map (mkTest env getRoot getGnuLs getScript) specCases)

mkTest ::
  TestEnv ->
  IO FilePath ->
  IO FilePath ->
  IO (Maybe FilePath) ->
  SpecCase ->
  TestTree
mkTest env getRoot getGnuLs getScript sc =
  testCase (scName sc) $ do
    root <- getRoot
    gnuLs <- getGnuLs
    let args = scArgs sc root
        envVars = baseEnv ++ scEnv sc
        gnuPrefix = if takeFileName gnuLs == "gls" then "g" else ""

    case scMode sc of
      RunNonTty -> do
        (rcRef, outRef, errRef) <- runCmdWithEnv gnuLs args envVars
        (rcTest, outTest, errTest) <- runCmdWithEnv (envHaskbox env) ("ls" : args) envVars
        let errRefNorm = normalizeErr "ls" gnuPrefix errRef
            errTestNorm = normalizeErr "ls" gnuPrefix errTest
        rcRef @?= rcTest
        outRef @=? outTest
        errRefNorm @=? errTestNorm
      RunTty -> do
        mScript <- getScript
        case mScript of
          Nothing -> pure ()
          Just scriptPath -> do
            (rcRef, outRef, errRef) <- runCmdTtyWithEnv scriptPath gnuLs args envVars
            (rcTest, outTest, errTest) <- runCmdTtyWithEnv scriptPath (envHaskbox env) ("ls" : args) envVars
            let errRefNorm = normalizeErr "ls" gnuPrefix errRef
                errTestNorm = normalizeErr "ls" gnuPrefix errTest
            rcRef @?= rcTest
            outRef @=? outTest
            errRefNorm @=? errTestNorm

baseEnv :: [(String, String)]
baseEnv =
  [ ("LC_ALL", "C"),
    ("LANG", "C"),
    ("TZ", "UTC"),
    ("COLUMNS", "80"),
    ("TERM", "xterm-256color"),
    ("LS_COLORS", "di=01;34"),
    ("QUOTING_STYLE", "literal")
  ]

specCases :: [SpecCase]
specCases =
  concat
    [ listingCases,
      infoCases,
      sortingCases,
      formatCases,
      quotingCases,
      colorCases,
      indicatorCases,
      hyperlinkCases,
      diredCases,
      exitStatusCases
    ]

caseNonTty :: String -> (FilePath -> [String]) -> SpecCase
caseNonTty name args = SpecCase name args RunNonTty []

caseTty :: String -> (FilePath -> [String]) -> SpecCase
caseTty name args = SpecCase name args RunTty []

listingCases :: [SpecCase]
listingCases =
  [ caseNonTty "default listing" (: []),
    caseNonTty "-a shows dotfiles" (\root -> ["-a", root]),
    caseNonTty "-A shows dotfiles except . and .." (\root -> ["-A", root]),
    caseNonTty "-B ignores backups" (\root -> ["-B", root]),
    caseNonTty "--ignore pattern" (\root -> ["--ignore=ignored*", root]),
    caseNonTty "--hide pattern" (\root -> ["--hide=hidden*", root]),
    caseNonTty "--hide with -A shows hidden" (\root -> ["-A", "--hide=hidden*", root]),
    caseNonTty "-d lists directories themselves" (\root -> ["-d", root </> "dir"]),
    caseNonTty "-R recursive listing" (\root -> ["-R", root]),
    caseNonTty "-H dereference cmdline symlink" (\root -> ["-H", root </> "link_dir"]),
    caseNonTty "--dereference-command-line-symlink-to-dir" (\root -> ["--dereference-command-line-symlink-to-dir", root </> "link_dir"]),
    caseNonTty "-L dereference all symlinks" (\root -> ["-L", root </> "link_dir"])
  ]

infoCases :: [SpecCase]
infoCases =
  [ caseNonTty "-l long format" (\root -> ["-l", root]),
    caseNonTty "-n numeric uid/gid" (\root -> ["-n", root]),
    caseNonTty "-g hide owner" (\root -> ["-g", root]),
    caseNonTty "-o hide group" (\root -> ["-o", root]),
    caseNonTty "--author" (\root -> ["--author", "-l", root]),
    caseNonTty "-i (non-long)" (\root -> ["-i", root]),
    caseNonTty "-s (non-long)" (\root -> ["-s", root]),
    caseNonTty "--block-size=1" (\root -> ["--block-size=1", "-l", root]),
    caseNonTty "-k" (\root -> ["-k", "-l", root]),
    caseNonTty "-h" (\root -> ["-h", "-l", root]),
    caseNonTty "--si" (\root -> ["--si", "-l", root]),
    caseNonTty "--context" (\root -> ["--context", "-l", root]),
    caseNonTty "--full-time" (\root -> ["--full-time", root]),
    caseNonTty "--time-style=full-iso" (\root -> ["--time-style=full-iso", "-l", root]),
    caseNonTty "--time-style=long-iso" (\root -> ["--time-style=long-iso", "-l", root]),
    caseNonTty "--time-style=iso" (\root -> ["--time-style=iso", "-l", root]),
    caseNonTty "--time-style=+FORMAT" (\root -> ["--time-style=+%Y-%m-%d", "-l", root])
  ]

sortingCases :: [SpecCase]
sortingCases =
  [ caseNonTty "--sort=name" (\root -> ["--sort=name", "-1", root]),
    caseNonTty "-S sort by size" (\root -> ["-1S", root]),
    caseNonTty "-t sort by time" (\root -> ["-1t", root]),
    caseNonTty "-v version sort" (\root -> ["-1v", root]),
    caseNonTty "-X extension sort" (\root -> ["-1X", root]),
    caseNonTty "--sort=width" (\root -> ["--sort=width", "-1", root]),
    caseNonTty "-U sort=none" (\root -> ["-1U", root]),
    caseNonTty "-f implies -a -U" (\root -> ["-f", root]),
    caseNonTty "-r reverse" (\root -> ["-1r", root]),
    caseNonTty "-c ctime" (\root -> ["-1c", root]),
    caseNonTty "-u atime" (\root -> ["-1u", root]),
    caseNonTty "--time=birth" (\root -> ["--time=birth", "-l", root]),
    caseNonTty "--group-directories-first" (\root -> ["--group-directories-first", "-1", root]),
    caseNonTty "--group-directories-first with --sort=none" (\root -> ["--group-directories-first", "--sort=none", "-1", root])
  ]

formatCases :: [SpecCase]
formatCases =
  [ caseNonTty "-1 single column" (\root -> ["-1", root]),
    caseNonTty "-C columns" (\root -> ["-C", root]),
    caseNonTty "-x across" (\root -> ["-x", root]),
    caseNonTty "-m commas" (\root -> ["-m", root]),
    caseNonTty "--format=long" (\root -> ["--format=long", root]),
    caseNonTty "--format=single-column" (\root -> ["--format=single-column", root]),
    caseNonTty "--format=commas" (\root -> ["--format=commas", root]),
    caseNonTty "--format=across" (\root -> ["--format=across", root]),
    caseNonTty "--zero" (\root -> ["--zero", root]),
    caseNonTty "--tabsize=4" (\root -> ["--tabsize=4", "-C", root]),
    caseTty "TTY default columns" (\root -> ["-w", "80", root])
  ]

quotingCases :: [SpecCase]
quotingCases =
  [ caseNonTty "-b escape" (\root -> ["-b", root]),
    caseNonTty "-q hide control chars" (\root -> ["-q", root]),
    caseNonTty "--show-control-chars" (\root -> ["--show-control-chars", root]),
    caseNonTty "-Q quote name" (\root -> ["-Q", root]),
    caseNonTty "-N literal" (\root -> ["-N", root]),
    caseNonTty "--literal" (\root -> ["--literal", root]),
    caseNonTty "--quoting-style=shell" (\root -> ["--quoting-style=shell", root]),
    caseNonTty "--quoting-style=shell-always" (\root -> ["--quoting-style=shell-always", root]),
    caseNonTty "--quoting-style=shell-escape" (\root -> ["--quoting-style=shell-escape", root]),
    caseNonTty "--quoting-style=escape" (\root -> ["--quoting-style=escape", root]),
    caseNonTty "--quoting-style=locale" (\root -> ["--quoting-style=locale", root])
  ]

colorCases :: [SpecCase]
colorCases =
  [ caseNonTty "--color=always" (\root -> ["--color=always", root]),
    caseNonTty "--color=never" (\root -> ["--color=never", root]),
    caseNonTty "--color=auto (non-tty)" (\root -> ["--color=auto", root]),
    caseTty "--color=auto (tty)" (\root -> ["--color=auto", "-w", "80", root])
  ]

indicatorCases :: [SpecCase]
indicatorCases =
  [ caseNonTty "-F classify" (\root -> ["-F", root]),
    caseNonTty "--file-type" (\root -> ["--file-type", root]),
    caseNonTty "-p slash" (\root -> ["-p", root]),
    caseNonTty "--indicator-style=classify" (\root -> ["--indicator-style=classify", root]),
    caseNonTty "--indicator-style=file-type" (\root -> ["--indicator-style=file-type", root]),
    caseNonTty "--indicator-style=slash" (\root -> ["--indicator-style=slash", root]),
    caseNonTty "--classify=auto (non-tty)" (\root -> ["--classify=auto", root]),
    caseTty "--classify=auto (tty)" (\root -> ["--classify=auto", "-w", "80", root])
  ]

hyperlinkCases :: [SpecCase]
hyperlinkCases =
  [ caseNonTty "--hyperlink=always" (\root -> ["--hyperlink=always", root]),
    caseNonTty "--hyperlink=auto (non-tty)" (\root -> ["--hyperlink=auto", root]),
    caseTty "--hyperlink=auto (tty)" (\root -> ["--hyperlink=auto", "-w", "80", root])
  ]

diredCases :: [SpecCase]
diredCases =
  [ caseNonTty "--dired" (\root -> ["--dired", root]),
    caseNonTty "--dired -R" (\root -> ["--dired", "-R", root])
  ]

exitStatusCases :: [SpecCase]
exitStatusCases =
  [ caseNonTty "nonexistent path exit status" (const ["/nonexistent/path/ls-spec-test-12345"])
  ]

findGnuLs :: IO FilePath
findGnuLs = do
  isMac <- doesFileExist "/usr/bin/sw_vers"
  if isMac
    then do
      mGls <- findExecutable "gls"
      case mGls of
        Just p -> return p
        Nothing -> fail "GNU ls (gls) not found. Install coreutils and ensure gls is on PATH."
    else do
      mGls <- findExecutable "gls"
      case mGls of
        Just p -> return p
        Nothing -> do
          mLs <- findExecutable "ls"
          case mLs of
            Nothing -> fail "ls not found on PATH."
            Just p -> do
              isGnu <- isGnuLs p
              if isGnu
                then return p
                else fail "Non-GNU ls found. Install coreutils or ensure GNU ls is first on PATH."

isGnuLs :: FilePath -> IO Bool
isGnuLs path = do
  (rc, out, _) <- readProcessWithExitCode path ["--version"] ""
  return $ rc == ExitSuccess && "GNU coreutils" `isInfixOf` out

setupLsSpecFixtures :: TestEnv -> IO FilePath
setupLsSpecFixtures env = do
  let root = envTmpDir env </> "ls_spec"
  exists <- doesDirectoryExist root
  when exists $ removePathForcibly root
  createDirectory root

  let writeFileBS = BS.writeFile
      ignoreIO action = action `catch` (\(_ :: IOException) -> pure ())
      nameTab = "tab" ++ [chr 9] ++ "name"
      nameNl = "line" ++ [chr 10] ++ "break"
      nameCtrl = "ctrl" ++ [chr 1] ++ "x"

  writeFileBS (root </> "file") "a"
  writeFileBS (root </> "file2") "bb"
  writeFileBS (root </> "file10") "cccccccccc"
  writeFileBS (root </> "noext") "x"
  writeFileBS (root </> "a.c") "x"
  writeFileBS (root </> "b.h") "x"
  writeFileBS (root </> "c.txt") "x"
  writeFileBS (root </> "ignored.tmp") "x"
  writeFileBS (root </> "hidden_file") "x"
  writeFileBS (root </> ".hidden") "x"
  writeFileBS (root </> ".h2") "x"
  writeFileBS (root </> "backup~") "x"
  writeFileBS (root </> ".hidden~") "x"
  writeFileBS (root </> "space name") "x"
  writeFileBS (root </> nameTab) "x"
  writeFileBS (root </> nameNl) "x"
  writeFileBS (root </> "single'quote") "x"
  writeFileBS (root </> "double\"quote") "x"
  writeFileBS (root </> nameCtrl) "x"

  createDirectory (root </> "dir")
  createDirectory (root </> "dir2")
  createDirectoryIfMissing True (root </> "dir" </> "subdir")
  writeFileBS (root </> "dir" </> "subfile") "x"

  ignoreIO $ createSymbolicLink "file" (root </> "link_file")
  ignoreIO $ createSymbolicLink "dir" (root </> "link_dir")
  ignoreIO $ createSymbolicLink "missing" (root </> "broken_link")
  ignoreIO $ createLink (root </> "file") (root </> "file_hardlink")
  ignoreIO $ createNamedPipe (root </> "fifo") 0o644

  writeFileBS (root </> "exec.sh") "#!/bin/sh\necho hi\n"
  ignoreIO $ setFileMode (root </> "exec.sh") 0o755
  writeFileBS (root </> "setuid") "x"
  ignoreIO $ setFileMode (root </> "setuid") 0o4755
  createDirectory (root </> "sticky")
  ignoreIO $ setFileMode (root </> "sticky") 0o1777

  now <- epochTime
  let oldTime = now - seconds (86400 * 400)
  writeFileBS (root </> "mtime_old") "x"
  ignoreIO $ setFileTimes (root </> "mtime_old") now oldTime
  writeFileBS (root </> "atime_old") "x"
  ignoreIO $ setFileTimes (root </> "atime_old") oldTime now

  return root

seconds :: Int -> EpochTime
seconds = fromIntegral
