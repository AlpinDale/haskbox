{-# LANGUAGE OverloadedStrings #-}

module Cmd.Timeout (run) where

import Control.Concurrent (threadDelay)
import Data.Char (isDigit)
import Data.Foldable (forM_)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Process (createProcess, getPid, getProcessExitCode, proc, waitForProcess)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox timeout: " ++ err
    exitFailure
  Right (opts, rest)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "timeout") >> exitSuccess
    | null rest -> do
        hPutStrLn stderr "haskbox timeout: missing operand"
        hPutStrLn stderr "Try 'haskbox timeout --help' for more information."
        exitFailure
    | length rest < 2 -> do
        hPutStrLn stderr "haskbox timeout: missing operand"
        hPutStrLn stderr "Try 'haskbox timeout --help' for more information."
        exitFailure
    | otherwise -> case rest of
        (durationStr : cmd : cmdArgs) -> case parseDuration durationStr of
          Nothing -> do
            hPutStrLn stderr $ "haskbox timeout: invalid time interval '" ++ durationStr ++ "'"
            exitFailure
          Just duration -> runWithTimeout opts duration cmd cmdArgs
        _ -> do
          hPutStrLn stderr "haskbox timeout: missing operand"
          hPutStrLn stderr "Try 'haskbox timeout --help' for more information."
          exitFailure

data Opts = Opts
  { optForeground :: !Bool,
    optKillAfter :: !(Maybe Int),
    optSignal :: !String,
    optPreserveStatus :: !Bool,
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optForeground = False,
      optKillAfter = Nothing,
      optSignal = "TERM",
      optPreserveStatus = False,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--foreground" : rest) = parseArgs opts {optForeground = True} rest
parseArgs opts ("--preserve-status" : rest) = parseArgs opts {optPreserveStatus = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts (('-' : 'k' : k) : rest)
  | null k = case rest of
      (dur : rest') -> case parseDuration dur of
        Just d -> parseArgs opts {optKillAfter = Just d} rest'
        Nothing -> Left $ "invalid time interval: '" ++ dur ++ "'"
      [] -> Left "option requires an argument -- 'k'"
  | otherwise = case parseDuration k of
      Just d -> parseArgs opts {optKillAfter = Just d} rest
      Nothing -> Left $ "invalid time interval: '" ++ k ++ "'"
parseArgs opts (('-' : 's' : s) : rest)
  | null s = case rest of
      (sig : rest') -> parseArgs opts {optSignal = sig} rest'
      [] -> Left "option requires an argument -- 's'"
  | otherwise = parseArgs opts {optSignal = s} rest
parseArgs opts ("--kill-after" : k : rest) = case parseDuration k of
  Just d -> parseArgs opts {optKillAfter = Just d} rest
  Nothing -> Left $ "invalid time interval: '" ++ k ++ "'"
parseArgs opts ("--signal" : s : rest) = parseArgs opts {optSignal = s} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (arg : rest) = do
  (o, args') <- parseArgs opts rest
  Right (o, arg : args')

parseDuration :: String -> Maybe Int
parseDuration s =
  let (numPart, suffix) = span (\c -> isDigit c || c == '.') s
   in case reads numPart :: [(Double, String)] of
        [(n, "")] -> Just $ round (n * multiplier suffix)
        _ -> Nothing
  where
    multiplier "" = 1000000 -- Default: seconds to microseconds
    multiplier "s" = 1000000
    multiplier "m" = 60000000
    multiplier "h" = 3600000000
    multiplier "d" = 86400000000
    multiplier _ = 1000000

runWithTimeout :: Opts -> Int -> String -> [String] -> IO ()
runWithTimeout opts duration cmd cmdArgs = do
  -- Start the process
  (_, _, _, ph) <- createProcess (proc cmd cmdArgs)

  -- Poll until timeout or process exits
  let pollInterval = 10000 -- 10ms
      loop remaining
        | remaining <= 0 = do
            -- Timeout! Kill the process
            when (optVerbose opts) $
              hPutStrLn stderr $
                "haskbox timeout: sending signal to command '" ++ cmd ++ "'"
            mbPid <- getPid ph
            forM_ mbPid (signalProcess sigTERM)
            -- Wait for process to die
            _ <- waitForProcess ph
            -- Handle kill-after if needed
            case optKillAfter opts of
              Just killDelay -> do
                threadDelay killDelay
                mbPid2 <- getPid ph
                forM_ mbPid2 (signalProcess sigKILL)
              Nothing -> return ()
            exitWith (ExitFailure 124)
        | otherwise = do
            mbCode <- getProcessExitCode ph
            case mbCode of
              Just exitCode -> exitWith exitCode
              Nothing -> do
                threadDelay pollInterval
                loop (remaining - pollInterval)

  loop duration

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox timeout [OPTION] DURATION COMMAND [ARG]...",
        "Start COMMAND, and kill it if still running after DURATION.",
        "",
        "  --foreground         when not running timeout directly from a shell prompt,",
        "                         allow COMMAND to read from the TTY and get TTY signals",
        "  -k, --kill-after=DURATION",
        "                       also send a KILL signal if COMMAND is still running",
        "                         this long after the initial signal was sent",
        "  -s, --signal=SIGNAL  specify the signal to be sent on timeout",
        "      --preserve-status",
        "                       exit with the same status as COMMAND, even when it times out",
        "  -v, --verbose        diagnose to stderr any signal sent upon timeout",
        "      --help           display this help and exit",
        "      --version        output version information and exit",
        "",
        "DURATION is a number with optional suffix:",
        "  s  seconds (default)",
        "  m  minutes",
        "  h  hours",
        "  d  days",
        "",
        "Exit status:",
        "  124  if COMMAND times out",
        "  125  if timeout itself fails",
        "  126  if COMMAND is found but cannot be invoked",
        "  127  if COMMAND cannot be found",
        "  otherwise, the exit status of COMMAND"
      ]
