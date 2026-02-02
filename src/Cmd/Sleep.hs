module Cmd.Sleep (run) where

import Control.Concurrent (threadDelay)
import Data.Char (isDigit)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "sleep") >> exitSuccess
  [] -> do
    hPutStrLn stderr "haskbox sleep: missing operand"
    hPutStrLn stderr "Try 'haskbox sleep --help' for more information."
    exitFailure
  _ -> do
    let total = sum <$> traverse parseDuration args
    case total of
      Left err -> do
        hPutStrLn stderr $ "haskbox sleep: " ++ err
        exitFailure
      Right secs -> do
        let micros = round (secs * 1000000) :: Int
        threadDelay micros

parseDuration :: String -> Either String Double
parseDuration s =
  let (numPart, suffix) = span (\c -> isDigit c || c == '.') s
   in case suffix of
        "" -> parseNum numPart
        "s" -> parseNum numPart
        "m" -> (* 60) <$> parseNum numPart
        "h" -> (* 3600) <$> parseNum numPart
        "d" -> (* 86400) <$> parseNum numPart
        _ -> Left $ "invalid time interval '" ++ s ++ "'"
  where
    parseNum n
      | null n = Left $ "invalid time interval '" ++ s ++ "'"
      | all (\c -> isDigit c || c == '.') n =
          let dots = filter (== '.') n
           in if length dots <= 1
                then Right (read n :: Double)
                else Left $ "invalid time interval '" ++ s ++ "'"
      | otherwise = Left $ "invalid time interval '" ++ s ++ "'"

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox sleep NUMBER[SUFFIX]...",
        "Pause for NUMBER seconds. SUFFIX may be 's' for seconds (default),",
        "'m' for minutes, 'h' for hours, or 'd' for days.",
        "NUMBER may be a floating point number.",
        "",
        "Given two or more arguments, pause for the sum of their values.",
        "",
        "      --help     display this help and exit",
        "      --version  output version information and exit"
      ]
