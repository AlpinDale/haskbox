{-# LANGUAGE BangPatterns #-}

module Cmd.Factor (run) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, isEOF, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "factor") >> exitSuccess
  [] -> factorStdin
  nums -> mapM_ factorArg nums

factorStdin :: IO ()
factorStdin = do
  eof <- isEOF
  if eof
    then return ()
    else do
      line <- getLine
      mapM_ factorArg (words line)
      factorStdin

factorArg :: String -> IO ()
factorArg s = case readInteger s of
  Just n
    | n < 0 -> do
        hPutStrLn stderr $ "haskbox factor: '" ++ s ++ "' is not a valid positive integer"
        exitFailure
    | otherwise -> do
        let factors = primeFactors n
        putStrLn $ s ++ ": " ++ unwords (map show factors)
  Nothing -> do
    hPutStrLn stderr $ "haskbox factor: '" ++ s ++ "' is not a valid positive integer"
    exitFailure

readInteger :: String -> Maybe Integer
readInteger s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1 = []
  | otherwise = go n 2
  where
    go !m !p
      | p * p > m = [m | m > 1]
      | m `mod` p == 0 = p : go (m `div` p) p
      | p == 2 = go m 3
      | otherwise = go m (p + 2)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox factor [NUMBER]...",
        "Print the prime factors of each specified integer NUMBER.",
        "",
        "If no NUMBER is specified, read from standard input.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
