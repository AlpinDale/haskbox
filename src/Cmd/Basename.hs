module Cmd.Basename (run) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "basename") >> exitSuccess
  [] -> do
    hPutStrLn stderr "haskbox basename: missing operand"
    hPutStrLn stderr "Try 'haskbox basename --help' for more information."
    exitFailure
  [name] -> putStrLn (basename name)
  [name, suffix] -> putStrLn (removeSuffix suffix (basename name))
  _ -> do
    hPutStrLn stderr "haskbox basename: extra operand"
    exitFailure

basename :: String -> String
basename = reverse . takeWhile (/= '/') . dropTrailingSlashes . reverse
  where
    dropTrailingSlashes = dropWhile (== '/')

removeSuffix :: String -> String -> String
removeSuffix suffix str
  | suffix `isSuffixOf'` str && length str > length suffix =
      take (length str - length suffix) str
  | otherwise = str
  where
    isSuffixOf' s1 s2 = reverse s1 `isPrefixOf'` reverse s2
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox basename NAME [SUFFIX]",
        "Print NAME with any leading directory components removed.",
        "If specified, also remove a trailing SUFFIX.",
        "",
        "      --help     display this help and exit",
        "      --version  output version information and exit"
      ]
