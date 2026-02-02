module Cmd.Dirname (run) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "dirname") >> exitSuccess
  [] -> do
    hPutStrLn stderr "haskbox dirname: missing operand"
    hPutStrLn stderr "Try 'haskbox dirname --help' for more information."
    exitFailure
  paths -> mapM_ (putStrLn . dirname) paths

dirname :: String -> String
dirname path =
  let stripped = dropTrailingSlashes path
      result = reverse $ dropWhile (/= '/') $ reverse stripped
   in case result of
        "" -> "."
        "/" -> "/"
        _ -> dropTrailingSlashes result
  where
    dropTrailingSlashes "" = ""
    dropTrailingSlashes "/" = "/"
    dropTrailingSlashes s = reverse $ dropWhile (== '/') $ reverse s

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox dirname NAME...",
        "Output each NAME with its last non-slash component and trailing slashes removed;",
        "if NAME contains no /'s, output '.' (meaning the current directory).",
        "",
        "      --help     display this help and exit",
        "      --version  output version information and exit"
      ]
