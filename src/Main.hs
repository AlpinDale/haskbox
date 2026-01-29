module Main where

import Cat
import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      prog <- getProgName
      hPutStrLn stderr $ prog ++ ": " ++ err
      exitFailure
    Right (opts, files)
      | showVersion opts -> do
          putStrLn "haskat 0.1.0"
          exitSuccess
      | showHelp opts -> do
          printHelp
          exitSuccess
      | otherwise -> do
          let paths = if null files then ["-"] else files
          hasError <- catFilesWithErrors (catOpts opts) paths
          when hasError exitFailure

data Args = Args
  { showHelp :: !Bool,
    showVersion :: !Bool,
    catOpts :: !CatOptions,
    argFiles :: ![FilePath]
  }

defaultArgs :: Args
defaultArgs =
  Args
    { showHelp = False,
      showVersion = False,
      catOpts = defaultOptions,
      argFiles = []
    }

parseArgs :: [String] -> Either String (Args, [FilePath])
parseArgs = go defaultArgs
  where
    go args [] = Right (args, reverse $ argFiles args)
    go args ("--" : rest) = Right (args {argFiles = reverse rest ++ argFiles args}, [])
    go args ("--help" : rest) = go args {showHelp = True} rest
    go args ("--version" : rest) = go args {showVersion = True} rest
    go args ("--show-all" : rest) = go (setA args) rest
    go args ("--number-nonblank" : rest) = go (setB args) rest
    go args ("--show-ends" : rest) = go (setE args) rest
    go args ("--number" : rest) = go (setN args) rest
    go args ("--squeeze-blank" : rest) = go (setS args) rest
    go args ("--show-tabs" : rest) = go (setT args) rest
    go args ("--show-nonprinting" : rest) = go (setV args) rest
    go _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
    go args (('-' : shorts) : rest)
      | null shorts = go args {argFiles = "-" : argFiles args} rest
      | otherwise = case parseShorts args shorts of
          Left e -> Left e
          Right args' -> go args' rest
    go args (file : rest) = go args {argFiles = file : argFiles args} rest

    parseShorts args [] = Right args
    parseShorts args (c : cs) = case c of
      'A' -> parseShorts (setA args) cs
      'b' -> parseShorts (setB args) cs
      'e' -> parseShorts (setVE args) cs
      'E' -> parseShorts (setE args) cs
      'n' -> parseShorts (setN args) cs
      's' -> parseShorts (setS args) cs
      't' -> parseShorts (setVT args) cs
      'T' -> parseShorts (setT args) cs
      'u' -> parseShorts args cs
      'v' -> parseShorts (setV args) cs
      'h' -> parseShorts args {showHelp = True} cs
      _ -> Left $ "invalid option -- '" ++ [c] ++ "'"

    setA a = a {catOpts = (catOpts a) {optShowEnds = True, optShowTabs = True, optShowNonprint = True}}
    setB a = a {catOpts = (catOpts a) {optNumberNonblank = True}}
    setE a = a {catOpts = (catOpts a) {optShowEnds = True}}
    setN a = a {catOpts = (catOpts a) {optNumber = True}}
    setS a = a {catOpts = (catOpts a) {optSqueezeBlank = True}}
    setT a = a {catOpts = (catOpts a) {optShowTabs = True}}
    setV a = a {catOpts = (catOpts a) {optShowNonprint = True}}
    setVE a = setV (setE a)
    setVT a = setV (setT a)

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskat [OPTION]... [FILE]...",
        "Concatenate FILE(s) to standard output.",
        "",
        "  -A, --show-all           equivalent to -vET",
        "  -b, --number-nonblank    number nonempty output lines",
        "  -e                       equivalent to -vE",
        "  -E, --show-ends          display $ at end of each line",
        "  -n, --number             number all output lines",
        "  -s, --squeeze-blank      suppress repeated empty output lines",
        "  -t                       equivalent to -vT",
        "  -T, --show-tabs          display TAB characters as ^I",
        "  -u                       (ignored)",
        "  -v, --show-nonprinting   use ^ and M- notation",
        "      --help               display this help and exit",
        "      --version            output version information and exit",
        "",
        "With no FILE, or when FILE is -, read standard input."
      ]
