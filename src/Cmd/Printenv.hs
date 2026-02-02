module Cmd.Printenv (run) where

import System.Environment (getEnvironment, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    putStrLn $ "haskbox printenv: " ++ err
    exitFailure
  Right (opts, vars)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "printenv") >> exitSuccess
    | null vars -> printAllEnv opts
    | otherwise -> printVars opts vars

data Opts = Opts
  { optNullTerminate :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optNullTerminate = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--null" : rest) = parseArgs opts {optNullTerminate = True} rest
parseArgs opts ("-0" : rest) = parseArgs opts {optNullTerminate = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (var : rest) = do
  (o, vars) <- parseArgs opts rest
  Right (o, var : vars)

printAllEnv :: Opts -> IO ()
printAllEnv opts = do
  env <- getEnvironment
  let sep = if optNullTerminate opts then '\0' else '\n'
  mapM_ (\(k, v) -> putStr (k ++ "=" ++ v) >> putChar sep) env

printVars :: Opts -> [String] -> IO ()
printVars opts vars = do
  let sep = if optNullTerminate opts then '\0' else '\n'
  results <- mapM lookupEnv vars
  let found = [v | (_, Just v) <- zip vars results]
  mapM_ (\v -> putStr v >> putChar sep) found
  if length found < length vars
    then exitFailure
    else exitSuccess

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox printenv [OPTION]... [VARIABLE]...",
        "Print the values of the specified environment VARIABLEs.",
        "If no VARIABLE is specified, print name and value pairs for them all.",
        "",
        "  -0, --null     end each output line with NUL, not newline",
        "      --help     display this help and exit",
        "      --version  output version information and exit",
        "",
        "Exit status is 0 if all VARIABLEs are defined, 1 otherwise."
      ]
