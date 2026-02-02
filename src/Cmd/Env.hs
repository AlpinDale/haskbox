module Cmd.Env (run) where

import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, waitForProcess)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox env: " ++ err
    exitFailure
  Right (opts, envMods, cmd)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "env") >> exitSuccess
    | optIgnoreEnv opts -> runWithCleanEnv opts envMods cmd
    | otherwise -> runWithMods opts envMods cmd

data Opts = Opts
  { optIgnoreEnv :: !Bool,
    optNullTerminate :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optIgnoreEnv = False,
      optNullTerminate = False,
      optShowHelp = False,
      optShowVersion = False
    }

data EnvMod = SetVar String String | UnsetVar String
  deriving (Show)

parseArgs :: Opts -> [String] -> Either String (Opts, [EnvMod], [String])
parseArgs opts [] = Right (opts, [], [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--ignore-environment" : rest) = parseArgs opts {optIgnoreEnv = True} rest
parseArgs opts ("--null" : rest) = parseArgs opts {optNullTerminate = True} rest
parseArgs opts ("--unset" : var : rest) = do
  (o, mods, cmd) <- parseArgs opts rest
  Right (o, UnsetVar var : mods, cmd)
parseArgs opts (('-' : 'u' : var) : rest)
  | null var = case rest of
      (v : rest') -> do
        (o, mods, cmd) <- parseArgs opts rest'
        Right (o, UnsetVar v : mods, cmd)
      [] -> Left "option requires an argument -- 'u'"
  | otherwise = do
      (o, mods, cmd) <- parseArgs opts rest
      Right (o, UnsetVar var : mods, cmd)
parseArgs opts ("-i" : rest) = parseArgs opts {optIgnoreEnv = True} rest
parseArgs opts ("-0" : rest) = parseArgs opts {optNullTerminate = True} rest
parseArgs opts ("--" : rest) = Right (opts, [], rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (arg : rest)
  | '=' `elem` arg =
      let (name, val') = break (== '=') arg
          val = drop 1 val'
       in do
            (o, mods, cmd) <- parseArgs opts rest
            Right (o, SetVar name val : mods, cmd)
  | otherwise = Right (opts, [], arg : rest)

runWithMods :: Opts -> [EnvMod] -> [String] -> IO ()
runWithMods opts mods cmd = do
  mapM_ applyMod (reverse mods)
  case cmd of
    [] -> printEnv opts
    (prog : args') -> do
      ph <- spawnProcess prog args'
      _ <- waitForProcess ph
      return ()

runWithCleanEnv :: Opts -> [EnvMod] -> [String] -> IO ()
runWithCleanEnv opts mods cmd = do
  env <- getEnvironment
  mapM_ (unsetEnv . fst) env
  mapM_ applyMod (reverse mods)
  case cmd of
    [] -> printEnv opts
    (prog : args') -> do
      ph <- spawnProcess prog args'
      _ <- waitForProcess ph
      return ()

applyMod :: EnvMod -> IO ()
applyMod (SetVar name val) = setEnv name val
applyMod (UnsetVar name) = unsetEnv name

printEnv :: Opts -> IO ()
printEnv opts = do
  env <- getEnvironment
  let sep = if optNullTerminate opts then '\0' else '\n'
  mapM_ (\(k, v) -> putStr (k ++ "=" ++ v) >> putChar sep) env

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox env [OPTION]... [NAME=VALUE]... [COMMAND [ARG]...]",
        "Set each NAME to VALUE in the environment and run COMMAND.",
        "",
        "  -i, --ignore-environment  start with an empty environment",
        "  -0, --null               end each output line with NUL, not newline",
        "  -u, --unset=NAME         remove variable from the environment",
        "      --help               display this help and exit",
        "      --version            output version information and exit",
        "",
        "If no COMMAND, print the environment (after modifications)."
      ]
