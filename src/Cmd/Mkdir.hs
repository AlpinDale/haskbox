{-# LANGUAGE OverloadedStrings #-}

module Cmd.Mkdir (run) where

import Control.Exception (IOException, catch)
import Data.Bits ((.|.))
import Data.Char (isOctDigit)
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (joinPath, splitDirectories)
import System.IO (hPutStrLn, stderr)
import System.Posix.Directory (createDirectory)
import System.Posix.Files (groupModes, otherExecuteMode, otherReadMode, ownerModes, setFileMode)
import System.Posix.Types (FileMode)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox mkdir: " ++ err
    exitFailure
  Right (opts, dirs)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "mkdir") >> exitSuccess
    | null dirs -> do
        hPutStrLn stderr "haskbox mkdir: missing operand"
        hPutStrLn stderr "Try 'haskbox mkdir --help' for more information."
        exitFailure
    | otherwise -> do
        results <- mapM (makeDir opts) dirs
        if or results then exitFailure else exitSuccess

data Opts = Opts
  { optParents :: !Bool,
    optMode :: !(Maybe FileMode),
    optVerbose :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optParents = False,
      optMode = Nothing,
      optVerbose = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--parents" : rest) = parseArgs opts {optParents = True} rest
parseArgs opts ("--verbose" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts ("-p" : rest) = parseArgs opts {optParents = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optVerbose = True} rest
parseArgs opts (('-' : 'm' : m) : rest)
  | null m = case rest of
      (mode : rest') -> case parseMode mode of
        Just md -> parseArgs opts {optMode = Just md} rest'
        Nothing -> Left $ "invalid mode: '" ++ mode ++ "'"
      [] -> Left "option requires an argument -- 'm'"
  | otherwise = case parseMode m of
      Just md -> parseArgs opts {optMode = Just md} rest
      Nothing -> Left $ "invalid mode: '" ++ m ++ "'"
parseArgs opts ("--mode" : m : rest) = case parseMode m of
  Just md -> parseArgs opts {optMode = Just md} rest
  Nothing -> Left $ "invalid mode: '" ++ m ++ "'"
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs opts (dir : rest) = do
  (o, dirs) <- parseArgs opts rest
  Right (o, dir : dirs)

parseMode :: String -> Maybe FileMode
parseMode s
  | all isOctDigit s && not (null s) = Just $ parseOctal s
  | otherwise = Nothing
  where
    parseOctal = foldl (\acc c -> acc * 8 + fromIntegral (fromEnum c - fromEnum '0')) 0

makeDir :: Opts -> FilePath -> IO Bool
makeDir opts dir
  | optParents opts = makeDirParents opts dir
  | otherwise = makeDirSingle opts dir

makeDirSingle :: Opts -> FilePath -> IO Bool
makeDirSingle opts dir = catch (doCreate >> return False) handler
  where
    mode = fromMaybe defaultMode (optMode opts)
    defaultMode = ownerModes .|. groupModes .|. otherReadMode .|. otherExecuteMode

    doCreate = do
      createDirectory dir mode
      forM_ (optMode opts) (setFileMode dir)
      when (optVerbose opts) $
        putStrLn $
          "haskbox mkdir: created directory '" ++ dir ++ "'"

    handler :: IOException -> IO Bool
    handler e = do
      hPutStrLn stderr $ "haskbox mkdir: cannot create directory '" ++ dir ++ "': " ++ friendlyError (show e)
      return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "already exists" `isIn` s = "File exists"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

makeDirParents :: Opts -> FilePath -> IO Bool
makeDirParents opts dir = go [] components
  where
    components = splitDirectories dir
    mode = fromMaybe defaultMode (optMode opts)
    defaultMode = ownerModes .|. groupModes .|. otherReadMode .|. otherExecuteMode

    go _ [] = return False
    go prefix (c : cs) = do
      let path = joinPath (prefix ++ [c])
      result <- catch (tryCreate path >> return False) handler
      if result
        then return True
        else go (prefix ++ [c]) cs

    tryCreate path = do
      createDirectory path mode
      forM_ (optMode opts) (setFileMode path)
      when (optVerbose opts) $
        putStrLn $
          "haskbox mkdir: created directory '" ++ path ++ "'"

    handler :: IOException -> IO Bool
    handler e
      | "already exists" `isIn` show e = return False -- OK, continue
      | otherwise = do
          hPutStrLn stderr $ "haskbox mkdir: cannot create directory '" ++ dir ++ "': " ++ friendlyError (show e)
          return True

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox mkdir [OPTION]... DIRECTORY...",
        "Create the DIRECTORY(ies), if they do not already exist.",
        "",
        "  -m, --mode=MODE   set file mode (as in chmod), not a=rwx - umask",
        "  -p, --parents     no error if existing, make parent directories as needed",
        "  -v, --verbose     print a message for each created directory",
        "      --help        display this help and exit",
        "      --version     output version information and exit"
      ]
