module Cmd.Unlink (run) where

import Control.Exception (IOException, catch)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (removeLink)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "unlink") >> exitSuccess
  [file] -> do
    result <- catch (removeLink file >> return Nothing) handler
    case result of
      Nothing -> return ()
      Just err -> do
        hPutStrLn stderr $ "haskbox unlink: cannot unlink '" ++ file ++ "': " ++ err
        exitFailure
  [] -> do
    hPutStrLn stderr "haskbox unlink: missing operand"
    hPutStrLn stderr "Try 'haskbox unlink --help' for more information."
    exitFailure
  _ -> do
    hPutStrLn stderr "haskbox unlink: extra operand"
    exitFailure
  where
    handler :: IOException -> IO (Maybe String)
    handler e = return $ Just $ friendlyError (show e)

    friendlyError s
      | "does not exist" `isIn` s = "No such file or directory"
      | "Permission denied" `isIn` s = "Permission denied"
      | "is a directory" `isIn` s = "Is a directory"
      | otherwise = s

    isIn needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' str@(_ : xs) = str : tails' xs

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox unlink FILE",
        "Call the unlink function to remove the specified FILE.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
