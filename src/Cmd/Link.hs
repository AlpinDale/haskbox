module Cmd.Link (run) where

import Control.Exception (IOException, catch)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (createLink)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "link") >> exitSuccess
  [file1, file2] -> do
    result <- catch (createLink file1 file2 >> return Nothing) handler
    case result of
      Nothing -> return ()
      Just err -> do
        hPutStrLn stderr $ "haskbox link: " ++ err
        exitFailure
  [] -> do
    hPutStrLn stderr "haskbox link: missing operand"
    hPutStrLn stderr "Try 'haskbox link --help' for more information."
    exitFailure
  [_] -> do
    hPutStrLn stderr "haskbox link: missing operand after FILE1"
    hPutStrLn stderr "Try 'haskbox link --help' for more information."
    exitFailure
  _ -> do
    hPutStrLn stderr "haskbox link: extra operand"
    exitFailure
  where
    handler :: IOException -> IO (Maybe String)
    handler e = return $ Just $ friendlyError (show e)

    friendlyError s
      | "does not exist" `isIn` s = "cannot create link: No such file or directory"
      | "Permission denied" `isIn` s = "cannot create link: Permission denied"
      | "already exists" `isIn` s = "cannot create link: File exists"
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
      [ "Usage: haskbox link FILE1 FILE2",
        "Call the link function to create a link named FILE2 to FILE1.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
