{-# LANGUAGE OverloadedStrings #-}

module Cmd.Tsort (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.List (nub)
import Data.Map.Strict qualified as Map
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "tsort") >> exitSuccess
  [] -> tsortFile "-"
  ["-"] -> tsortFile "-"
  [file] -> tsortFile file
  _ -> do
    hPutStrLn stderr "haskbox tsort: extra operand"
    exitFailure

tsortFile :: FilePath -> IO ()
tsortFile path = catch doTsort handler
  where
    doTsort = do
      contents <-
        if path == "-"
          then BS.hGetContents stdin
          else BS.readFile path

      let tokens = words $ C8.unpack contents
          pairs = makePairs tokens
          nodes = nub $ concatMap (\(a, b) -> [a, b]) pairs
          result = topologicalSort nodes pairs

      case result of
        Left [] -> do
          hPutStrLn stderr "haskbox tsort: cycle detected"
          exitFailure
        Left (firstNode : _) -> do
          hPutStrLn stderr $ "haskbox tsort: cycle detected: " ++ firstNode
          exitFailure
        Right sorted -> mapM_ putStrLn sorted

    handler :: IOException -> IO ()
    handler e = do
      hPutStrLn stderr $ "haskbox tsort: " ++ path ++ ": " ++ friendlyError (show e)
      exitFailure

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

makePairs :: [String] -> [(String, String)]
makePairs [] = []
makePairs [_] = []
makePairs (a : b : rest) = (a, b) : makePairs rest

-- | Kahn's algorithm for topological sort
topologicalSort :: [String] -> [(String, String)] -> Either [String] [String]
topologicalSort nodes edges =
  let -- Build adjacency list and in-degree count
      adjList = Map.fromListWith (++) [(from, [to]) | (from, to) <- edges, from /= to]
      inDegree :: Map.Map String Int
      inDegree =
        Map.fromListWith (+) $
          [(n, 0 :: Int) | n <- nodes]
            ++ [(to, 1) | (from, to) <- edges, from /= to]

      -- Find nodes with no incoming edges
      noIncoming = [n | n <- nodes, Map.findWithDefault 0 n inDegree == 0]

      -- Process nodes
      go [] res _ _ = Right (reverse res)
      go (n : queue) res adj inDeg =
        let successors = Map.findWithDefault [] n adj
            (newQueue, newInDeg) = foldl updateInDegree (queue, inDeg) successors
         in go newQueue (n : res) adj newInDeg

      updateInDegree (q, deg) node =
        let newDeg = Map.adjust (subtract 1) node deg
            newDegVal = Map.findWithDefault 0 node newDeg
         in if newDegVal == 0
              then (q ++ [node], newDeg)
              else (q, newDeg)

      result = go noIncoming [] adjList inDegree
   in case result of
        Right sorted | length sorted == length (nub nodes) -> Right sorted
        Right sorted ->
          -- Cycle detected: find a node that wasn't output
          let remaining = filter (`notElem` sorted) nodes
           in Left remaining
        Left err -> Left err

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox tsort [OPTION] [FILE]",
        "Write totally ordered list consistent with the partial ordering in FILE.",
        "",
        "With no FILE, or when FILE is -, read standard input.",
        "",
        "The input is a list of pairs of whitespace-separated strings.",
        "Each pair represents a partial ordering where the first string",
        "must come before the second.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
