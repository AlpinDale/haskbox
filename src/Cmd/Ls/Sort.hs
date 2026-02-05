module Cmd.Ls.Sort
  ( sortFiles,
    versionCompare,
  )
where

import Cmd.Ls.Format (displayNameWidth)
import Cmd.Ls.Types
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import System.FilePath (takeExtension)

sortFiles :: Opts -> [FileInfo] -> [FileInfo]
sortFiles opts files
  | optGroupDirsFirst opts && optSortMode opts /= SortNone =
      let (dirs, nonDirs) = partition isDir files
       in applySort opts dirs ++ applySort opts nonDirs
  | otherwise = applySort opts files
  where
    isDir fi = fiType fi == TypeDirectory || (fiType fi == TypeSymlink && fiLinkTargetIsDir fi)
    partition p xs = (filter p xs, filter (not . p) xs)

applySort :: Opts -> [FileInfo] -> [FileInfo]
applySort opts
  | optSortMode opts == SortNone = id
  | optReverse opts = sortBy (flip cmp)
  | otherwise = sortBy cmp
  where
    cmp = case optSortMode opts of
      SortByName -> comparingName
      SortBySize -> comparing (Down . fiSize) <> comparingName
      SortByTime -> comparingTime opts <> comparingName
      SortByVersion -> \a b -> versionCompare (fiName a) (fiName b)
      SortByExtension -> comparingExt <> comparingName
      SortByWidth -> comparing (displayNameWidth opts) <> comparingName
      SortNone -> \_ _ -> EQ

    -- Use byte-order comparison (like strcmp), matching GNU ls in C/POSIX locale
    comparingName :: FileInfo -> FileInfo -> Ordering
    comparingName a b = compare (fiName a) (fiName b)

    -- Extension comparison uses byte-order (like strcmp)
    comparingExt :: FileInfo -> FileInfo -> Ordering
    comparingExt a b = compare (getExt (fiName a)) (getExt (fiName b))

    getExt :: String -> String
    getExt name = case takeExtension name of
      "" -> ""
      ('.' : ext) -> ext
      ext -> ext

comparingTime :: Opts -> FileInfo -> FileInfo -> Ordering
comparingTime opts = comparing (Down . getTime)
  where
    getTime fi = case optTimeMode opts of
      TimeMod -> fiModTime fi
      TimeAccess -> fiAccessTime fi
      TimeChange -> fiChangeTime fi
      TimeBirth -> case fiBirthTime fi of
        Just t -> t
        Nothing -> fiModTime fi

-- | Version-sort comparison (like GNU filevercmp)
-- Handles embedded numbers naturally: file1 < file2 < file10
-- Uses byte-order comparison for non-digit characters (locale-independent)
versionCompare :: String -> String -> Ordering
versionCompare [] [] = EQ
versionCompare [] _ = LT
versionCompare _ [] = GT
versionCompare s1@(c1 : _) s2@(c2 : _)
  | isDigit c1 && isDigit c2 =
      let (n1, r1) = spanDigits s1
          (n2, r2) = spanDigits s2
          -- Compare numbers by value, not lexicographically
          numCmp = compareNums n1 n2
       in if numCmp /= EQ then numCmp else versionCompare r1 r2
  | otherwise =
      -- Byte-order comparison (like strcmp), locale-independent
      case compare c1 c2 of
        EQ -> versionCompare (drop 1 s1) (drop 1 s2)
        other -> other
  where
    spanDigits = span isDigit

    -- Compare numeric strings, handling leading zeros
    compareNums :: String -> String -> Ordering
    compareNums a b =
      let a' = dropWhile (== '0') a
          b' = dropWhile (== '0') b
       in case compare (length a') (length b') of
            EQ -> compare a' b'
            other -> other
