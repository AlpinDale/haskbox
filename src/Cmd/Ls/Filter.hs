module Cmd.Ls.Filter
  ( shouldShow,
    matchPattern,
    isHidden,
    isBackup,
  )
where

import Cmd.Ls.Types

shouldShow :: Opts -> String -> Bool
shouldShow opts name
  -- Always show . and .. if -a is set
  | optAll opts = not ignored
  -- Show hidden files (except . and ..) if -A is set
  | optAlmostAll opts = not (isDotOrDotDot name) && not ignored && not hidden
  -- Default: hide hidden files and apply filters
  | otherwise = not (isHidden name) && not ignored && not hidden
  where
    ignored = matchesAny (optIgnorePatterns opts) name || isBackupFile
    hidden = not (optAll opts || optAlmostAll opts) && matchesAny (optHidePatterns opts) name
    isBackupFile = optIgnoreBackups opts && isBackup name

-- | Check if name is "." or ".."
isDotOrDotDot :: String -> Bool
isDotOrDotDot "." = True
isDotOrDotDot ".." = True
isDotOrDotDot _ = False

-- | Check if name starts with "." (hidden file)
isHidden :: String -> Bool
isHidden ('.' : _) = True
isHidden _ = False

-- | Check if name ends with "~" (backup file)
isBackup :: String -> Bool
isBackup [] = False
isBackup s = last s == '~'

-- | Check if name matches any of the patterns
matchesAny :: [String] -> String -> Bool
matchesAny patterns name = any (`matchPattern` name) patterns

-- | Match a glob pattern against a filename
-- Supports *, ?, and [chars] patterns
matchPattern :: String -> String -> Bool
matchPattern pat name =
  -- Leading dot must be matched explicitly (like shell glob rules)
  not (isHidden name && not (patternCanMatchDot pat))
    && matchPattern' pat name

matchPattern' :: String -> String -> Bool
matchPattern' [] [] = True
matchPattern' [] _ = False
matchPattern' ('*' : ps) s = matchStar ps s
matchPattern' ('?' : ps) (_ : cs) = matchPattern' ps cs
matchPattern' ('?' : _) [] = False
matchPattern' ('[' : ps) (c : cs) = case parseCharClass ps of
  Just (chars, negated, rest) ->
    let matches = c `elem` chars
     in (if negated then not matches else matches) && matchPattern' rest cs
  Nothing -> '[' == c && matchPattern' ps cs
matchPattern' ('[' : _) [] = False
matchPattern' ('\\' : p : ps) (c : cs) = p == c && matchPattern' ps cs
matchPattern' ('\\' : _) _ = False
matchPattern' (p : ps) (c : cs) = p == c && matchPattern' ps cs
matchPattern' (_ : _) [] = False

-- Match * (zero or more characters)
matchStar :: String -> String -> Bool
matchStar ps s =
  matchPattern' ps s || case s of
    [] -> False
    (_ : cs) -> matchStar ps cs

-- Parse [abc] or [!abc] or [^abc] character class
parseCharClass :: String -> Maybe (String, Bool, String)
parseCharClass ('^' : rest) = parseChars True rest
parseCharClass ('!' : rest) = parseChars True rest
parseCharClass rest = parseChars False rest

parseChars :: Bool -> String -> Maybe (String, Bool, String)
parseChars negated s = case span (/= ']') s of
  (_, []) -> Nothing
  (chars, ']' : rest) -> Just (expandRanges chars, negated, rest)
  _ -> Nothing

-- Expand character ranges like a-z
expandRanges :: String -> String
expandRanges [] = []
expandRanges [c] = [c]
expandRanges (c1 : '-' : c2 : rest)
  | c1 <= c2 = [c1 .. c2] ++ expandRanges rest
  | otherwise = c1 : '-' : expandRanges (c2 : rest)
expandRanges (c : rest) = c : expandRanges rest

patternCanMatchDot :: String -> Bool
patternCanMatchDot [] = False
patternCanMatchDot ('.' : _) = True
patternCanMatchDot ('\\' : '.' : _) = True
patternCanMatchDot ('[' : rest) = case parseCharClass rest of
  Just (chars, negated, _) -> if negated then '.' `notElem` chars else '.' `elem` chars
  Nothing -> False
patternCanMatchDot _ = False
