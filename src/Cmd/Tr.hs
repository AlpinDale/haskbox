{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Tr (run) where

import Data.Array.Unboxed (UArray, array, (!))
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (chr, isAlpha, isDigit, isLower, isSpace, isUpper, ord)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (poke)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox tr: " ++ err
    exitFailure
  Right (opts, sets)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "tr") >> exitSuccess
    | otherwise -> case sets of
        [] -> do
          hPutStrLn stderr "haskbox tr: missing operand"
          hPutStrLn stderr "Try 'haskbox tr --help' for more information."
          exitFailure
        [set1]
          | optDelete opts -> trDelete opts set1
          | optSqueeze opts -> trSqueeze opts set1
          | otherwise -> do
              hPutStrLn stderr "haskbox tr: missing operand after SET1"
              hPutStrLn stderr "Try 'haskbox tr --help' for more information."
              exitFailure
        [set1, set2]
          | optDelete opts && optSqueeze opts -> trDeleteAndSqueeze opts set1 set2
          | optDelete opts -> trDelete opts set1
          | optSqueeze opts -> trTranslateAndSqueeze opts set1 set2
          | otherwise -> trTranslate opts set1 set2
        _ -> do
          hPutStrLn stderr "haskbox tr: extra operand"
          exitFailure

data Opts = Opts
  { optComplement :: !Bool,
    optDelete :: !Bool,
    optSqueeze :: !Bool,
    optTruncate :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optComplement = False,
      optDelete = False,
      optSqueeze = False,
      optTruncate = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--complement" : rest) = parseArgs opts {optComplement = True} rest
parseArgs opts ("--delete" : rest) = parseArgs opts {optDelete = True} rest
parseArgs opts ("--squeeze-repeats" : rest) = parseArgs opts {optSqueeze = True} rest
parseArgs opts ("--truncate-set1" : rest) = parseArgs opts {optTruncate = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
-- Handle combined short options like -cd, -ds, etc.
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      'c' -> parseShortFlags o {optComplement = True} fs r
      'C' -> parseShortFlags o {optComplement = True} fs r
      'd' -> parseShortFlags o {optDelete = True} fs r
      's' -> parseShortFlags o {optSqueeze = True} fs r
      't' -> parseShortFlags o {optTruncate = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (arg : rest) = do
  (o, sets) <- parseArgs opts rest
  Right (o, arg : sets)

-- | Build a membership lookup table (256 bools as Word8: 0 or 1)
buildMembershipTable :: [Word8] -> UArray Word8 Word8
buildMembershipTable chars =
  array (0, 255) [(i, if i `elem` chars then 1 else 0) | i <- [0 .. 255]]

-- | Build a translation lookup table
buildTranslateTable :: [Word8] -> [Word8] -> UArray Word8 Word8
buildTranslateTable chars1 chars2 =
  array (0, 255) [(i, lookupTrans i) | i <- [0 .. 255]]
  where
    pairs = zip chars1 chars2
    lookupTrans c = case [to | (from, to) <- pairs, from == c] of
      (x : _) -> x
      [] -> c

expandSet :: String -> [Word8]
expandSet = go
  where
    go [] = []
    go ('\\' : 'n' : rest) = 10 : go rest
    go ('\\' : 't' : rest) = 9 : go rest
    go ('\\' : 'r' : rest) = 13 : go rest
    go ('\\' : '\\' : rest) = fromIntegral (ord '\\') : go rest
    go ('\\' : c : rest) = fromIntegral (ord c) : go rest
    go ('[' : ':' : rest) = expandClass rest
    go (a : '-' : b : rest)
      | ord a <= ord b = map (fromIntegral . ord) [a .. b] ++ go rest
      | otherwise = fromIntegral (ord a) : fromIntegral (ord '-') : go (b : rest)
    go (c : rest) = fromIntegral (ord c) : go rest

    expandClass s = case break (== ':') s of
      (className, ':' : ']' : rest) -> classChars className ++ go rest
      _ -> map (fromIntegral . ord) ('[' : ':' : s)

    classChars "alnum" = [c | c <- [0 .. 255], isAlpha (chr (fromIntegral c)) || isDigit (chr (fromIntegral c))]
    classChars "alpha" = [c | c <- [0 .. 255], isAlpha (chr (fromIntegral c))]
    classChars "blank" = [9, 32]
    classChars "cntrl" = [0 .. 31] ++ [127]
    classChars "digit" = [48 .. 57]
    classChars "graph" = [33 .. 126]
    classChars "lower" = [c | c <- [0 .. 255], isLower (chr (fromIntegral c))]
    classChars "print" = [32 .. 126]
    classChars "punct" = [c | c <- [33 .. 126], not (isAlpha (chr (fromIntegral c))) && not (isDigit (chr (fromIntegral c)))]
    classChars "space" = [c | c <- [0 .. 255], isSpace (chr (fromIntegral c))]
    classChars "upper" = [c | c <- [0 .. 255], isUpper (chr (fromIntegral c))]
    classChars "xdigit" = [48 .. 57] ++ [65 .. 70] ++ [97 .. 102]
    classChars _ = []

trDelete :: Opts -> String -> IO ()
trDelete opts set1 = do
  let chars1 = expandSet set1
      memberTable = buildMembershipTable chars1
      -- Use table-based lookup directly instead of function
      keepTable =
        if optComplement opts
          then memberTable
          else array (0, 255) [(i, 1 - (memberTable ! i)) | i <- [0 .. 255]]
  contents <- BS.hGetContents stdin
  let result = filterByTable keepTable contents
  BS.hPut stdout result

-- | Fast filter using a pre-computed lookup table (1 = keep, 0 = drop)
filterByTable :: UArray Word8 Word8 -> BS.ByteString -> BS.ByteString
filterByTable keepTable bs
  | BS.null bs = bs
  | otherwise = BSI.unsafeCreateUptoN (BS.length bs) $ \dst -> do
      let !len = BS.length bs
          go !srcIdx !dstIdx
            | srcIdx >= len = return dstIdx
            | otherwise =
                let !c = BSU.unsafeIndex bs srcIdx
                 in if keepTable ! c == 1
                      then do
                        poke (dst `plusPtr` dstIdx) c
                        go (srcIdx + 1) (dstIdx + 1)
                      else go (srcIdx + 1) dstIdx
      go 0 0

trSqueeze :: Opts -> String -> IO ()
trSqueeze opts set1 = do
  let chars1 = expandSet set1
      memberTable = buildMembershipTable chars1
      inSet c = memberTable ! c == 1
      shouldSqueeze = if optComplement opts then not . inSet else inSet
  contents <- BS.hGetContents stdin
  let result = squeeze shouldSqueeze contents
  BS.hPut stdout result

trTranslate :: Opts -> String -> String -> IO ()
trTranslate opts set1 set2 = do
  let chars1 = expandSet set1
      chars2 = expandSet set2
      chars1' = if optComplement opts then [c | c <- [0 .. 255], c `notElem` chars1] else chars1
      chars2' = padSet chars1' chars2
      transTable = buildTranslateTable chars1' chars2'
  contents <- BS.hGetContents stdin
  let result = BS.map (transTable !) contents
  BS.hPut stdout result

trTranslateAndSqueeze :: Opts -> String -> String -> IO ()
trTranslateAndSqueeze opts set1 set2 = do
  let chars1 = expandSet set1
      chars2 = expandSet set2
      chars1' = if optComplement opts then [c | c <- [0 .. 255], c `notElem` chars1] else chars1
      chars2' = padSet chars1' chars2
      transTable = buildTranslateTable chars1' chars2'
      memberTable2 = buildMembershipTable chars2
      inSet2 c = memberTable2 ! c == 1
  contents <- BS.hGetContents stdin
  let translated = BS.map (transTable !) contents
      result = squeeze inSet2 translated
  BS.hPut stdout result

trDeleteAndSqueeze :: Opts -> String -> String -> IO ()
trDeleteAndSqueeze opts set1 set2 = do
  let chars1 = expandSet set1
      chars2 = expandSet set2
      memberTable1 = buildMembershipTable chars1
      memberTable2 = buildMembershipTable chars2
      inSet2 c = memberTable2 ! c == 1
      keepTable =
        if optComplement opts
          then memberTable1
          else array (0, 255) [(i, 1 - (memberTable1 ! i)) | i <- [0 .. 255]]
  contents <- BS.hGetContents stdin
  let deleted = filterByTable keepTable contents
      result = squeeze inSet2 deleted
  BS.hPut stdout result

padSet :: [Word8] -> [Word8] -> [Word8]
padSet set1 set2
  | length set2 >= length set1 = set2
  | null set2 = set2
  | otherwise = set2 ++ replicate (length set1 - length set2) (last set2)

-- | Squeeze repeated characters efficiently using direct memory allocation
squeeze :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
squeeze shouldSqueeze bs
  | BS.null bs = bs
  | otherwise = BSI.unsafeCreateUptoN (BS.length bs) $ \dst -> do
      let !first = BSU.unsafeIndex bs 0
      poke dst first
      let !len = BS.length bs
          go !srcIdx !dstIdx !prev
            | srcIdx >= len = return dstIdx
            | otherwise =
                let !c = BSU.unsafeIndex bs srcIdx
                 in if shouldSqueeze c && c == prev
                      then go (srcIdx + 1) dstIdx prev
                      else do
                        poke (dst `plusPtr` dstIdx) c
                        go (srcIdx + 1) (dstIdx + 1) c
      go 1 1 first

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox tr [OPTION]... SET1 [SET2]",
        "Translate, squeeze, and/or delete characters from standard input,",
        "writing to standard output.",
        "",
        "  -c, -C, --complement    use the complement of SET1",
        "  -d, --delete            delete characters in SET1, do not translate",
        "  -s, --squeeze-repeats   replace each sequence of repeated characters",
        "                            in the last specified SET with a single char",
        "  -t, --truncate-set1     first truncate SET1 to length of SET2",
        "      --help              display this help and exit",
        "      --version           output version information and exit",
        "",
        "SETs are specified as strings of characters. Interpreted sequences:",
        "  \\n, \\t, \\r, \\\\         newline, tab, carriage return, backslash",
        "  CHAR1-CHAR2            all characters from CHAR1 to CHAR2 in order",
        "  [:alnum:], [:alpha:], [:blank:], [:cntrl:], [:digit:], [:graph:]",
        "  [:lower:], [:print:], [:punct:], [:space:], [:upper:], [:xdigit:]"
      ]
