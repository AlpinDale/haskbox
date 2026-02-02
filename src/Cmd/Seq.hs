module Cmd.Seq (run) where

import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.Char (isDigit)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdout)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox seq: " ++ err
    exitFailure
  Right (opts, numbers)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "seq") >> exitSuccess
    | otherwise -> case numbers of
        [last'] -> seqOutput opts 1 1 last'
        [first', last'] -> seqOutput opts first' 1 last'
        [first', incr, last'] -> seqOutput opts first' incr last'
        _ -> do
          hPutStrLn stderr "haskbox seq: missing operand"
          hPutStrLn stderr "Try 'haskbox seq --help' for more information."
          exitFailure

data Opts = Opts
  { optSeparator :: !String,
    optFormat :: !(Maybe String),
    optEqualWidth :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optSeparator = "\n",
      optFormat = Nothing,
      optEqualWidth = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [Double])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--separator" : s : rest) = parseArgs opts {optSeparator = s} rest
parseArgs opts ("--equal-width" : rest) = parseArgs opts {optEqualWidth = True} rest
parseArgs opts (('-' : 's' : s) : rest)
  | null s = case rest of
      (sep : rest') -> parseArgs opts {optSeparator = sep} rest'
      [] -> Left "option requires an argument -- 's'"
  | otherwise = parseArgs opts {optSeparator = s} rest
parseArgs opts ("-w" : rest) = parseArgs opts {optEqualWidth = True} rest
parseArgs opts ("--" : rest) = parseNums opts rest
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (arg : rest)
  | isNumber arg = do
      (o, nums) <- parseArgs opts rest
      case parseNumber arg of
        Just n -> Right (o, n : nums)
        Nothing -> Left $ "invalid floating point argument: '" ++ arg ++ "'"
parseArgs _ (('-' : c : _) : _) = Left $ "invalid option -- '" ++ [c] ++ "'"
parseArgs _ (arg : _) = Left $ "invalid argument: '" ++ arg ++ "'"

parseNums :: Opts -> [String] -> Either String (Opts, [Double])
parseNums opts [] = Right (opts, [])
parseNums opts (arg : rest) = do
  (o, nums) <- parseNums opts rest
  case parseNumber arg of
    Just n -> Right (o, n : nums)
    Nothing -> Left $ "invalid floating point argument: '" ++ arg ++ "'"

isNumber :: String -> Bool
isNumber [] = False
isNumber ('-' : rest) = isPositiveNumber rest
isNumber s = isPositiveNumber s

isPositiveNumber :: String -> Bool
isPositiveNumber [] = False
isPositiveNumber str =
  let (before, after) = span isDigit str
   in not (null before)
        && case after of
          [] -> True
          ('.' : rest') -> all isDigit rest' && not (null rest')
          _ -> False

parseNumber :: String -> Maybe Double
parseNumber s
  | isNumber s = Just (read s)
  | otherwise = Nothing

seqOutput :: Opts -> Double -> Double -> Double -> IO ()
seqOutput opts first' incr last'
  | incr == 0 = do
      hPutStrLn stderr "haskbox seq: increment must not be zero"
      exitFailure
  | incr > 0 && first' > last' = return ()
  | incr < 0 && first' < last' = return ()
  | otherwise = do
      let nums = takeWhile inRange [first', first' + incr ..]
          inRange n
            | incr > 0 = n <= last' + 1e-10
            | otherwise = n >= last' - 1e-10
          width
            | optEqualWidth opts = maximum (map (length . formatNum) nums)
            | otherwise = 0
          formatNum n
            | isInt n = show (round n :: Integer)
            | otherwise = show n
          isInt n = n == fromIntegral (round n :: Integer)
          padNum n =
            let s = formatNum n
             in replicate (width - length s) '0' ++ s
          sep = C8.pack (optSeparator opts)
          output =
            B.hPutBuilder stdout $
              mconcat $
                intersperse' (B.byteString sep) $
                  map (B.string7 . padNum) nums
      output
      C8.hPutStrLn stdout C8.empty
  where
    intersperse' _ [] = []
    intersperse' _ [x] = [x]
    intersperse' s (x : xs) = x : s : intersperse' s xs

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox seq [OPTION]... LAST",
        "  or:  haskbox seq [OPTION]... FIRST LAST",
        "  or:  haskbox seq [OPTION]... FIRST INCREMENT LAST",
        "Print numbers from FIRST to LAST, in steps of INCREMENT.",
        "",
        "  -s, --separator=STRING  use STRING to separate numbers (default: \\n)",
        "  -w, --equal-width       equalize width by padding with leading zeroes",
        "      --help              display this help and exit",
        "      --version           output version information and exit"
      ]
