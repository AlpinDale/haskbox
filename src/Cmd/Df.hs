module Cmd.Df (run) where

import Cmd.Df.Mount
import Cmd.Df.Types
import Control.Monad (forM, unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox df: " ++ err
    hPutStrLn stderr "Try 'haskbox df --help' for more information."
    exitFailure
  Right (opts, paths)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "df") >> exitSuccess
    | otherwise -> do
        -- Check for mutually exclusive options
        case checkMutualExclusion opts of
          Just err -> do
            hPutStrLn stderr $ "haskbox df: " ++ err
            hPutStrLn stderr "Try 'haskbox df --help' for more information."
            exitFailure
          Nothing -> runDf opts paths

-- | Check for mutually exclusive options
checkMutualExclusion :: Opts -> Maybe String
checkMutualExclusion opts
  | optInodes opts && hasOutput = Just "options -i and --output are mutually exclusive"
  | optPosix opts && hasOutput = Just "options -P and --output are mutually exclusive"
  | optPrintType opts && hasOutput = Just "options -T and --output are mutually exclusive"
  | otherwise = Nothing
  where
    hasOutput = case optOutput opts of
      Just _ -> True
      Nothing -> False

runDf :: Opts -> [FilePath] -> IO ()
runDf opts paths = do
  -- Get block size from environment if not specified
  blockSize <- getEffectiveBlockSize opts

  -- Sync if requested
  when (optSync opts) $ return () -- TODO: implement sync

  -- Get filesystems to display
  (fsInfos, hadErrors) <-
    if null paths
      then do
        fss <- getMountedFilesystems
        return (fss, False)
      else do
        results <- forM paths $ \path -> do
          info <- getFilesystemInfo path
          case info of
            Left err -> do
              hPutStrLn stderr $ "haskbox df: " ++ path ++ ": " ++ err
              return (Nothing, True)
            Right fs -> return (Just fs, False)
        let fss = [fs | (Just fs, _) <- results]
            errs = any snd results
        return (fss, errs)

  -- Filter filesystems (preserve order from mount table)
  let filtered = filterFilesystems opts fsInfos

  -- Only print output if there are results or no errors (default mode)
  unless (null filtered && hadErrors) $ do
    printOutput opts blockSize filtered

    -- Print total if requested
    when (optTotal opts && not (null filtered)) $
      printTotal opts blockSize filtered

  -- Exit with failure if we had errors
  when hadErrors exitFailure

-- | Get effective block size
getEffectiveBlockSize :: Opts -> IO Integer
getEffectiveBlockSize opts
  | optHumanReadable opts = return 1 -- Will be formatted as human readable
  | optSI opts = return 1 -- Will be formatted as SI units
  | Just bs <- optBlockSize opts = return bs
  | otherwise = do
      -- Check BLOCK_SIZE and DF_BLOCK_SIZE environment variables
      mDfBlockSize <- lookupEnv "DF_BLOCK_SIZE"
      mBlockSize <- lookupEnv "BLOCK_SIZE"
      mPosixlyCorrect <- lookupEnv "POSIXLY_CORRECT"
      -- -P uses 512-byte blocks only with POSIXLY_CORRECT, else 1024
      let posixCorrect = isJust mPosixlyCorrect
      if optPosix opts
        then return (if posixCorrect then 512 else 1024)
        else case mDfBlockSize >>= parseBlockSize of
          Just bs -> return bs
          Nothing -> case mBlockSize >>= parseBlockSize of
            Just bs -> return bs
            Nothing ->
              if posixCorrect
                then return 512
                else return 1024 -- Default to 1K blocks

-- | Parse block size string (e.g., "1K", "1M", "1024")
parseBlockSize :: String -> Maybe Integer
parseBlockSize s = case reads s of
  [(n, "")] -> Just n
  [(n, "K")] -> Just (n * 1024)
  [(n, "M")] -> Just (n * 1024 * 1024)
  [(n, "G")] -> Just (n * 1024 * 1024 * 1024)
  [(n, "T")] -> Just (n * 1024 * 1024 * 1024 * 1024)
  _ -> Nothing

-- | Filter filesystems based on options
filterFilesystems :: Opts -> [FsInfo] -> [FsInfo]
filterFilesystems opts = filter keep
  where
    keep fs
      | not (optAll opts) && isPseudoFs fs = False
      | not (optAll opts) && isFirmlink fs = False
      | optLocal opts && isRemoteFs fs = False
      | not (null (optTypes opts)) && fsType fs `notElem` optTypes opts = False
      | not (null (optExcludeTypes opts)) && fsType fs `elem` optExcludeTypes opts = False
      | otherwise = True

    isPseudoFs fs = fsType fs `elem` pseudoFsTypes || fsTotalBlocks fs == 0

    -- On macOS, /System/Volumes/Data is a firmlink to the root volume
    -- GNU df filters this out by default
    isFirmlink fs = fsMountPoint fs == "/System/Volumes/Data"

    pseudoFsTypes =
      [ "devfs",
        "devpts",
        "proc",
        "sysfs",
        "tmpfs",
        "cgroup",
        "cgroup2",
        "autofs",
        "mqueue",
        "hugetlbfs",
        "debugfs",
        "tracefs",
        "securityfs",
        "pstore",
        "configfs",
        "fusectl",
        "binfmt_misc"
      ]

    isRemoteFs fs = fsType fs `elem` remoteFsTypes

    remoteFsTypes =
      [ "nfs",
        "nfs4",
        "cifs",
        "smb",
        "smbfs",
        "afs",
        "ncpfs",
        "fuse.sshfs"
      ]

-- | Print df output
printOutput :: Opts -> Integer -> [FsInfo] -> IO ()
printOutput opts blockSize fsInfos = do
  let header = getHeader opts blockSize
      rows = map (formatRow opts blockSize) fsInfos
      -- Calculate column widths
      allRows = header : rows
      colWidths = getColumnWidths allRows
      -- Format with proper alignment
      formatted = map (formatAligned colWidths) allRows
  mapM_ putStrLn formatted

-- | Get header row
getHeader :: Opts -> Integer -> [String]
getHeader opts blockSize
  | optPosix opts && optPrintType opts =
      ["Filesystem", "Type", blockHeader, "Used", "Available", "Capacity", "Mounted on"]
  | optPosix opts =
      ["Filesystem", blockHeader, "Used", "Available", "Capacity", "Mounted on"]
  | optInodes opts && optPrintType opts =
      ["Filesystem", "Type", "Inodes", "IUsed", "IFree", "IUse%", "Mounted on"]
  | optInodes opts =
      ["Filesystem", "Inodes", "IUsed", "IFree", "IUse%", "Mounted on"]
  | optPrintType opts =
      ["Filesystem", "Type", blockHeader, "Used", availHeader, "Use%", "Mounted on"]
  | otherwise =
      ["Filesystem", blockHeader, "Used", availHeader, "Use%", "Mounted on"]
  where
    blockHeader
      | optHumanReadable opts = "Size"
      | optSI opts = "Size"
      | optPosix opts && blockSize == 1024 = "1024-blocks"
      | blockSize == 1024 = "1K-blocks"
      | blockSize == 512 = "512-blocks"
      | blockSize == 1 = "1B-blocks"
      | otherwise = show blockSize ++ "-blocks"

    -- GNU df uses "Avail" for human-readable, "Available" otherwise
    availHeader
      | optHumanReadable opts = "Avail"
      | optSI opts = "Avail"
      | otherwise = "Available"

-- | Format a single row
formatRow :: Opts -> Integer -> FsInfo -> [String]
formatRow opts blockSize fs
  | optPosix opts && optPrintType opts =
      [ fsDevice fs,
        fsType fs,
        formatBlocks opts blockSize total,
        formatBlocks opts blockSize used,
        formatBlocks opts blockSize avail,
        formatPercent used total,
        fsMountPoint fs
      ]
  | optPosix opts =
      [ fsDevice fs,
        formatBlocks opts blockSize total,
        formatBlocks opts blockSize used,
        formatBlocks opts blockSize avail,
        formatPercent used total,
        fsMountPoint fs
      ]
  | optInodes opts && optPrintType opts =
      [ fsDevice fs,
        fsType fs,
        show (fsTotalInodes fs),
        show iused,
        show (fsFreeInodes fs),
        formatPercent iused (fsTotalInodes fs),
        fsMountPoint fs
      ]
  | optInodes opts =
      [ fsDevice fs,
        show (fsTotalInodes fs),
        show iused,
        show (fsFreeInodes fs),
        formatPercent iused (fsTotalInodes fs),
        fsMountPoint fs
      ]
  | optPrintType opts =
      [ fsDevice fs,
        fsType fs,
        formatBlocks opts blockSize total,
        formatBlocks opts blockSize used,
        formatBlocks opts blockSize avail,
        formatPercent used total,
        fsMountPoint fs
      ]
  | otherwise =
      [ fsDevice fs,
        formatBlocks opts blockSize total,
        formatBlocks opts blockSize used,
        formatBlocks opts blockSize avail,
        formatPercent used total,
        fsMountPoint fs
      ]
  where
    total = fsTotalBlocks fs * fsBlockSize fs
    used = total - fsFreeBlocks fs * fsBlockSize fs
    avail = fsAvailBlocks fs * fsBlockSize fs
    iused = fsTotalInodes fs - fsFreeInodes fs

-- | Format block count
formatBlocks :: Opts -> Integer -> Integer -> String
formatBlocks opts blockSize bytes
  | optHumanReadable opts = humanReadable 1024 bytes
  | optSI opts = humanReadable 1000 bytes
  | blockSize == 1 = show bytes
  | otherwise = show ((bytes + blockSize - 1) `div` blockSize)

-- | Format as human readable (1K, 1M, etc.)
humanReadable :: Integer -> Integer -> String
humanReadable base bytes
  | bytes < base = show bytes
  | otherwise = go (fromIntegral bytes :: Double) suffixes
  where
    suffixes
      | base == 1000 = ["", "k", "M", "G", "T", "P", "E"]
      | otherwise = ["", "K", "M", "G", "T", "P", "E"]

    go :: Double -> [String] -> String
    go n [] = show (round n :: Integer)
    go n [s] = formatNum n ++ s
    go n (s : ss)
      | n < fromIntegral base = formatNum n ++ s
      | otherwise = go (n / fromIntegral base) ss

    formatNum :: Double -> String
    formatNum n
      | n < 10 = showDecimal 1 n
      | otherwise = show (ceiling n :: Integer)

    showDecimal :: Int -> Double -> String
    showDecimal places n =
      let factor = 10 ^ places
          rounded = fromIntegral (round (n * factor) :: Integer) / factor
       in if rounded == fromIntegral (floor rounded :: Integer)
            then show (floor rounded :: Integer)
            else show rounded

-- | Format percentage
formatPercent :: Integer -> Integer -> String
formatPercent used total
  | total == 0 = "-"
  | otherwise =
      let pct = (used * 100 + total - 1) `div` total -- Round up
       in show pct ++ "%"

-- | Print total row
printTotal :: Opts -> Integer -> [FsInfo] -> IO ()
printTotal opts blockSize fsInfos = do
  let totalBytes = sum [fsTotalBlocks fs * fsBlockSize fs | fs <- fsInfos]
      usedBytes = sum [(fsTotalBlocks fs - fsFreeBlocks fs) * fsBlockSize fs | fs <- fsInfos]
      availBytes = sum [fsAvailBlocks fs * fsBlockSize fs | fs <- fsInfos]
      totalInodes = sum [fsTotalInodes fs | fs <- fsInfos]
      usedInodes = sum [fsTotalInodes fs - fsFreeInodes fs | fs <- fsInfos]
      freeInodes = sum [fsFreeInodes fs | fs <- fsInfos]

      row
        | optInodes opts && optPrintType opts =
            ["total", "-", show totalInodes, show usedInodes, show freeInodes, formatPercent usedInodes totalInodes, "-"]
        | optInodes opts =
            ["total", show totalInodes, show usedInodes, show freeInodes, formatPercent usedInodes totalInodes, "-"]
        | optPrintType opts =
            [ "total",
              "-",
              formatBlocks opts blockSize totalBytes,
              formatBlocks opts blockSize usedBytes,
              formatBlocks opts blockSize availBytes,
              formatPercent usedBytes totalBytes,
              "-"
            ]
        | otherwise =
            [ "total",
              formatBlocks opts blockSize totalBytes,
              formatBlocks opts blockSize usedBytes,
              formatBlocks opts blockSize availBytes,
              formatPercent usedBytes totalBytes,
              "-"
            ]

  -- Get header to calculate column widths
  let header = getHeader opts blockSize
      allRows = header : map (formatRow opts blockSize) fsInfos ++ [row]
      colWidths = getColumnWidths allRows
  putStrLn $ formatAligned colWidths row

-- | Calculate column widths
getColumnWidths :: [[String]] -> [Int]
getColumnWidths rows =
  [maximum (map (length . (!! i)) rows) | i <- [0 .. maxCols - 1]]
  where
    maxCols = maximum (map length rows)

-- | Format row with alignment
-- First column is left-aligned, others are right-aligned, last column has no padding
formatAligned :: [Int] -> [String] -> String
formatAligned widths cols = unwords $ zipWith3 pad [0 ..] widths cols
  where
    pad :: Int -> Int -> String -> String
    pad idx w s
      | idx == length cols - 1 = s -- Don't pad last column
      | idx == 0 = s ++ replicate (w - length s) ' ' -- Left-align first column
      | otherwise = replicate (w - length s) ' ' ++ s -- Right-align others

-- Argument parsing

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("-a" : rest) = parseArgs opts {optAll = True} rest
parseArgs opts ("--all" : rest) = parseArgs opts {optAll = True} rest
parseArgs opts ("-h" : rest) = parseArgs opts {optHumanReadable = True} rest
parseArgs opts ("--human-readable" : rest) = parseArgs opts {optHumanReadable = True} rest
parseArgs opts ("-H" : rest) = parseArgs opts {optSI = True} rest
parseArgs opts ("--si" : rest) = parseArgs opts {optSI = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optInodes = True} rest
parseArgs opts ("--inodes" : rest) = parseArgs opts {optInodes = True} rest
parseArgs opts ("-k" : rest) = parseArgs opts {optBlockSize = Just 1024} rest
parseArgs opts ("-l" : rest) = parseArgs opts {optLocal = True} rest
parseArgs opts ("--local" : rest) = parseArgs opts {optLocal = True} rest
parseArgs opts ("-P" : rest) = parseArgs opts {optPosix = True} rest
parseArgs opts ("--portability" : rest) = parseArgs opts {optPosix = True} rest
parseArgs opts ("-T" : rest) = parseArgs opts {optPrintType = True} rest
parseArgs opts ("--print-type" : rest) = parseArgs opts {optPrintType = True} rest
parseArgs opts ("--total" : rest) = parseArgs opts {optTotal = True} rest
parseArgs opts ("--sync" : rest) = parseArgs opts {optSync = True} rest
parseArgs opts ("--no-sync" : rest) = parseArgs opts {optSync = False} rest
parseArgs opts ("--output" : rest) = parseArgs opts {optOutput = Just allFields} rest
parseArgs opts (opt : rest)
  | "--output=" `isPrefixOf` opt =
      let fieldStr = drop 9 opt
       in case parseFields fieldStr of
            Just fields -> parseArgs opts {optOutput = Just fields} rest
            Nothing -> Left $ "invalid field: '" ++ fieldStr ++ "'"
  | "-B" `isPrefixOf` opt =
      let sizeStr = drop 2 opt
       in case parseBlockSize sizeStr of
            Just bs -> parseArgs opts {optBlockSize = Just bs} rest
            Nothing -> Left $ "invalid block size: '" ++ sizeStr ++ "'"
  | "--block-size=" `isPrefixOf` opt =
      let sizeStr = drop 13 opt
       in case parseBlockSize sizeStr of
            Just bs -> parseArgs opts {optBlockSize = Just bs} rest
            Nothing -> Left $ "invalid block size: '" ++ sizeStr ++ "'"
  | "-t" `isPrefixOf` opt =
      let fstype = drop 2 opt
       in if null fstype
            then case rest of
              (t : rest') -> parseArgs opts {optTypes = t : optTypes opts} rest'
              [] -> Left "-t requires an argument"
            else parseArgs opts {optTypes = fstype : optTypes opts} rest
  | "--type=" `isPrefixOf` opt =
      let fstype = drop 7 opt
       in parseArgs opts {optTypes = fstype : optTypes opts} rest
  | "-x" `isPrefixOf` opt =
      let fstype = drop 2 opt
       in if null fstype
            then case rest of
              (t : rest') -> parseArgs opts {optExcludeTypes = t : optExcludeTypes opts} rest'
              [] -> Left "-x requires an argument"
            else parseArgs opts {optExcludeTypes = fstype : optExcludeTypes opts} rest
  | "--exclude-type=" `isPrefixOf` opt =
      let fstype = drop 15 opt
       in parseArgs opts {optExcludeTypes = fstype : optExcludeTypes opts} rest
  | "-" `isPrefixOf` opt = Left $ "unrecognized option '" ++ opt ++ "'"
parseArgs opts (path : rest) = do
  (opts', paths) <- parseArgs opts rest
  Right (opts', path : paths)

-- | Parse field list for --output
parseFields :: String -> Maybe [Field]
parseFields str = mapM parseField (splitOn ',' str)
  where
    splitOn _ [] = []
    splitOn c s = case break (== c) s of
      (x, []) -> [x]
      (x, _ : rest) -> x : splitOn c rest

parseField :: String -> Maybe Field
parseField "source" = Just FieldSource
parseField "fstype" = Just FieldFstype
parseField "size" = Just FieldSize
parseField "used" = Just FieldUsed
parseField "avail" = Just FieldAvail
parseField "pcent" = Just FieldPcent
parseField "itotal" = Just FieldItotal
parseField "iused" = Just FieldIused
parseField "iavail" = Just FieldIavail
parseField "ipcent" = Just FieldIpcent
parseField "target" = Just FieldTarget
parseField "file" = Just FieldFile
parseField _ = Nothing

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox df [OPTION]... [FILE]...",
        "Show information about the file system on which each FILE resides,",
        "or all file systems by default.",
        "",
        "  -a, --all             include pseudo, duplicate, inaccessible file systems",
        "  -B, --block-size=SIZE scale sizes by SIZE",
        "  -h, --human-readable  print sizes in powers of 1024 (e.g., 1023M)",
        "  -H, --si              print sizes in powers of 1000 (e.g., 1.1G)",
        "  -i, --inodes          list inode information instead of block usage",
        "  -k                    like --block-size=1K",
        "  -l, --local           limit listing to local file systems",
        "  -P, --portability     use the POSIX output format",
        "  -T, --print-type      print file system type",
        "  -t, --type=TYPE       limit listing to file systems of type TYPE",
        "  -x, --exclude-type=TYPE",
        "                        limit listing to file systems not of type TYPE",
        "      --total           elicit a grand total",
        "      --sync            invoke sync before getting usage info",
        "      --no-sync         do not invoke sync before getting usage info (default)",
        "      --output[=FIELD_LIST]",
        "                        use the output format defined by FIELD_LIST,",
        "                        or print all fields if FIELD_LIST is omitted",
        "      --help            display this help and exit",
        "      --version         output version information and exit",
        "",
        "FIELD_LIST is a comma-separated list of columns to be included. Valid",
        "field names are: 'source', 'fstype', 'itotal', 'iused', 'iavail', 'ipcent',",
        "'size', 'used', 'avail', 'pcent', 'file' and 'target'."
      ]
