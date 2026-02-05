{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

module Cmd.Ls.Format
  ( formatLong,
    formatLongEntries,
    LongEntry (..),
    formatColumns,
    formatAcross,
    formatCommas,
    formatSingleColumn,
    formatModeString,
    formatSize,
    formatBlocks,
    formatTime,
    formatIndicator,
    formatName,
    formatEntryName,
    displayNameWidth,
    getTerminalWidth,
  )
where

import Cmd.Ls.Color
import Cmd.Ls.Types
import Control.Exception (IOException, try)
import Data.Bits ((.&.))
import Data.Char (isAsciiLower, isAsciiUpper, isControl, isDigit, ord)
import Data.List (intercalate, zipWith5)
import Data.Time.Clock (UTCTime, diffTimeToPicoseconds, diffUTCTime, getCurrentTime, utctDayTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format qualified as DTF
import Data.Time.LocalTime (TimeZone, utcToZonedTime)
import Foreign.C.Types (CInt (..), CUShort (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.Posix.Files
  ( groupExecuteMode,
    groupReadMode,
    groupWriteMode,
    otherExecuteMode,
    otherReadMode,
    otherWriteMode,
    ownerExecuteMode,
    ownerReadMode,
    ownerWriteMode,
    setGroupIDMode,
    setUserIDMode,
  )
import System.Posix.Types (FileMode, FileOffset, GroupID, UserID)
import System.Posix.User (getGroupEntryForID, getUserEntryForID, groupName, userName)
import Text.Read (readMaybe)

stickyMode :: FileMode
stickyMode = 0o1000

-- | Terminal window size structure (struct winsize)
data Winsize = Winsize
  { _wsRow :: !CUShort,
    wsCol :: !CUShort,
    _wsXpixel :: !CUShort,
    _wsYpixel :: !CUShort
  }

instance Storable Winsize where
  sizeOf _ = 8
  alignment _ = 2
  peek ptr = do
    r <- peekByteOff ptr 0
    c <- peekByteOff ptr 2
    x <- peekByteOff ptr 4
    y <- peekByteOff ptr 6
    return $ Winsize r c x y
  poke ptr (Winsize r c x y) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 2 c
    pokeByteOff ptr 4 x
    pokeByteOff ptr 6 y

-- | TIOCGWINSZ ioctl request code
-- On macOS: 0x40087468, on Linux: 0x5413
#if defined(darwin_HOST_OS)
foreign import capi "sys/ioctl.h value TIOCGWINSZ" tiocgwinsz :: CInt
#else
foreign import capi "sys/ioctl.h value TIOCGWINSZ" tiocgwinsz :: CInt
#endif

foreign import ccall unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CInt -> Ptr Winsize -> IO CInt

-- | Get terminal width using ioctl TIOCGWINSZ, with fallbacks
getTerminalWidth :: IO Int
getTerminalWidth = do
  -- First check COLUMNS environment variable (user override)
  mCols <- lookupEnv "COLUMNS"
  case mCols >>= readMaybe of
    Just n -> return n
    Nothing -> do
      -- Try ioctl on stdout (fd 1)
      result <- try (getTerminalWidthFromFd 1) :: IO (Either IOException (Maybe Int))
      case result of
        Right (Just w) | w > 0 -> return w
        _ -> return 80 -- Default fallback

-- | Get terminal width from a file descriptor using ioctl
getTerminalWidthFromFd :: CInt -> IO (Maybe Int)
getTerminalWidthFromFd fd =
  alloca $ \ptr -> do
    result <- c_ioctl fd tiocgwinsz ptr
    if result == 0
      then do
        ws <- peek ptr
        return $ Just (fromIntegral (wsCol ws))
      else return Nothing

data LongEntry = LongEntry
  { leLine :: String,
    leNameStart :: Int,
    leNameEnd :: Int
  }

-- | Format files in long format (-l)
formatLong :: Opts -> TimeZone -> ColorScheme -> [FileInfo] -> IO [String]
formatLong opts tz cs files = do
  entries <- formatLongEntries opts tz cs files
  return $ map leLine entries

formatLongEntries :: Opts -> TimeZone -> ColorScheme -> [FileInfo] -> IO [LongEntry]
formatLongEntries opts tz cs files = do
  let needsOwner = not (optHideOwner opts)
      needsGroup = not (optHideGroup opts) && not (optNoGroup opts)
      needsAuthor = optAuthor opts
      needsContext = optContext opts

  now <- getCurrentTime
  let colorFlags = colorPrefixFlags opts cs files

  -- Get user/group names (or use numeric IDs)
  ownerNames <-
    if optNumericUID opts || not needsOwner
      then return $ map (show . fiOwner) files
      else mapM (getUserName . fiOwner) files

  groupNames <-
    if optNumericUID opts || not needsGroup
      then return $ map (show . fiGroup) files
      else mapM (getGroupName . fiGroup) files

  let authorNames =
        if needsAuthor
          then ownerNames
          else replicate (length files) ""
      contextNames =
        if needsContext
          then replicate (length files) "?"
          else replicate (length files) ""

      -- Calculate column widths for alignment
      inodeWidth = if optInode opts then maxWidth (map (show . fiInode) files) else 0
      blocksWidth = if optSize opts then maxWidth (map (formatBlocks opts . fiBlocks) files) else 0
      linksWidth = maxWidth $ map (show . fiLinks) files
      ownerWidth = if needsOwner then maxWidth ownerNames else 0
      groupWidth = if needsGroup then maxWidth groupNames else 0
      authorWidth = if needsAuthor then maxWidth authorNames else 0
      contextWidth = if needsContext then maxWidth contextNames else 0
      sizeWidth = maxWidth $ map (formatFileSize opts . fiSize) files

      formatEntry :: (FileInfo, Bool) -> String -> String -> String -> String -> LongEntry
      formatEntry (fi, prefixReset) owner group author context =
        let inode =
              if optInode opts
                then padLeft inodeWidth (show (fiInode fi)) ++ " "
                else ""
            blocks =
              if optSize opts
                then padLeft blocksWidth (formatBlocks opts (fiBlocks fi)) ++ " "
                else ""
            mode = formatModeString (fiMode fi) (fiType fi)
            links = padLeft linksWidth (show (fiLinks fi))
            ownerStr =
              if needsOwner
                then " " ++ padRight ownerWidth owner
                else ""
            groupStr =
              if needsGroup
                then " " ++ padRight groupWidth group
                else ""
            authorStr =
              if needsAuthor
                then " " ++ padRight authorWidth author
                else ""
            contextStr =
              if needsContext
                then " " ++ padRight contextWidth context
                else ""
            sizeStr = padLeft sizeWidth (formatFileSize opts (fiSize fi))
            timeStr = formatFileTime now opts tz fi
            namePlain = formatNamePlain opts fi
            nameDecorated = formatNameWith prefixReset opts cs fi
            prefix = inode ++ blocks ++ mode ++ " " ++ links ++ ownerStr ++ groupStr ++ authorStr ++ contextStr ++ " " ++ sizeStr ++ " " ++ timeStr ++ " "
            nameStart = length prefix
            nameEnd = nameStart + length namePlain
            line = prefix ++ nameDecorated
         in LongEntry line nameStart nameEnd

  let fileInfos = zip files colorFlags
  return $ zipWith5 formatEntry fileInfos ownerNames groupNames authorNames contextNames

-- | Format files in column format (-C, default for terminal)
formatColumns :: Opts -> Int -> ColorScheme -> [FileInfo] -> [String]
formatColumns opts termWidth cs files
  | null files = []
  | otherwise =
      let displayWidths = map (displayEntryWidth opts) files
          -- Max columns is bounded by: termWidth / (minWidth + 2 spaces)
          minWidth = minimum displayWidths
          useTabs = shouldUseTabs opts
          gap = 2
          maxCols = max 1 (termWidth `div` (minWidth + gap))
          layout@(ColumnLayout numCols _) = findOptimalColumns termWidth gap displayWidths maxCols
          numRows = (length files + numCols - 1) `div` numCols
          order =
            [ col * numRows + row
            | row <- [0 .. numRows - 1],
              col <- [0 .. numCols - 1],
              col * numRows + row < length files
            ]
          flags = colorPrefixFlagsForOrder opts cs files order
          names = zipWith (\flag fi -> formatEntryNameWith flag opts cs fi) flags files
       in renderColumnsVertical layout names displayWidths (optTabSize opts) useTabs gap

-- | Format files in across format (-x)
formatAcross :: Opts -> Int -> ColorScheme -> [FileInfo] -> [String]
formatAcross opts termWidth cs files
  | null files = []
  | otherwise =
      let displayWidths = map (displayEntryWidth opts) files
          minWidth = minimum displayWidths
          useTabs = shouldUseTabs opts
          gap = 2
          maxCols = max 1 (termWidth `div` (minWidth + gap))
          layout@(ColumnLayout numCols _) = findOptimalColumnsHorizontal termWidth gap displayWidths maxCols
          numRows = (length files + numCols - 1) `div` numCols
          order =
            [ row * numCols + col
            | row <- [0 .. numRows - 1],
              col <- [0 .. numCols - 1],
              row * numCols + col < length files
            ]
          flags = colorPrefixFlagsForOrder opts cs files order
          names = zipWith (\flag fi -> formatEntryNameWith flag opts cs fi) flags files
       in renderColumnsHorizontal layout names displayWidths (optTabSize opts) useTabs gap

-- | Format files in comma-separated format (-m)
formatCommas :: Opts -> Int -> ColorScheme -> [FileInfo] -> [String]
formatCommas opts termWidth cs files =
  let flags = colorPrefixFlags opts cs files
      names = zipWith (\flag fi -> formatEntryNameWith flag opts cs fi) flags files
   in wrapText termWidth (intercalate ", " names)

-- | Format files one per line (-1)
formatSingleColumn :: Opts -> ColorScheme -> [FileInfo] -> [String]
formatSingleColumn opts cs files =
  let flags = colorPrefixFlags opts cs files
   in zipWith (\flag fi -> formatEntryNameWith flag opts cs fi) flags files

formatNamePlain :: Opts -> FileInfo -> String
formatNamePlain opts fi = quoteName opts (fiName fi)

formatNameParts :: Opts -> FileInfo -> (String, String, String)
formatNameParts opts fi =
  let name = formatNamePlain opts fi
      indicator = formatIndicator opts fi
      linkTarget = case fiLinkTarget fi of
        Just target
          | optLongFormat opts || optHideOwner opts || optHideGroup opts ->
              " -> " ++ quoteName opts target
        _ -> ""
   in (name, indicator, linkTarget)

-- | Format file name with color, hyperlink, and indicator
formatName :: Opts -> ColorScheme -> FileInfo -> String
formatName = formatNameWith False

formatNameWith :: Bool -> Opts -> ColorScheme -> FileInfo -> String
formatNameWith prefixReset opts cs fi =
  let (name, indicator, linkTarget) = formatNameParts opts fi
      decorated = applyDecorations opts cs prefixReset fi name
   in decorated ++ indicator ++ linkTarget

-- | Format a non-long entry name (including -i/-s prefixes)
formatEntryName :: Opts -> ColorScheme -> FileInfo -> String
formatEntryName = formatEntryNameWith False

formatEntryNameWith :: Bool -> Opts -> ColorScheme -> FileInfo -> String
formatEntryNameWith prefixReset opts cs fi =
  let prefix = entryPrefix opts fi
      (name, indicator, _) = formatNameParts opts fi
      decorated = applyDecorations opts cs prefixReset fi name
   in prefix ++ decorated ++ indicator

-- | Format indicator suffix (-F, -p, --classify)
formatIndicator :: Opts -> FileInfo -> String
formatIndicator opts fi = case optIndicator opts of
  IndicatorNone -> ""
  IndicatorSlash -> case fiType fi of
    TypeDirectory -> "/"
    _ -> ""
  IndicatorFileType -> case fiType fi of
    TypeDirectory -> "/"
    TypeSymlink -> "@"
    TypeNamedPipe -> "|"
    TypeSocket -> "="
    TypeDoor -> ">"
    _ -> ""
  IndicatorClassify -> case fiType fi of
    TypeDirectory -> "/"
    TypeSymlink -> "@"
    TypeNamedPipe -> "|"
    TypeSocket -> "="
    TypeDoor -> ">"
    TypeRegular | fiExecutable fi -> "*"
    _ -> ""

entryPrefix :: Opts -> FileInfo -> String
entryPrefix opts fi =
  let inode =
        if optInode opts
          then show (fiInode fi) ++ " "
          else ""
      blocks =
        if optSize opts
          then formatBlocks opts (fiBlocks fi) ++ " "
          else ""
   in inode ++ blocks

applyDecorations :: Opts -> ColorScheme -> Bool -> FileInfo -> String -> String
applyDecorations opts cs prefixReset fi name =
  let colorCode = getFileColor cs fi
      colored =
        if optColorMode opts /= ColorNever && not (null colorCode)
          then
            if prefixReset
              then resetCode ++ colorize colorCode name
              else colorize colorCode name
          else name
      linked =
        if optHyperlink opts /= HyperlinkNever
          then hyperlinkize opts fi colored
          else colored
   in linked

colorPrefixFlags :: Opts -> ColorScheme -> [FileInfo] -> [Bool]
colorPrefixFlags opts cs files
  | optColorMode opts == ColorNever = replicate (length files) False
  | otherwise = markFirst (map (getFileColor cs) files)
  where
    markFirst [] = []
    markFirst (c : cs') =
      if null c
        then False : markFirst cs'
        else True : map (const False) cs'

colorPrefixFlagsForOrder :: Opts -> ColorScheme -> [FileInfo] -> [Int] -> [Bool]
colorPrefixFlagsForOrder opts cs files order
  | optColorMode opts == ColorNever = replicate (length files) False
  | otherwise =
      let codes = map (getFileColor cs) files
          firstIdx = findFirstColored order codes
       in [i == firstIdx | i <- [0 .. length files - 1]]
  where
    findFirstColored [] _ = -1
    findFirstColored (i : is) codes =
      case drop i codes of
        (c : _) | not (null c) -> i
        _ -> findFirstColored is codes

hyperlinkize :: Opts -> FileInfo -> String -> String
hyperlinkize opts fi label =
  let host = optHyperlinkHost opts
      targetPath = case fiLinkTargetAbs fi of
        Just t -> t
        Nothing -> fiAbsPath fi
      url = "file://" ++ host ++ percentEncode targetPath
   in "\ESC]8;;" ++ url ++ "\a" ++ label ++ "\ESC]8;;\a"

percentEncode :: String -> String
percentEncode = concatMap encodeChar
  where
    encodeChar c
      | isUnreserved c || c == '/' = [c]
      | otherwise = '%' : toHex (ord c)
    isUnreserved ch =
      isAsciiUpper ch
        || isAsciiLower ch
        || isDigit ch
        || ch `elem` "-._~"
    toHex n =
      let h = showHex n ""
       in case h of
            [a] -> ['0', a]
            [a, b] -> [a, b]
            _ -> take 2 (reverse (take 2 (reverse h)))

displayNameWidth :: Opts -> FileInfo -> Int
displayNameWidth opts fi =
  let (name, indicator, _) = formatNameParts opts fi
   in measureSortWidth (name ++ indicator)

displayEntryWidth :: Opts -> FileInfo -> Int
displayEntryWidth opts fi =
  let prefix = entryPrefix opts fi
      (name, indicator, _) = formatNameParts opts fi
   in measureDisplayWidth (optTabSize opts) (prefix ++ name ++ indicator)

measureDisplayWidth :: Int -> String -> Int
measureDisplayWidth tabSize = go 0
  where
    ts = if tabSize <= 0 then 8 else tabSize
    go col [] = col
    go col (c : cs)
      | isControl c = go col cs
      | c == '\t' =
          let next = col + (ts - (col `mod` ts))
           in go next cs
      | otherwise = go (col + 1) cs

measureSortWidth :: String -> Int
measureSortWidth = foldr (\c acc -> acc + if isControl c then 0 else 1) 0

shouldUseTabs :: Opts -> Bool
shouldUseTabs opts =
  optColorMode opts == ColorNever
    && not (optDired opts)

-- | Format mode string (e.g., "drwxr-xr-x")
formatModeString :: FileMode -> FileType -> String
formatModeString mode ftype =
  [typeChar]
    ++ [permChar ownerReadMode 'r']
    ++ [permChar ownerWriteMode 'w']
    ++ [execChar ownerExecuteMode setUserIDMode 's' 'S']
    ++ [permChar groupReadMode 'r']
    ++ [permChar groupWriteMode 'w']
    ++ [execChar groupExecuteMode setGroupIDMode 's' 'S']
    ++ [permChar otherReadMode 'r']
    ++ [permChar otherWriteMode 'w']
    ++ [execChar otherExecuteMode stickyMode 't' 'T']
  where
    typeChar = case ftype of
      TypeDirectory -> 'd'
      TypeSymlink -> 'l'
      TypeBlockDevice -> 'b'
      TypeCharDevice -> 'c'
      TypeNamedPipe -> 'p'
      TypeSocket -> 's'
      TypeDoor -> 'D'
      _ -> '-'

    permChar :: FileMode -> Char -> Char
    permChar m c = if (mode .&. m) /= 0 then c else '-'

    execChar :: FileMode -> FileMode -> Char -> Char -> Char
    execChar execBit specialBit lower upper
      | (mode .&. execBit) /= 0 && (mode .&. specialBit) /= 0 = lower
      | (mode .&. execBit) /= 0 = 'x'
      | (mode .&. specialBit) /= 0 = upper
      | otherwise = '-'

-- | Format file size (with -h for human readable)
formatSize :: Opts -> FileOffset -> String
formatSize = formatFileSize

formatFileSize :: Opts -> FileOffset -> String
formatFileSize opts size
  | optHumanReadable opts = humanReadable (optSI opts) size
  | otherwise = case effectiveSizeScale opts of
      Just (BlockSize bsBytes suffix) ->
        let n = ceilDiv (toInteger size) bsBytes
         in show n ++ suffix
      Nothing -> show size

-- | Format size in human readable format
humanReadable :: Bool -> FileOffset -> String
humanReadable useSI bytes
  | bytes < unit = show bytes
  | otherwise = go (fromIntegral bytes :: Double) suffixes
  where
    unit = if useSI then 1000 else 1024
    suffixes = if useSI then ["", "k", "M", "G", "T", "P", "E"] else ["", "K", "M", "G", "T", "P", "E"]

    go :: Double -> [String] -> String
    go n [] = show (round n :: Integer)
    go n [s] = formatNum n ++ s
    go n (s : ss)
      | n < fromIntegral unit = formatNum n ++ s
      | otherwise = go (n / fromIntegral unit) ss

    formatNum :: Double -> String
    formatNum n
      | n < 10 = showDecimalCeil 1 n
      | otherwise = show (ceiling n :: Integer)

    showDecimalCeil :: Int -> Double -> String
    showDecimalCeil places n =
      let factor = (10 :: Integer) ^ places
          scaled = ceiling (n * fromIntegral factor) :: Integer
          whole = scaled `div` factor
          frac = scaled `mod` factor
          fracStr = replicate (places - length (show frac)) '0' ++ show frac
       in show whole ++ "." ++ fracStr

-- | Format blocks count
-- GNU ls defaults to 1K blocks (st_blocks is in 512-byte units)
formatBlocks :: Opts -> FileOffset -> String
formatBlocks opts blocks
  | optHumanReadable opts = humanReadable (optSI opts) (blocks * 512)
  | otherwise = case effectiveBlockSizeForBlocks opts of
      Just (BlockSize bsBytes suffix) ->
        let n = ceilDiv (toInteger blocks * 512) bsBytes
         in show n ++ suffix
      Nothing -> show ((blocks + 1) `div` 2)

effectiveSizeScale :: Opts -> Maybe BlockSize
effectiveSizeScale = optBlockSize

effectiveBlockSizeForBlocks :: Opts -> Maybe BlockSize
effectiveBlockSizeForBlocks opts = case optBlockSize opts of
  Just bs -> Just bs
  Nothing ->
    if optKibibytes opts
      then Just (BlockSize 1024 "")
      else Nothing

ceilDiv :: Integer -> Integer -> Integer
ceilDiv a b = (a + b - 1) `div` b

-- | Format time according to --time-style
formatTime :: Opts -> TimeZone -> UTCTime -> UTCTime -> String
formatTime opts tz now = formatFileTime' now opts tz

formatFileTime :: UTCTime -> Opts -> TimeZone -> FileInfo -> String
formatFileTime now opts tz fi = formatFileTime' now opts tz (getTimeField opts fi)

formatFileTime' :: UTCTime -> Opts -> TimeZone -> UTCTime -> String
formatFileTime' now opts tz time =
  let zonedTime = utcToZonedTime tz time
      recent = isRecent now time
      fmt = DTF.formatTime defaultTimeLocale
   in case getEffectiveTimeStyle opts of
        TimeFullISO ->
          let base = fmt "%Y-%m-%d %H:%M:%S" zonedTime
              tzStr = fmt "%z" zonedTime
              frac = formatNanos time
           in base ++ "." ++ frac ++ " " ++ tzStr
        TimeLongISO -> fmt "%Y-%m-%d %H:%M" zonedTime
        TimeISO ->
          if recent
            then fmt "%m-%d %H:%M" zonedTime
            else fmt "%Y-%m-%d" zonedTime ++ " "
        TimeLocale ->
          if recent
            then fmt "%b %e %H:%M" zonedTime
            else fmt "%b %e  %Y" zonedTime
        TimeCustom f -> fmt (convertFormat f) zonedTime

formatNanos :: UTCTime -> String
formatNanos t =
  let pico = diffTimeToPicoseconds (utctDayTime t)
      nanos = (pico `div` 1000) `mod` 1000000000
      s = show nanos
   in replicate (9 - length s) '0' ++ s

isRecent :: UTCTime -> UTCTime -> Bool
isRecent now time =
  let sixMonths = 15552000 :: Integer -- 6 * 30 * 24 * 60 * 60
      futureWindow = 3600 :: Integer
      diff = diffUTCTime now time
   in diff <= fromIntegral sixMonths && diff >= fromIntegral (negate futureWindow)

getEffectiveTimeStyle :: Opts -> TimeStyle
getEffectiveTimeStyle opts
  | optFullTime opts = TimeFullISO
  | otherwise = optTimeStyle opts

-- | Convert GNU ls time format to strftime format
convertFormat :: String -> String
convertFormat = id -- For now, assume compatible format

-- | Get the appropriate time field based on --time option
getTimeField :: Opts -> FileInfo -> UTCTime
getTimeField opts fi = case optTimeMode opts of
  TimeMod -> fiModTime fi
  TimeAccess -> fiAccessTime fi
  TimeChange -> fiChangeTime fi
  TimeBirth -> case fiBirthTime fi of
    Just t -> t
    Nothing -> fiModTime fi -- Fall back to mod time if birth time unavailable

-- | Quote file name according to quoting style
quoteName :: Opts -> String -> String
quoteName opts name = case optQuotingStyle opts of
  QuoteLiteral -> if optLiteralControl opts then map hideControl name else name
  QuoteShell -> quoteShell False name
  QuoteShellAlways -> quoteShell True name
  QuoteShellEscape -> quoteShellEscape name
  QuoteC -> quoteCStyle name
  QuoteEscape -> escapeSpecial name
  QuoteLocale -> "'" ++ escapeLocale name ++ "'"
  where
    hideControl c = if isControl c then '?' else c

-- | Quote for shell if needed
quoteShell :: Bool -> String -> String
quoteShell always name
  | not always && not (needsShellQuoting name) = name
  | '\'' `elem` name = "\"" ++ escapeDoubleQuoted name ++ "\""
  | otherwise = "'" ++ name ++ "'"

-- | Shell-escape special characters
quoteShellEscape :: String -> String
quoteShellEscape name
  | not (needsShellQuoting name) && not (any isControl name) = name
  | '\'' `elem` name && not (any isControl name) = "\"" ++ escapeDoubleQuoted name ++ "\""
  | otherwise = concatMap renderSegment (splitSegments name)
  where
    renderSegment (Left safe) = "'" ++ safe ++ "'"
    renderSegment (Right ctrl) = "$'" ++ escapeControl ctrl ++ "'"

    splitSegments [] = []
    splitSegments s =
      let (safe, rest) = break isControl s
       in case rest of
            [] -> [Left safe | not (null safe)]
            (c : cs) ->
              [Left safe | not (null safe)]
                ++ [Right c]
                ++ splitSegments cs

-- | C-style quoting
quoteCStyle :: String -> String
quoteCStyle name = "\"" ++ concatMap escapeC name ++ "\""
  where
    escapeC '\\' = "\\\\"
    escapeC '"' = "\\\""
    escapeC '\n' = "\\n"
    escapeC '\t' = "\\t"
    escapeC '\r' = "\\r"
    escapeC c
      | isControl c = "\\" ++ octal3 c
      | otherwise = [c]

-- | Escape special characters with backslash
escapeSpecial :: String -> String
escapeSpecial = concatMap escape
  where
    escape c
      | c == ' ' = "\\ "
      | c == '\t' = "\\t"
      | c == '\n' = "\\n"
      | isControl c = "\\" ++ octal3 c
      | otherwise = [c]

escapeLocale :: String -> String
escapeLocale = concatMap escape
  where
    escape c
      | c == '\'' = "\\'"
      | c == '\\' = "\\\\"
      | c == '\t' = "\\t"
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | isControl c = "\\" ++ octal3 c
      | otherwise = [c]

needsShellQuoting :: String -> Bool
needsShellQuoting s = any (`elem` s) " \t\n'\"\\|&;()<>!$`*?[]#"

escapeDoubleQuoted :: String -> String
escapeDoubleQuoted = concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '$' = "\\$"
    escape '`' = "\\`"
    escape c = [c]

escapeControl :: Char -> String
escapeControl c = case c of
  '\n' -> "\\n"
  '\t' -> "\\t"
  '\r' -> "\\r"
  _ -> "\\" ++ octal3 c

octal3 :: Char -> String
octal3 c =
  let n = fromEnum c
      d1 = n `div` 64
      d2 = (n `div` 8) `mod` 8
      d3 = n `mod` 8
      digit d = toEnum (fromEnum '0' + d)
   in [digit d1, digit d2, digit d3]

-- | Get user name from UID
getUserName :: UserID -> IO String
getUserName uid = do
  result <- try (getUserEntryForID uid)
  case result of
    Right entry -> return (userName entry)
    Left (_ :: IOError) -> return (show uid)

-- | Get group name from GID
getGroupName :: GroupID -> IO String
getGroupName gid = do
  result <- try (getGroupEntryForID gid)
  case result of
    Right entry -> return (groupName entry)
    Left (_ :: IOError) -> return (show gid)

-- | Find optimal column layout for vertical (down-then-across) output
findOptimalColumns :: Int -> Int -> [Int] -> Int -> ColumnLayout
findOptimalColumns = findOptimalColumnsWith computeColumnWidthsVertical

-- | Find optimal column layout for horizontal (across-then-down) output
findOptimalColumnsHorizontal :: Int -> Int -> [Int] -> Int -> ColumnLayout
findOptimalColumnsHorizontal = findOptimalColumnsWith computeColumnWidthsHorizontal

findOptimalColumnsWith :: (Int -> Int -> [Int] -> [Int]) -> Int -> Int -> [Int] -> Int -> ColumnLayout
findOptimalColumnsWith compute termWidth gap widths maxCols
  | null widths = ColumnLayout 1 [0]
  | otherwise =
      -- Try decreasing number of columns until it fits
      let tryLayout n
            | n <= 1 = ColumnLayout 1 [maximum widths]
            | otherwise =
                let numRows = (length widths + n - 1) `div` n
                    colWidths = compute n numRows widths
                    totalWidth = sum colWidths + (gap * (n - 1))
                 in if totalWidth <= termWidth
                      then ColumnLayout n colWidths
                      else tryLayout (n - 1)
       in tryLayout maxCols

-- | Compute the width of each column for vertical layout
computeColumnWidthsVertical :: Int -> Int -> [Int] -> [Int]
computeColumnWidthsVertical numCols numRows widths =
  [ columnWidth col
  | col <- [0 .. numCols - 1]
  ]
  where
    columnWidth col =
      let startIdx = col * numRows
          endIdx = min (startIdx + numRows) (length widths)
          colWidths = take (endIdx - startIdx) (drop startIdx widths)
       in if null colWidths then 0 else maximum colWidths

-- | Compute the width of each column for horizontal layout
computeColumnWidthsHorizontal :: Int -> Int -> [Int] -> [Int]
computeColumnWidthsHorizontal numCols numRows widths =
  [ columnWidth col
  | col <- [0 .. numCols - 1]
  ]
  where
    columnWidth col =
      let idxs = [col + row * numCols | row <- [0 .. numRows - 1], col + row * numCols < length widths]
          colWidths = map (widths !!) idxs
       in if null colWidths then 0 else maximum colWidths

data ColumnLayout = ColumnLayout
  { _clNumCols :: Int,
    _clColWidths :: [Int]
  }

-- | Render columns with vertical sorting (down, then across)
renderColumnsVertical :: ColumnLayout -> [String] -> [Int] -> Int -> Bool -> Int -> [String]
renderColumnsVertical (ColumnLayout numCols widths) names nameWidths tabSize useTabs gap
  | numCols <= 1 = names
  | null names = []
  | otherwise =
      let numRows = (length names + numCols - 1) `div` numCols
          getCell row col =
            let idx = col * numRows + row
             in if idx < length names
                  then (names !! idx, nameWidths !! idx)
                  else ("", 0)
          rows =
            [ [getCell row col | col <- [0 .. numCols - 1]]
            | row <- [0 .. numRows - 1]
            ]
       in map (formatRow tabSize useTabs gap widths) rows

-- | Render columns with horizontal sorting (across, then down)
renderColumnsHorizontal :: ColumnLayout -> [String] -> [Int] -> Int -> Bool -> Int -> [String]
renderColumnsHorizontal (ColumnLayout numCols widths) names nameWidths tabSize useTabs gap
  | numCols <= 1 = names
  | null names = []
  | otherwise =
      let numRows = (length names + numCols - 1) `div` numCols
          getCell row col =
            let idx = row * numCols + col
             in if idx < length names
                  then (names !! idx, nameWidths !! idx)
                  else ("", 0)
          rows =
            [ [getCell row col | col <- [0 .. numCols - 1]]
            | row <- [0 .. numRows - 1]
            ]
       in map (formatRow tabSize useTabs gap widths) rows

-- | Format a row with proper column widths
-- Pad each column to its width, with 2 spaces between columns
-- Don't pad the last column
formatRow :: Int -> Bool -> Int -> [Int] -> [(String, Int)] -> String
formatRow tabSize useTabs gap widths cols = trimEnd $ go 0 (zip widths cols)
  where
    go _ [] = ""
    go _ [(_, (c, _))] = c
    go pos ((w, (c, cw)) : rest) =
      let pos' = pos + cw
          target = pos + w + gap
          padding = padTo tabSize useTabs pos' target
       in c ++ padding ++ go target rest
    trimEnd = reverse . dropWhile (`elem` " \t") . reverse

padTo :: Int -> Bool -> Int -> Int -> String
padTo _ _ cur target | cur >= target = ""
padTo tabSize useTabs cur target
  | not useTabs = replicate (target - cur) ' '
  | tabSize <= 0 = replicate (target - cur) ' '
  | otherwise = go cur
  where
    ts = tabSize
    go pos
      | pos >= target = ""
      | otherwise =
          if target `div` ts > (pos + 1) `div` ts
            then
              let nextTab = pos + (ts - (pos `mod` ts))
               in '\t' : go nextTab
            else ' ' : go (pos + 1)

-- | Wrap text to fit terminal width
wrapText :: Int -> String -> [String]
wrapText _ [] = []
wrapText width text = go text
  where
    go [] = []
    go s
      | length s <= width = [s]
      | otherwise =
          let (line, rest) = splitAt width s
              -- Find last space to break at
              (line', rest') = case break (== ' ') (reverse line) of
                (_, []) -> (line, rest)
                (end, ' ' : start) -> (reverse start, reverse end ++ rest)
                _ -> (line, rest)
           in line' : go (dropWhile (== ' ') rest')

-- Helper functions

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

maxWidth :: [String] -> Int
maxWidth [] = 0
maxWidth xs = maximum (map length xs)
