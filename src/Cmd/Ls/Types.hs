{-# LANGUAGE StrictData #-}

module Cmd.Ls.Types
  ( Opts (..),
    defaultOpts,
    SortMode (..),
    TimeMode (..),
    OutputFormat (..),
    IndicatorStyle (..),
    IndicatorWhen (..),
    ColorMode (..),
    DereferenceMode (..),
    QuotingStyle (..),
    TimeStyle (..),
    HyperlinkMode (..),
    BlockSize (..),
    FileInfo (..),
    FileType (..),
  )
where

import Data.Time.Clock (UTCTime)
import System.Posix.Types (DeviceID, FileMode, FileOffset, GroupID, LinkCount, UserID)

data Opts = Opts
  { -- Display options

    -- | -a: show hidden files (including . and ..)
    optAll :: Bool,
    -- | -A: show hidden files except . and ..
    optAlmostAll :: Bool,
    -- | -l: long listing format
    optLongFormat :: Bool,
    -- | -1: one file per line
    optOnePerLine :: Bool,
    -- | -R: recursive listing
    optRecursive :: Bool,
    -- | -d: list directories themselves, not contents
    optDirectory :: Bool,
    -- | -h: human readable sizes (1K, 234M)
    optHumanReadable :: Bool,
    -- | --si: use powers of 1000 not 1024
    optSI :: Bool,
    -- | -i: show inode number
    optInode :: Bool,
    -- | -s: show allocated blocks
    optSize :: Bool,
    -- | -n: numeric uid/gid
    optNumericUID :: Bool,
    -- | -G: hide group in long format
    optNoGroup :: Bool,
    -- | -g: hide owner (but imply -l and show group)
    optHideOwner :: Bool,
    -- | -o: hide group (but imply -l and show owner)
    optHideGroup :: Bool,
    -- | --author: show author in long format
    optAuthor :: Bool,
    -- | -F/-p/--classify/--file-type/--indicator-style
    optIndicator :: IndicatorStyle,
    -- | --classify=WHEN (auto/always)
    optIndicatorWhen :: IndicatorWhen,
    -- | --sort, -S, -t, -X, -v, -U
    optSortMode :: SortMode,
    -- | --time, -u, -c
    optTimeMode :: TimeMode,
    -- | --time-style
    optTimeStyle :: TimeStyle,
    -- | --color
    optColorMode :: ColorMode,
    -- | --quoting-style, -Q, -N, -b
    optQuotingStyle :: QuotingStyle,
    -- | -H, -L, --dereference-command-line*
    optDereferenceMode :: DereferenceMode,
    -- | -I PATTERN: patterns to ignore
    optIgnorePatterns :: [String],
    -- | --hide=PATTERN: patterns to hide (overridden by -a/-A)
    optHidePatterns :: [String],
    -- | -w WIDTH: explicit terminal width
    optWidth :: Maybe Int,
    -- | -T COLS: tab size
    optTabSize :: Int,
    -- | -r: reverse sort order
    optReverse :: Bool,
    -- | --group-directories-first
    optGroupDirsFirst :: Bool,
    -- | -B: ignore *~ files
    optIgnoreBackups :: Bool,
    -- | -C, -x, -m, -l, -1
    optFormat :: OutputFormat,
    -- | --help
    optShowHelp :: Bool,
    -- | --version
    optShowVersion :: Bool,
    -- | --block-size=SIZE
    optBlockSize :: Maybe BlockSize,
    -- | --hyperlink
    optHyperlink :: HyperlinkMode,
    -- | resolved hostname for hyperlinks
    optHyperlinkHost :: String,
    -- | --zero: NUL terminated output
    optZero :: Bool,
    -- | -D/--dired: generate dired output
    optDired :: Bool,
    -- | -Z: show SELinux security context
    optContext :: Bool,
    -- | --full-time: like -l --time-style=full-iso
    optFullTime :: Bool,
    -- | -k: use 1024-byte blocks
    optKibibytes :: Bool,
    -- | -q: print ? for control chars
    optLiteralControl :: Bool,
    -- | --show-control-chars: show control chars as-is
    optShowControl :: Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optAll = False,
      optAlmostAll = False,
      optLongFormat = False,
      optOnePerLine = False,
      optRecursive = False,
      optDirectory = False,
      optHumanReadable = False,
      optSI = False,
      optInode = False,
      optSize = False,
      optNumericUID = False,
      optNoGroup = False,
      optHideOwner = False,
      optHideGroup = False,
      optAuthor = False,
      optIndicator = IndicatorNone,
      optIndicatorWhen = IndicatorAlways,
      optSortMode = SortByName,
      optTimeMode = TimeMod,
      optTimeStyle = TimeLocale,
      optColorMode = ColorNever,
      optQuotingStyle = QuoteLiteral,
      optDereferenceMode = DerefNever,
      optIgnorePatterns = [],
      optHidePatterns = [],
      optWidth = Nothing,
      optTabSize = 8,
      optReverse = False,
      optGroupDirsFirst = False,
      optIgnoreBackups = False,
      optFormat = FormatDefault,
      optShowHelp = False,
      optShowVersion = False,
      optBlockSize = Nothing,
      optHyperlink = HyperlinkNever,
      optHyperlinkHost = "",
      optZero = False,
      optDired = False,
      optContext = False,
      optFullTime = False,
      optKibibytes = False,
      optLiteralControl = False,
      optShowControl = False
    }

data SortMode
  = SortByName
  | SortBySize
  | SortByTime
  | SortByVersion
  | SortByExtension
  | SortByWidth
  | SortNone
  deriving (Eq, Show)

data BlockSize = BlockSize
  { bsBytes :: Integer,
    bsSuffix :: String
  }
  deriving (Eq, Show)

data TimeMode
  = -- | modification time (default)
    TimeMod
  | -- | -u: access time (atime)
    TimeAccess
  | -- | -c: status change time (ctime)
    TimeChange
  | -- | --time=birth: creation time
    TimeBirth
  deriving (Eq, Show)

data OutputFormat
  = -- | Default: columns on TTY, one-per-line otherwise
    FormatDefault
  | -- | -l: long format
    FormatLong
  | -- | -C: columns, sorted down (explicit)
    FormatColumns
  | -- | -x: columns, sorted across
    FormatAcross
  | -- | -m: comma separated
    FormatCommas
  | -- | -1: one per line
    FormatSingleColumn
  deriving (Eq, Show)

data IndicatorStyle
  = -- | no indicators
    IndicatorNone
  | -- | -p: / for directories
    IndicatorSlash
  | -- | --file-type: all except * for executables
    IndicatorFileType
  | -- | -F/--classify: all indicators
    IndicatorClassify
  deriving (Eq, Show)

data IndicatorWhen
  = IndicatorAlways
  | IndicatorAuto
  deriving (Eq, Show)

data ColorMode
  = ColorNever
  | ColorAlways
  | -- | auto: color if stdout is a terminal
    ColorAuto
  deriving (Eq, Show)

data DereferenceMode
  = -- | never follow symlinks
    DerefNever
  | -- | -H: follow symlinks on command line
    DerefCmdLine
  | -- | follow cmdline symlinks to dirs
    DerefCmdLineDir
  | -- | -L: always follow symlinks
    DerefAlways
  deriving (Eq, Show)

data QuotingStyle
  = -- | -N/--literal: no quoting
    QuoteLiteral
  | -- | shell: quote when needed
    QuoteShell
  | -- | shell-always: always quote
    QuoteShellAlways
  | -- | shell-escape: escape special chars
    QuoteShellEscape
  | -- | -Q/--quote-name: C-style quotes
    QuoteC
  | -- | -b: escape special chars
    QuoteEscape
  | -- | locale: quote with locale chars
    QuoteLocale
  deriving (Eq, Show)

data TimeStyle
  = -- | full-iso: 2023-01-15 10:30:45.123456789 +0000
    TimeFullISO
  | -- | long-iso: 2023-01-15 10:30
    TimeLongISO
  | -- | iso: 01-15 10:30 or 2023-01-15
    TimeISO
  | -- | locale: Jan 15 10:30 or Jan 15  2023
    TimeLocale
  | -- | +FORMAT: strftime format string
    TimeCustom String
  deriving (Eq, Show)

data HyperlinkMode
  = HyperlinkNever
  | HyperlinkAlways
  | HyperlinkAuto
  deriving (Eq, Show)

data FileInfo = FileInfo
  { -- | file name (without path)
    fiName :: String,
    -- | full path to file
    fiPath :: FilePath,
    -- | absolute path to file
    fiAbsPath :: FilePath,
    -- | file type
    fiType :: FileType,
    -- | permissions
    fiMode :: FileMode,
    -- | number of hard links
    fiLinks :: LinkCount,
    -- | owner user id
    fiOwner :: UserID,
    -- | owner group id
    fiGroup :: GroupID,
    -- | file size in bytes
    fiSize :: FileOffset,
    -- | allocated blocks
    fiBlocks :: FileOffset,
    -- | modification time
    fiModTime :: UTCTime,
    -- | access time
    fiAccessTime :: UTCTime,
    -- | status change time
    fiChangeTime :: UTCTime,
    -- | inode number
    fiInode :: FileOffset,
    -- | device id
    fiDevice :: DeviceID,
    -- | symlink target (if symlink)
    fiLinkTarget :: Maybe String,
    -- | absolute symlink target (if known)
    fiLinkTargetAbs :: Maybe FilePath,
    -- | does symlink target point to a directory?
    fiLinkTargetIsDir :: Bool,
    -- | is this a broken symlink?
    fiLinkBroken :: Bool,
    -- | is this file executable?
    fiExecutable :: Bool
  }
  deriving (Eq, Show)

data FileType
  = TypeRegular
  | TypeDirectory
  | TypeSymlink
  | TypeBlockDevice
  | TypeCharDevice
  | TypeNamedPipe
  | TypeSocket
  | -- | Solaris door (treat as socket on other systems)
    TypeDoor
  | TypeUnknown
  deriving (Eq, Show)
