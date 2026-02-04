{-# LANGUAGE StrictData #-}

module Cmd.Df.Types
  ( Opts (..),
    defaultOpts,
    Field (..),
    allFields,
    fieldName,
    FsInfo (..),
  )
where

data Opts = Opts
  { -- | -h: human readable (powers of 1024)
    optHumanReadable :: Bool,
    -- | -H/--si: SI units (powers of 1000)
    optSI :: Bool,
    -- | -B/--block-size: block size
    optBlockSize :: Maybe Integer,
    -- | -P: POSIX format
    optPosix :: Bool,
    -- | -T: show filesystem type
    optPrintType :: Bool,
    -- | -i: show inode info
    optInodes :: Bool,
    -- | -a: include pseudo filesystems
    optAll :: Bool,
    -- | -l: local filesystems only
    optLocal :: Bool,
    -- | -t: include only these types
    optTypes :: [String],
    -- | -x: exclude these types
    optExcludeTypes :: [String],
    -- | --total: print grand total
    optTotal :: Bool,
    -- | --output: custom field list
    optOutput :: Maybe [Field],
    -- | --sync: sync before reading
    optSync :: Bool,
    -- | --help
    optShowHelp :: Bool,
    -- | --version
    optShowVersion :: Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optHumanReadable = False,
      optSI = False,
      optBlockSize = Nothing,
      optPosix = False,
      optPrintType = False,
      optInodes = False,
      optAll = False,
      optLocal = False,
      optTypes = [],
      optExcludeTypes = [],
      optTotal = False,
      optOutput = Nothing,
      optSync = False,
      optShowHelp = False,
      optShowVersion = False
    }

-- | Display fields for --output option
data Field
  = -- | Filesystem device
    FieldSource
  | -- | Filesystem type
    FieldFstype
  | -- | Total size
    FieldSize
  | -- | Used space
    FieldUsed
  | -- | Available space
    FieldAvail
  | -- | Use percentage
    FieldPcent
  | -- | Total inodes
    FieldItotal
  | -- | Used inodes
    FieldIused
  | -- | Available inodes
    FieldIavail
  | -- | Inode percentage
    FieldIpcent
  | -- | Mount point
    FieldTarget
  | -- | File argument
    FieldFile
  deriving (Eq, Show)

allFields :: [Field]
allFields =
  [ FieldSource,
    FieldFstype,
    FieldItotal,
    FieldIused,
    FieldIavail,
    FieldIpcent,
    FieldSize,
    FieldUsed,
    FieldAvail,
    FieldPcent,
    FieldFile,
    FieldTarget
  ]

fieldName :: Field -> String
fieldName FieldSource = "source"
fieldName FieldFstype = "fstype"
fieldName FieldSize = "size"
fieldName FieldUsed = "used"
fieldName FieldAvail = "avail"
fieldName FieldPcent = "pcent"
fieldName FieldItotal = "itotal"
fieldName FieldIused = "iused"
fieldName FieldIavail = "iavail"
fieldName FieldIpcent = "ipcent"
fieldName FieldTarget = "target"
fieldName FieldFile = "file"

-- | Information about a mounted filesystem
data FsInfo = FsInfo
  { -- | Device path (e.g., /dev/sda1)
    fsDevice :: String,
    -- | Filesystem type (e.g., ext4, apfs)
    fsType :: String,
    -- | Mount point (e.g., /home)
    fsMountPoint :: String,
    -- | Block size in bytes
    fsBlockSize :: Integer,
    -- | Total blocks
    fsTotalBlocks :: Integer,
    -- | Free blocks (including reserved)
    fsFreeBlocks :: Integer,
    -- | Available blocks (to non-root)
    fsAvailBlocks :: Integer,
    -- | Total inodes
    fsTotalInodes :: Integer,
    -- | Free inodes
    fsFreeInodes :: Integer
  }
  deriving (Show)
