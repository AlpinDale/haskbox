module Cmd.Ls.Color
  ( ColorScheme,
    defaultColorScheme,
    noColorScheme,
    parseLsColors,
    colorize,
    getFileColor,
    resetCode,
  )
where

import Cmd.Ls.Types
import Data.Bits ((.&.))
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Posix.Files (otherWriteMode, setGroupIDMode, setUserIDMode)
import System.Posix.Types (FileMode)

stickyMode :: FileMode
stickyMode = 0o1000

data ColorScheme = ColorScheme
  { -- | no = normal text
    csNormal :: String,
    -- | fi = regular file
    csFile :: String,
    -- | di = directory
    csDirectory :: String,
    -- | ln = symbolic link
    csSymlink :: String,
    -- | mh = multi-hardlink
    csMultiHardLink :: String,
    -- | pi = named pipe (FIFO)
    csPipe :: String,
    -- | so = socket
    csSocket :: String,
    -- | do = door (Solaris)
    csDoor :: String,
    -- | bd = block device
    csBlockDevice :: String,
    -- | cd = character device
    csCharDevice :: String,
    -- | or = orphan symlink
    csOrphan :: String,
    -- | mi = missing file (target of orphan)
    csMissing :: String,
    -- | su = setuid (u+s)
    csSetUID :: String,
    -- | sg = setgid (g+s)
    csSetGID :: String,
    -- | ca = file with capability
    csCapability :: String,
    -- | st = sticky bit (+t)
    csSticky :: String,
    -- | tw = other-writable + sticky
    csOtherWritableSticky :: String,
    -- | ow = other-writable
    csOtherWritable :: String,
    -- | ex = executable
    csExecutable :: String,
    -- | *.ext = extension colors
    csExtensions :: Map String String
  }
  deriving (Show)

defaultColorScheme :: ColorScheme
defaultColorScheme =
  ColorScheme
    { csNormal = "",
      csFile = "",
      csDirectory = "01;34", -- bold blue
      csSymlink = "01;36", -- bold cyan
      csMultiHardLink = "",
      csPipe = "33", -- yellow
      csSocket = "01;35", -- bold magenta
      csDoor = "01;35", -- bold magenta
      csBlockDevice = "01;33", -- bold yellow
      csCharDevice = "01;33", -- bold yellow
      csOrphan = "", -- use symlink color unless explicitly set
      csMissing = "",
      csSetUID = "37;41", -- white on red
      csSetGID = "30;43", -- black on yellow
      csCapability = "30;41", -- black on red
      csSticky = "37;44", -- white on blue
      csOtherWritableSticky = "30;42", -- black on green
      csOtherWritable = "34;42", -- blue on green
      csExecutable = "01;32", -- bold green
      csExtensions = defaultExtensions
    }

noColorScheme :: ColorScheme
noColorScheme =
  ColorScheme
    { csNormal = "",
      csFile = "",
      csDirectory = "",
      csSymlink = "",
      csMultiHardLink = "",
      csPipe = "",
      csSocket = "",
      csDoor = "",
      csBlockDevice = "",
      csCharDevice = "",
      csOrphan = "",
      csMissing = "",
      csSetUID = "",
      csSetGID = "",
      csCapability = "",
      csSticky = "",
      csOtherWritableSticky = "",
      csOtherWritable = "",
      csExecutable = "",
      csExtensions = Map.empty
    }

defaultExtensions :: Map String String
defaultExtensions =
  Map.fromList
    [ -- Archives (bold red)
      (".tar", "01;31"),
      (".tgz", "01;31"),
      (".arc", "01;31"),
      (".arj", "01;31"),
      (".taz", "01;31"),
      (".lha", "01;31"),
      (".lz4", "01;31"),
      (".lzh", "01;31"),
      (".lzma", "01;31"),
      (".tlz", "01;31"),
      (".txz", "01;31"),
      (".tzo", "01;31"),
      (".t7z", "01;31"),
      (".zip", "01;31"),
      (".z", "01;31"),
      (".dz", "01;31"),
      (".gz", "01;31"),
      (".lrz", "01;31"),
      (".lz", "01;31"),
      (".lzo", "01;31"),
      (".xz", "01;31"),
      (".zst", "01;31"),
      (".tzst", "01;31"),
      (".bz2", "01;31"),
      (".bz", "01;31"),
      (".tbz", "01;31"),
      (".tbz2", "01;31"),
      (".tz", "01;31"),
      (".deb", "01;31"),
      (".rpm", "01;31"),
      (".jar", "01;31"),
      (".war", "01;31"),
      (".ear", "01;31"),
      (".sar", "01;31"),
      (".rar", "01;31"),
      (".alz", "01;31"),
      (".ace", "01;31"),
      (".zoo", "01;31"),
      (".cpio", "01;31"),
      (".7z", "01;31"),
      (".rz", "01;31"),
      (".cab", "01;31"),
      (".wim", "01;31"),
      (".swm", "01;31"),
      (".dwm", "01;31"),
      (".esd", "01;31"),
      -- Images (bold magenta)
      (".jpg", "01;35"),
      (".jpeg", "01;35"),
      (".mjpg", "01;35"),
      (".mjpeg", "01;35"),
      (".gif", "01;35"),
      (".bmp", "01;35"),
      (".pbm", "01;35"),
      (".pgm", "01;35"),
      (".ppm", "01;35"),
      (".tga", "01;35"),
      (".xbm", "01;35"),
      (".xpm", "01;35"),
      (".tif", "01;35"),
      (".tiff", "01;35"),
      (".png", "01;35"),
      (".svg", "01;35"),
      (".svgz", "01;35"),
      (".mng", "01;35"),
      (".pcx", "01;35"),
      (".webp", "01;35"),
      (".ico", "01;35"),
      -- Video (bold magenta)
      (".mov", "01;35"),
      (".mpg", "01;35"),
      (".mpeg", "01;35"),
      (".m2v", "01;35"),
      (".mkv", "01;35"),
      (".webm", "01;35"),
      (".ogm", "01;35"),
      (".mp4", "01;35"),
      (".m4v", "01;35"),
      (".mp4v", "01;35"),
      (".vob", "01;35"),
      (".qt", "01;35"),
      (".nuv", "01;35"),
      (".wmv", "01;35"),
      (".asf", "01;35"),
      (".rm", "01;35"),
      (".rmvb", "01;35"),
      (".flc", "01;35"),
      (".avi", "01;35"),
      (".fli", "01;35"),
      (".flv", "01;35"),
      (".gl", "01;35"),
      (".dl", "01;35"),
      (".xcf", "01;35"),
      (".xwd", "01;35"),
      (".yuv", "01;35"),
      (".cgm", "01;35"),
      (".emf", "01;35"),
      (".ogv", "01;35"),
      (".ogx", "01;35"),
      -- Audio (cyan)
      (".aac", "00;36"),
      (".au", "00;36"),
      (".flac", "00;36"),
      (".m4a", "00;36"),
      (".mid", "00;36"),
      (".midi", "00;36"),
      (".mka", "00;36"),
      (".mp3", "00;36"),
      (".mpc", "00;36"),
      (".ogg", "00;36"),
      (".ra", "00;36"),
      (".wav", "00;36"),
      (".oga", "00;36"),
      (".opus", "00;36"),
      (".spx", "00;36"),
      (".xspf", "00;36")
    ]

parseLsColors :: String -> ColorScheme
parseLsColors str = foldr applyEntry defaultColorScheme entries
  where
    entries = map parseEntry $ splitOn ':' str

    parseEntry :: String -> (String, String)
    parseEntry s = case break (== '=') s of
      (key, '=' : val) -> (key, val)
      (key, _) -> (key, "")

    applyEntry :: (String, String) -> ColorScheme -> ColorScheme
    applyEntry (key, val) cs = case key of
      "no" -> cs {csNormal = val}
      "fi" -> cs {csFile = val}
      "di" -> cs {csDirectory = val}
      "ln" -> cs {csSymlink = val}
      "mh" -> cs {csMultiHardLink = val}
      "pi" -> cs {csPipe = val}
      "so" -> cs {csSocket = val}
      "do" -> cs {csDoor = val}
      "bd" -> cs {csBlockDevice = val}
      "cd" -> cs {csCharDevice = val}
      "or" -> cs {csOrphan = val}
      "mi" -> cs {csMissing = val}
      "su" -> cs {csSetUID = val}
      "sg" -> cs {csSetGID = val}
      "ca" -> cs {csCapability = val}
      "st" -> cs {csSticky = val}
      "tw" -> cs {csOtherWritableSticky = val}
      "ow" -> cs {csOtherWritable = val}
      "ex" -> cs {csExecutable = val}
      '*' : ext -> cs {csExtensions = Map.insert (map toLower ext) val (csExtensions cs)}
      _ -> cs

    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn c s = case break (== c) s of
      (a, _ : b) -> a : splitOn c b
      (a, []) -> [a]

getFileColor :: ColorScheme -> FileInfo -> String
getFileColor cs fi
  | fiType fi == TypeRegular && isSetUID = csSetUID cs
  | fiType fi == TypeRegular && isSetGID = csSetGID cs
  | fiType fi == TypeDirectory && isSticky && isOtherWritable = csOtherWritableSticky cs
  | fiType fi == TypeDirectory && isOtherWritable = csOtherWritable cs
  | fiType fi == TypeDirectory && isSticky = csSticky cs
  | fiType fi == TypeSymlink && fiLinkBroken fi =
      if null (csOrphan cs) then csSymlink cs else csOrphan cs
  | otherwise = case fiType fi of
      TypeDirectory -> csDirectory cs
      TypeSymlink -> csSymlink cs
      TypeNamedPipe -> csPipe cs
      TypeSocket -> csSocket cs
      TypeDoor -> csDoor cs
      TypeBlockDevice -> csBlockDevice cs
      TypeCharDevice -> csCharDevice cs
      TypeRegular
        | fiExecutable fi -> csExecutable cs
        | otherwise -> getExtColor cs (fiName fi)
      TypeUnknown -> csFile cs
  where
    mode = fiMode fi
    isSetUID = (mode .&. setUserIDMode) /= 0
    isSetGID = (mode .&. setGroupIDMode) /= 0
    isSticky = (mode .&. stickyMode) /= 0
    isOtherWritable = (mode .&. otherWriteMode) /= 0

getExtColor :: ColorScheme -> String -> String
getExtColor cs name =
  case getExtension name of
    Just ext -> Map.findWithDefault (csFile cs) (map toLower ext) (csExtensions cs)
    Nothing -> csFile cs
  where
    getExtension :: String -> Maybe String
    getExtension n = case dropWhile (/= '.') n of
      ext@('.' : _) -> Just ext
      _ -> Nothing

colorize :: String -> String -> String
colorize code str
  | null code = str
  | otherwise = "\ESC[" ++ code ++ "m" ++ str ++ resetCode

resetCode :: String
resetCode = "\ESC[0m"
