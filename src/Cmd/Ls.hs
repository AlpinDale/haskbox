module Cmd.Ls (run) where

import Cmd.Ls.Color
import Cmd.Ls.Filter
import Cmd.Ls.Format
import Cmd.Ls.Sort
import Cmd.Ls.Stat (getBirthTime)
import Cmd.Ls.Types
import Control.Exception (IOException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Bits ((.&.))
import Data.Char (isDigit, toUpper)
import Data.Either (partitionEithers)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import System.Directory (getDirectoryContents, makeAbsolute)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.Posix.Files
  ( FileStatus,
    accessTimeHiRes,
    deviceID,
    fileBlocks,
    fileGroup,
    fileID,
    fileMode,
    fileOwner,
    fileSize,
    getFileStatus,
    getSymbolicLinkStatus,
    groupExecuteMode,
    isBlockDevice,
    isCharacterDevice,
    isDirectory,
    isNamedPipe,
    isRegularFile,
    isSocket,
    isSymbolicLink,
    linkCount,
    modificationTimeHiRes,
    otherExecuteMode,
    ownerExecuteMode,
    readSymbolicLink,
    statusChangeTimeHiRes,
  )
import System.Posix.Types (DeviceID, FileOffset)
import System.Posix.Unistd (SystemID (..), getSystemID)
import Text.Read (readMaybe)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox ls: " ++ err
    hPutStrLn stderr "Try 'haskbox ls --help' for more information."
    exitFailure
  Right (opts, paths)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "ls") >> exitSuccess
    | otherwise -> do
        opts' <- adjustForTerminal opts
        host <-
          if optHyperlink opts' /= HyperlinkNever
            then nodeName <$> getSystemID
            else return ""
        let opts'' = opts' {optHyperlinkHost = host}
        colorScheme <- getColorScheme opts''
        tz <- getCurrentTimeZone
        termWidth <- maybe getTerminalWidth return (optWidth opts'')

        let targets = if null paths then ["."] else paths

        exitCode <-
          if optDired opts''
            then processTargetsDired opts'' colorScheme tz termWidth targets
            else processTargets opts'' colorScheme tz termWidth targets
        exitWith exitCode

adjustForTerminal :: Opts -> IO Opts
adjustForTerminal opts = do
  isTTY <- hIsTerminalDevice stdout
  return $
    if isTTY
      then
        opts
          { optFormat =
              if optLongFormat opts || optHideOwner opts || optHideGroup opts
                then FormatLong
                else
                  if optOnePerLine opts
                    then FormatSingleColumn
                    else case optFormat opts of
                      FormatDefault -> FormatColumns -- TTY default is columns
                      other -> other,
            optColorMode =
              if optColorMode opts == ColorAuto
                then ColorAlways
                else optColorMode opts,
            optIndicator = optIndicator opts,
            optHyperlink =
              if optHyperlink opts == HyperlinkAuto
                then HyperlinkAlways
                else optHyperlink opts,
            optLiteralControl =
              -- Default to hiding control chars on terminal
              not (optShowControl opts)
          }
      else
        opts
          { optFormat =
              if optLongFormat opts || optHideOwner opts || optHideGroup opts
                then FormatLong
                else
                  if optOnePerLine opts
                    then FormatSingleColumn
                    else case optFormat opts of
                      FormatDefault -> FormatSingleColumn -- Non-TTY default is one-per-line
                      other -> other, -- Preserve explicit format choice
            optColorMode =
              if optColorMode opts == ColorAuto
                then ColorNever
                else optColorMode opts,
            optIndicator =
              if optIndicatorWhen opts == IndicatorAuto
                then IndicatorNone
                else optIndicator opts,
            optHyperlink =
              if optHyperlink opts == HyperlinkAuto
                then HyperlinkNever
                else optHyperlink opts
          }

getColorScheme :: Opts -> IO ColorScheme
getColorScheme opts
  | optColorMode opts == ColorNever = return noColorScheme
  | otherwise = do
      mColors <- lookupEnv "LS_COLORS"
      return $ maybe defaultColorScheme parseLsColors mColors

setSeverity :: IORef Int -> Int -> IO ()
setSeverity ref sev = modifyIORef' ref (`max` sev)

processTargets :: Opts -> ColorScheme -> TimeZone -> Int -> [FilePath] -> IO ExitCode
processTargets opts cs tz termWidth targets = do
  statusRef <- newIORef 0

  -- Classify each target as either a file entry or a directory to list
  -- This depends on -d flag and dereference mode
  classified <- forM targets $ \path -> do
    result <- try $ getSymbolicLinkStatus path
    case result of
      Left (_ :: IOException) -> do
        hPutStrLn stderr $ "haskbox ls: cannot access '" ++ path ++ "': No such file or directory"
        setSeverity statusRef 2
        return Nothing
      Right stat -> do
        -- Determine if this is a directory we should descend into
        shouldDescend <- classifyTarget opts path stat
        return $ Just (path, shouldDescend)

  let validTargets = catMaybes classified
      (files, dirs) = partitionByDescend validTargets

  -- First list any files given on command line (including dirs with -d)
  unless (null files) $ do
    results <- mapM (getFileInfoForCmdLine opts) files
    let (errors, fileInfos) = partitionEithers results
    unless (null errors) $ setSeverity statusRef 2
    unless (null fileInfos) $ do
      let sorted = sortFiles opts fileInfos
      outputFiles opts cs tz termWidth False sorted

  -- Then list directories (those we're descending into)
  visited <- newIORef Set.empty
  let multipleTargets = length targets > 1

  forM_ (zip [0 :: Int ..] dirs) $ \(i, dir) -> do
    -- Print blank line between entries (except before first)
    when (i > 0 || not (null files)) $ putStrLn ""

    -- Print directory name if multiple targets or recursive
    when (multipleTargets || optRecursive opts) $
      putStrLn $
        dir ++ ":"

    result <- listDirectory opts cs tz termWidth visited statusRef dir
    case result of
      Left err -> do
        hPutStrLn stderr $ "haskbox ls: cannot access '" ++ dir ++ "': " ++ err
        setSeverity statusRef 2
      Right () -> return ()

  status <- readIORef statusRef
  return $ case status of
    0 -> ExitSuccess
    1 -> ExitFailure 1
    _ -> ExitFailure 2

data DiredState = DiredState
  { dsLines :: [String],
    dsOffsets :: [(Int, Int)],
    dsSubOffsets :: [(Int, Int)],
    dsPos :: Int,
    dsHadError :: Bool
  }

emptyDiredState :: DiredState
emptyDiredState = DiredState [] [] [] 0 False

appendLineD :: String -> DiredState -> DiredState
appendLineD line st =
  let pos' = dsPos st + length line + 1
   in st {dsLines = line : dsLines st, dsPos = pos'}

appendOffsetD :: (Int, Int) -> DiredState -> DiredState
appendOffsetD off st = st {dsOffsets = off : dsOffsets st}

appendSubOffsetD :: (Int, Int) -> DiredState -> DiredState
appendSubOffsetD off st = st {dsSubOffsets = off : dsSubOffsets st}

setErrorD :: DiredState -> DiredState
setErrorD st = st {dsHadError = True}

diredIndent :: String
diredIndent = "  "

processTargetsDired :: Opts -> ColorScheme -> TimeZone -> Int -> [FilePath] -> IO ExitCode
processTargetsDired opts cs tz termWidth targets = do
  let initial = emptyDiredState
  statusRef <- newIORef 0

  -- Classify each target as either a file entry or a directory to list
  classified <- forM targets $ \path -> do
    result <- try $ getSymbolicLinkStatus path
    case result of
      Left (_ :: IOException) -> do
        setSeverity statusRef 2
        return (Nothing, Just $ "haskbox ls: cannot access '" ++ path ++ "': No such file or directory")
      Right stat -> do
        shouldDescend <- classifyTarget opts path stat
        return (Just (path, shouldDescend), Nothing)

  let validTargets = catMaybes [c | (c, _) <- classified]
      errors = [e | (_, Just e) <- classified]
      (files, dirs) = partitionByDescend validTargets

  let st1 = foldr (\e st -> appendLineD e (setErrorD st)) initial errors

  -- First list any files given on command line (including dirs with -d)
  st2 <-
    if null files
      then return st1
      else do
        results <- mapM (getFileInfoForCmdLine opts) files
        let (cmdErrors, fileInfos) = partitionEithers results
            stErr = if null cmdErrors then st1 else setErrorD st1
        unless (null cmdErrors) $ setSeverity statusRef 2
        outputFilesDired opts cs tz termWidth False fileInfos stErr

  visited <- newIORef Set.empty
  let multipleTargets = length targets > 1

  let hadFiles = not (null files)
  stFinal <- foldM (\st (i, dir) -> listDirectoryDired opts cs tz termWidth visited statusRef st i multipleTargets hadFiles True dir) st2 (zip [0 :: Int ..] dirs)

  let outLines = reverse (dsLines stFinal)
      diredOffsets = reverse (dsOffsets stFinal)
      subOffsets = reverse (dsSubOffsets stFinal)
      quoting = quotingStyleName (optQuotingStyle opts)
      diredLine = "//DIRED// " ++ unwords [show a ++ " " ++ show b | (a, b) <- diredOffsets]
      subLine =
        if optRecursive opts
          then Just ("//SUBDIRED// " ++ unwords [show a ++ " " ++ show b | (a, b) <- subOffsets])
          else Nothing
      diredOptsLine = "//DIRED-OPTIONS// --quoting-style=" ++ quoting
      finalLines = outLines ++ [diredLine] ++ maybeToList subLine ++ [diredOptsLine]

  mapM_ putStrLn finalLines

  status <- readIORef statusRef
  return $ case status of
    0 -> ExitSuccess
    1 -> ExitFailure 1
    _ -> ExitFailure 2

listDirectoryDired ::
  Opts ->
  ColorScheme ->
  TimeZone ->
  Int ->
  IORef (Set (DeviceID, FileOffset)) ->
  IORef Int ->
  DiredState ->
  Int ->
  Bool ->
  Bool ->
  Bool ->
  FilePath ->
  IO DiredState
listDirectoryDired opts cs tz termWidth visited statusRef st idx multipleTargets hadFiles isCmdLineDir dir = do
  -- Print blank line between entries (except before first)
  let st1 =
        if idx > 0 || hadFiles
          then appendLineD "" st
          else st

  -- Print directory name if multiple targets or recursive
  st2 <-
    if multipleTargets || optRecursive opts
      then do
        let header = diredIndent ++ dir ++ ":"
            start = dsPos st1 + length diredIndent
            end = start + length dir
            st1' = appendLineD header st1
        return $
          if optRecursive opts
            then appendSubOffsetD (start, end) st1'
            else st1'
      else return st1

  result <- try $ getDirectoryContents dir
  case result of
    Left (e :: IOException) -> do
      let errLine = "haskbox ls: cannot access '" ++ dir ++ "': " ++ friendlyError (show e)
      setSeverity statusRef (if isCmdLineDir then 2 else 1)
      return $ appendLineD errLine (setErrorD st2)
    Right contents -> do
      let filtered = filter (shouldShow opts) contents
      entries <- mapM (getFileInfo opts . (dir </>)) filtered
      let (errors, fileInfos) = partitionEithers entries
          stErr = if null errors then st2 else setErrorD st2
      unless (null errors) $ setSeverity statusRef 1
      let sorted = sortFiles opts fileInfos
      st3 <- outputFilesDired opts cs tz termWidth True sorted stErr

      if optRecursive opts
        then do
          let subdirs = [fi | fi <- sorted, fiType fi == TypeDirectory, fiName fi `notElem` [".", ".."]]
          foldM
            ( \stAcc subdir -> do
                let subpath = fiPath subdir
                    key = (fiDevice subdir, fiInode subdir)
                visitedSet <- readIORef visited
                if Set.member key visitedSet
                  then return stAcc
                  else do
                    writeIORef visited (Set.insert key visitedSet)
                    let stAcc' = appendLineD "" stAcc
                    listDirectoryDired opts cs tz termWidth visited statusRef stAcc' 0 True False False subpath
            )
            st3
            subdirs
        else return st3

outputFilesDired :: Opts -> ColorScheme -> TimeZone -> Int -> Bool -> [FileInfo] -> DiredState -> IO DiredState
outputFilesDired opts cs tz _termWidth showTotal files st = do
  let st1 =
        if showTotal
          then
            let totalBlocks = sum [fiBlocks fi | fi <- files]
                totalLine = diredIndent ++ "total " ++ formatBlocks opts totalBlocks
             in appendLineD totalLine st
          else st

  entries <- formatLongEntries opts tz cs files
  return $ foldl addEntry st1 entries
  where
    addEntry acc entry =
      let line = diredIndent ++ leLine entry
          start = dsPos acc + length diredIndent + leNameStart entry
          end = dsPos acc + length diredIndent + leNameEnd entry
          acc1 = appendLineD line acc
       in appendOffsetD (start, end) acc1

quotingStyleName :: QuotingStyle -> String
quotingStyleName style = case style of
  QuoteLiteral -> "literal"
  QuoteShell -> "shell"
  QuoteShellAlways -> "shell-always"
  QuoteShellEscape -> "shell-escape"
  QuoteC -> "c"
  QuoteEscape -> "escape"
  QuoteLocale -> "locale"

-- | Determine if a command-line target should be descended into (list contents)
-- Returns True if we should list the directory contents, False if we should
-- show it as a file entry.
classifyTarget :: Opts -> FilePath -> FileStatus -> IO Bool
classifyTarget opts path stat
  -- -d means never descend into directories
  | optDirectory opts = return False
  -- If it's not a symlink, descend if it's a directory
  | not (isSymbolicLink stat) = return (isDirectory stat)
  -- It's a symlink - check dereference mode
  | otherwise = case effectiveDerefMode opts of
      DerefAlways -> do
        -- Follow the symlink, descend if target is a directory
        targetResult <- try $ getFileStatus path
        case targetResult of
          Left (_ :: IOException) -> return False -- Broken symlink
          Right targetStat -> return (isDirectory targetStat)
      DerefCmdLine -> do
        -- -H: follow symlinks on command line
        targetResult <- try $ getFileStatus path
        case targetResult of
          Left (_ :: IOException) -> return False
          Right targetStat -> return (isDirectory targetStat)
      DerefCmdLineDir -> do
        -- Default: follow symlinks to directories on command line
        targetResult <- try $ getFileStatus path
        case targetResult of
          Left (_ :: IOException) -> return False
          Right targetStat -> return (isDirectory targetStat)
      DerefNever -> return False -- Don't follow symlink, treat as file

-- | Get the effective dereference mode, applying defaults based on other options
effectiveDerefMode :: Opts -> DereferenceMode
effectiveDerefMode opts
  -- -d, -l, -F/--classify imply DerefNever by default
  | optDirectory opts = DerefNever
  | optLongFormat opts || optHideOwner opts || optHideGroup opts = DerefNever
  | optIndicator opts == IndicatorClassify = DerefNever
  -- If explicitly set, use that
  | optDereferenceMode opts /= DerefNever = optDereferenceMode opts
  -- Default is to follow symlinks to directories on command line
  | otherwise = DerefCmdLineDir

-- | Partition targets into those to show as files and those to descend into
partitionByDescend :: [(FilePath, Bool)] -> ([FilePath], [FilePath])
partitionByDescend = foldr go ([], [])
  where
    go (path, shouldDescend) (files, dirs)
      | shouldDescend = (files, path : dirs)
      | otherwise = (path : files, dirs)

-- | Get file info for a command-line argument
-- Uses the appropriate stat function based on dereference mode
getFileInfoForCmdLine :: Opts -> FilePath -> IO (Either () FileInfo)
getFileInfoForCmdLine opts path = do
  let deref = effectiveDerefMode opts
  result <- try $ getSymbolicLinkStatus path
  case result of
    Left (_ :: IOException) -> do
      hPutStrLn stderr $ "haskbox ls: cannot access '" ++ path ++ "': No such file or directory"
      return $ Left ()
    Right lstat -> do
      -- For symlinks, maybe get target stat too
      (stat, linkTarget, linkBroken, linkTargetIsDir) <-
        if isSymbolicLink lstat
          then do
            targetResult <- try $ getFileStatus path
            linkName <- try $ readSymbolicLink path
            let target = case linkName of
                  Right t -> Just t
                  Left (_ :: IOException) -> Nothing
            case targetResult of
              Left (_ :: IOException) ->
                -- Broken symlink - use lstat, mark as broken
                return (lstat, target, True, False)
              Right targetStat ->
                -- Valid symlink
                case deref of
                  DerefAlways -> return (targetStat, target, False, isDirectory targetStat)
                  DerefCmdLine -> return (targetStat, target, False, isDirectory targetStat)
                  _ -> return (lstat, target, False, isDirectory targetStat)
          else return (lstat, Nothing, False, False)
      absPath <- makeAbsolute path
      let displayName = path
      fi <- buildFileInfo opts displayName path absPath stat lstat linkTarget linkBroken linkTargetIsDir
      return $ Right fi

listDirectory ::
  Opts ->
  ColorScheme ->
  TimeZone ->
  Int ->
  IORef (Set (DeviceID, FileOffset)) ->
  IORef Int ->
  FilePath ->
  IO (Either String ())
listDirectory opts cs tz termWidth visited statusRef dir = do
  result <- try $ getDirectoryContents dir
  case result of
    Left (e :: IOException) -> return $ Left (friendlyError (show e))
    Right contents -> do
      let filtered = filter (shouldShow opts) contents
      entries <- mapM (getFileInfo opts . (dir </>)) filtered
      let (errors, fileInfos) = partitionEithers entries
      unless (null errors) $ setSeverity statusRef 1
      let sorted = sortFiles opts fileInfos
      outputFiles opts cs tz termWidth True sorted

      when (optRecursive opts) $ do
        let subdirs = [fi | fi <- sorted, fiType fi == TypeDirectory, fiName fi `notElem` [".", ".."]]
        forM_ subdirs $ \subdir -> do
          let subpath = fiPath subdir
          let key = (fiDevice subdir, fiInode subdir)
          visitedSet <- readIORef visited
          if Set.member key visitedSet
            then hPutStrLn stderr $ "haskbox ls: not listing already-visited directory: " ++ subpath
            else do
              writeIORef visited (Set.insert key visitedSet)
              putStrLn ""
              putStrLn $ subpath ++ ":"
              subResult <- listDirectory opts cs tz termWidth visited statusRef subpath
              case subResult of
                Left err -> do
                  hPutStrLn stderr $ "haskbox ls: cannot access '" ++ subpath ++ "': " ++ err
                  setSeverity statusRef 1
                Right () -> return ()

      return $ Right ()

outputFiles :: Opts -> ColorScheme -> TimeZone -> Int -> Bool -> [FileInfo] -> IO ()
outputFiles opts cs tz termWidth showTotal files = do
  let terminator = if optZero opts then '\0' else '\n'
      showTotalLine = showTotal && (optLongFormat opts || optSize opts)
      totalBlocks = sum [fiBlocks fi | fi <- files]
      totalLine = "total " ++ formatBlocks opts totalBlocks

  case optFormat opts of
    FormatLong -> do
      when showTotalLine $ putStrLn totalLine
      outputLines <- formatLong opts tz cs files
      mapM_ (\l -> putStr l >> putChar terminator) outputLines
    FormatColumns -> do
      when showTotalLine $ putStrLn totalLine
      let outputLines = formatColumns opts termWidth cs files
      mapM_ (\l -> putStr l >> putChar terminator) outputLines
    FormatAcross -> do
      when showTotalLine $ putStrLn totalLine
      let outputLines = formatAcross opts termWidth cs files
      mapM_ (\l -> putStr l >> putChar terminator) outputLines
    FormatCommas -> do
      when showTotalLine $ putStrLn totalLine
      let outputLines = formatCommas opts termWidth cs files
      mapM_ (\l -> putStr l >> putChar terminator) outputLines
    FormatSingleColumn -> do
      when showTotalLine $ putStrLn totalLine
      let outputLines = formatSingleColumn opts cs files
      mapM_ (\l -> putStr l >> putChar terminator) outputLines
    FormatDefault -> do
      -- Should have been resolved by adjustForTerminal, but default to single column
      when showTotalLine $ putStrLn totalLine
      let outputLines = formatSingleColumn opts cs files
      mapM_ (\l -> putStr l >> putChar terminator) outputLines

getFileInfo :: Opts -> FilePath -> IO (Either () FileInfo)
getFileInfo opts path = do
  result <- try $ getFileStatus' opts path
  case result of
    Left (e :: IOException) -> do
      hPutStrLn stderr $ "haskbox ls: cannot access '" ++ path ++ "': " ++ friendlyError (show e)
      return $ Left ()
    Right (status, linkStatus, linkTarget) -> do
      absPath <- makeAbsolute path
      let name = takeFileName path
          ftype = getFileType linkStatus
          mode = fileMode linkStatus
          isExec =
            (mode .&. ownerExecuteMode) /= 0
              || (mode .&. groupExecuteMode) /= 0
              || (mode .&. otherExecuteMode) /= 0

      (linkBroken, linkTargetIsDir) <-
        if isSymbolicLink linkStatus
          then do
            targetResult <- try $ getFileStatus path
            case targetResult of
              Right targetStat -> return (False, isDirectory targetStat)
              Left (_ :: IOException) -> return (True, False)
          else return (False, False)

      birthTime <- getBirthTime path

      let linkTargetAbs = makeLinkTargetAbs absPath linkTarget
      return $
        Right
          FileInfo
            { fiName = name,
              fiPath = path,
              fiAbsPath = absPath,
              fiType = ftype,
              fiMode = mode,
              fiLinks = linkCount linkStatus,
              fiOwner = fileOwner status,
              fiGroup = fileGroup status,
              fiSize = fileSize status,
              fiBlocks = getBlocks status,
              fiModTime = posixToUTC (modificationTimeHiRes status),
              fiAccessTime = posixToUTC (accessTimeHiRes status),
              fiChangeTime = posixToUTC (statusChangeTimeHiRes status),
              fiBirthTime = birthTime,
              fiInode = fromIntegral $ fileID linkStatus,
              fiDevice = deviceID linkStatus,
              fiLinkTarget = linkTarget,
              fiLinkTargetAbs = linkTargetAbs,
              fiLinkTargetIsDir = linkTargetIsDir,
              fiLinkBroken = linkBroken,
              fiExecutable = isExec && ftype == TypeRegular
            }

getFileStatus' :: Opts -> FilePath -> IO (FileStatus, FileStatus, Maybe String)
getFileStatus' opts path = do
  linkStatus <- getSymbolicLinkStatus path

  if isSymbolicLink linkStatus
    then do
      target <- readSymbolicLink path

      let shouldDeref = case optDereferenceMode opts of
            DerefAlways -> True
            DerefCmdLine -> False
            DerefCmdLineDir -> False
            DerefNever -> False

      if shouldDeref
        then do
          result <- try $ getFileStatus path
          case result of
            Right status -> return (status, linkStatus, Just target)
            Left (_ :: IOException) -> return (linkStatus, linkStatus, Just target)
        else return (linkStatus, linkStatus, Just target)
    else return (linkStatus, linkStatus, Nothing)

getFileType :: FileStatus -> FileType
getFileType status
  | isDirectory status = TypeDirectory
  | isSymbolicLink status = TypeSymlink
  | isRegularFile status = TypeRegular
  | isBlockDevice status = TypeBlockDevice
  | isCharacterDevice status = TypeCharDevice
  | isNamedPipe status = TypeNamedPipe
  | isSocket status = TypeSocket
  | otherwise = TypeUnknown

-- | Build a FileInfo from the stat results
buildFileInfo :: Opts -> FilePath -> FilePath -> FilePath -> FileStatus -> FileStatus -> Maybe String -> Bool -> Bool -> IO FileInfo
buildFileInfo _opts displayName path absPath stat lstat linkTarget linkBroken linkTargetIsDir = do
  let name = displayName
      ftype = getFileType lstat
      mode = fileMode lstat
      isExec =
        (mode .&. ownerExecuteMode) /= 0
          || (mode .&. groupExecuteMode) /= 0
          || (mode .&. otherExecuteMode) /= 0

  birthTime <- getBirthTime path

  let linkTargetAbs = makeLinkTargetAbs absPath linkTarget
  return
    FileInfo
      { fiName = name,
        fiPath = path,
        fiAbsPath = absPath,
        fiType = ftype,
        fiMode = mode,
        fiLinks = linkCount lstat,
        fiOwner = fileOwner stat,
        fiGroup = fileGroup stat,
        fiSize = fileSize stat,
        fiBlocks = getBlocks stat,
        fiModTime = posixToUTC (modificationTimeHiRes stat),
        fiAccessTime = posixToUTC (accessTimeHiRes stat),
        fiChangeTime = posixToUTC (statusChangeTimeHiRes stat),
        fiBirthTime = birthTime,
        fiInode = fromIntegral $ fileID lstat,
        fiDevice = deviceID lstat,
        fiLinkTarget = linkTarget,
        fiLinkTargetAbs = linkTargetAbs,
        fiLinkTargetIsDir = linkTargetIsDir,
        fiLinkBroken = linkBroken,
        fiExecutable = isExec && ftype == TypeRegular
      }

-- | Get allocated blocks from stat (st_blocks), with fallback to size-based calculation
getBlocks :: FileStatus -> FileOffset
getBlocks status = case fileBlocks status of
  Just blocks -> fromIntegral blocks
  -- Fallback for systems that don't support st_blocks
  Nothing -> (fileSize status + 511) `div` 512

makeLinkTargetAbs :: FilePath -> Maybe String -> Maybe FilePath
makeLinkTargetAbs _ Nothing = Nothing
makeLinkTargetAbs absPath (Just target) =
  if isAbsolute target
    then Just target
    else Just (takeDirectory absPath </> target)

posixToUTC :: POSIXTime -> UTCTime
posixToUTC = posixSecondsToUTCTime . realToFrac

friendlyError :: String -> String
friendlyError s
  | "does not exist" `isIn` s = "No such file or directory"
  | "Permission denied" `isIn` s = "Permission denied"
  | "Not a directory" `isIn` s = "Not a directory"
  | otherwise = s
  where
    isIn needle haystack = needle `isPrefixOf` haystack || any (isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'

parseArgs :: Opts -> [String] -> Either String (Opts, [FilePath])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--all" : rest) = parseArgs opts {optAll = True} rest
parseArgs opts ("--almost-all" : rest) = parseArgs opts {optAlmostAll = True} rest
parseArgs opts ("--author" : rest) = parseArgs opts {optAuthor = True} rest
parseArgs opts ("--escape" : rest) = parseArgs opts {optQuotingStyle = QuoteEscape} rest
parseArgs opts ("--block-size" : arg : rest) = case parseBlockSizeArg arg of
  Just n -> parseArgs opts {optBlockSize = Just n} rest
  Nothing -> Left $ "invalid block size: '" ++ arg ++ "'"
parseArgs opts (opt : rest)
  | "--block-size=" `isPrefixOf` opt =
      let arg = drop 13 opt
       in case parseBlockSizeArg arg of
            Just n -> parseArgs opts {optBlockSize = Just n} rest
            Nothing -> Left $ "invalid block size: '" ++ arg ++ "'"
parseArgs opts ("--classify" : rest) = parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAlways} rest
parseArgs opts (opt : rest)
  | "--classify=" `isPrefixOf` opt =
      let arg = drop 11 opt
       in case arg of
            "always" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAlways} rest
            "yes" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAlways} rest
            "force" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAlways} rest
            "auto" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAuto} rest
            "tty" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAuto} rest
            "if-tty" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAuto} rest
            "never" -> parseArgs opts {optIndicator = IndicatorNone, optIndicatorWhen = IndicatorAlways} rest
            "no" -> parseArgs opts {optIndicator = IndicatorNone, optIndicatorWhen = IndicatorAlways} rest
            "none" -> parseArgs opts {optIndicator = IndicatorNone, optIndicatorWhen = IndicatorAlways} rest
            _ -> Left $ "invalid argument '" ++ arg ++ "' for '--classify'"
parseArgs opts ("--color" : rest) = parseArgs opts {optColorMode = ColorAlways} rest
parseArgs opts (opt : rest)
  | "--color=" `isPrefixOf` opt =
      let arg = drop 8 opt
       in case arg of
            "always" -> parseArgs opts {optColorMode = ColorAlways} rest
            "yes" -> parseArgs opts {optColorMode = ColorAlways} rest
            "force" -> parseArgs opts {optColorMode = ColorAlways} rest
            "auto" -> parseArgs opts {optColorMode = ColorAuto} rest
            "tty" -> parseArgs opts {optColorMode = ColorAuto} rest
            "if-tty" -> parseArgs opts {optColorMode = ColorAuto} rest
            "never" -> parseArgs opts {optColorMode = ColorNever} rest
            "no" -> parseArgs opts {optColorMode = ColorNever} rest
            "none" -> parseArgs opts {optColorMode = ColorNever} rest
            _ -> Left $ "invalid argument '" ++ arg ++ "' for '--color'"
parseArgs opts ("--directory" : rest) = parseArgs opts {optDirectory = True} rest
parseArgs opts ("--dired" : rest) = parseArgs opts {optDired = True, optLongFormat = True, optFormat = FormatLong, optHyperlink = HyperlinkNever} rest
parseArgs opts ("--file-type" : rest) = parseArgs opts {optIndicator = IndicatorFileType, optIndicatorWhen = IndicatorAlways} rest
parseArgs opts ("--format" : arg : rest) = case arg of
  "long" -> parseArgs opts {optFormat = FormatLong, optLongFormat = True} rest
  "verbose" -> parseArgs opts {optFormat = FormatLong, optLongFormat = True} rest
  "single-column" -> parseArgs opts {optFormat = FormatSingleColumn, optOnePerLine = True} rest
  "vertical" -> parseArgs opts {optFormat = FormatColumns} rest
  "across" -> parseArgs opts {optFormat = FormatAcross} rest
  "horizontal" -> parseArgs opts {optFormat = FormatAcross} rest
  "commas" -> parseArgs opts {optFormat = FormatCommas} rest
  _ -> Left $ "invalid argument '" ++ arg ++ "' for '--format'"
parseArgs opts (opt : rest)
  | "--format=" `isPrefixOf` opt =
      parseArgs opts ("--format" : drop 9 opt : rest)
parseArgs opts ("--full-time" : rest) = parseArgs opts {optFullTime = True, optLongFormat = True, optFormat = FormatLong} rest
parseArgs opts ("--group-directories-first" : rest) = parseArgs opts {optGroupDirsFirst = True} rest
parseArgs opts ("--human-readable" : rest) = parseArgs opts {optHumanReadable = True} rest
parseArgs opts ("--si" : rest) = parseArgs opts {optSI = True, optHumanReadable = True} rest
parseArgs opts ("--dereference-command-line" : rest) = parseArgs opts {optDereferenceMode = DerefCmdLine} rest
parseArgs opts ("--dereference-command-line-symlink-to-dir" : rest) = parseArgs opts {optDereferenceMode = DerefCmdLineDir} rest
parseArgs opts ("--hide-control-chars" : rest) = parseArgs opts {optLiteralControl = True} rest
parseArgs opts ("--show-control-chars" : rest) = parseArgs opts {optShowControl = True} rest
parseArgs opts ("--hyperlink" : rest) = parseArgs opts {optHyperlink = HyperlinkAlways} rest
parseArgs opts (opt : rest)
  | "--hyperlink=" `isPrefixOf` opt =
      let arg = drop 12 opt
       in case arg of
            "always" -> parseArgs opts {optHyperlink = HyperlinkAlways} rest
            "auto" -> parseArgs opts {optHyperlink = HyperlinkAuto} rest
            "never" -> parseArgs opts {optHyperlink = HyperlinkNever} rest
            _ -> Left $ "invalid argument '" ++ arg ++ "' for '--hyperlink'"
parseArgs opts (opt : rest)
  | "--hide=" `isPrefixOf` opt =
      let pat = drop 7 opt
       in parseArgs opts {optHidePatterns = pat : optHidePatterns opts} rest
parseArgs opts ("--ignore" : pat : rest) = parseArgs opts {optIgnorePatterns = pat : optIgnorePatterns opts} rest
parseArgs opts (opt : rest)
  | "--ignore=" `isPrefixOf` opt =
      let pat = drop 9 opt
       in parseArgs opts {optIgnorePatterns = pat : optIgnorePatterns opts} rest
parseArgs opts ("--ignore-backups" : rest) = parseArgs opts {optIgnoreBackups = True} rest
parseArgs opts ("--indicator-style" : arg : rest) = case arg of
  "none" -> parseArgs opts {optIndicator = IndicatorNone, optIndicatorWhen = IndicatorAlways} rest
  "slash" -> parseArgs opts {optIndicator = IndicatorSlash, optIndicatorWhen = IndicatorAlways} rest
  "file-type" -> parseArgs opts {optIndicator = IndicatorFileType, optIndicatorWhen = IndicatorAlways} rest
  "classify" -> parseArgs opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAlways} rest
  _ -> Left $ "invalid argument '" ++ arg ++ "' for '--indicator-style'"
parseArgs opts (opt : rest)
  | "--indicator-style=" `isPrefixOf` opt =
      parseArgs opts ("--indicator-style" : drop 18 opt : rest)
parseArgs opts ("--inode" : rest) = parseArgs opts {optInode = True} rest
parseArgs opts ("--kibibytes" : rest) = parseArgs opts {optKibibytes = True} rest
parseArgs opts ("--dereference" : rest) = parseArgs opts {optDereferenceMode = DerefAlways} rest
parseArgs opts ("--literal" : rest) = parseArgs opts {optQuotingStyle = QuoteLiteral} rest
parseArgs opts ("--numeric-uid-gid" : rest) = parseArgs opts {optNumericUID = True} rest
parseArgs opts ("--no-group" : rest) = parseArgs opts {optNoGroup = True} rest
parseArgs opts ("--quote-name" : rest) = parseArgs opts {optQuotingStyle = QuoteC} rest
parseArgs opts ("--quoting-style" : arg : rest) = case arg of
  "literal" -> parseArgs opts {optQuotingStyle = QuoteLiteral} rest
  "shell" -> parseArgs opts {optQuotingStyle = QuoteShell} rest
  "shell-always" -> parseArgs opts {optQuotingStyle = QuoteShellAlways} rest
  "shell-escape" -> parseArgs opts {optQuotingStyle = QuoteShellEscape} rest
  "shell-escape-always" -> parseArgs opts {optQuotingStyle = QuoteShellEscape} rest
  "c" -> parseArgs opts {optQuotingStyle = QuoteC} rest
  "escape" -> parseArgs opts {optQuotingStyle = QuoteEscape} rest
  "locale" -> parseArgs opts {optQuotingStyle = QuoteLocale} rest
  "clocale" -> parseArgs opts {optQuotingStyle = QuoteLocale} rest
  _ -> Left $ "invalid argument '" ++ arg ++ "' for '--quoting-style'"
parseArgs opts (opt : rest)
  | "--quoting-style=" `isPrefixOf` opt =
      parseArgs opts ("--quoting-style" : drop 16 opt : rest)
parseArgs opts ("--reverse" : rest) = parseArgs opts {optReverse = True} rest
parseArgs opts ("--recursive" : rest) = parseArgs opts {optRecursive = True} rest
parseArgs opts ("--size" : rest) = parseArgs opts {optSize = True} rest
parseArgs opts ("--sort" : arg : rest) = case arg of
  "none" -> parseArgs opts {optSortMode = SortNone} rest
  "size" -> parseArgs opts {optSortMode = SortBySize} rest
  "time" -> parseArgs opts {optSortMode = SortByTime} rest
  "version" -> parseArgs opts {optSortMode = SortByVersion} rest
  "extension" -> parseArgs opts {optSortMode = SortByExtension} rest
  "width" -> parseArgs opts {optSortMode = SortByWidth} rest
  "name" -> parseArgs opts {optSortMode = SortByName} rest
  _ -> Left $ "invalid argument '" ++ arg ++ "' for '--sort'"
parseArgs opts (opt : rest)
  | "--sort=" `isPrefixOf` opt =
      parseArgs opts ("--sort" : drop 7 opt : rest)
parseArgs opts ("--time" : arg : rest) = case arg of
  "atime" -> parseArgs (setTimeMode opts TimeAccess) rest
  "access" -> parseArgs (setTimeMode opts TimeAccess) rest
  "use" -> parseArgs (setTimeMode opts TimeAccess) rest
  "ctime" -> parseArgs (setTimeMode opts TimeChange) rest
  "status" -> parseArgs (setTimeMode opts TimeChange) rest
  "mtime" -> parseArgs (setTimeMode opts TimeMod) rest
  "modification" -> parseArgs (setTimeMode opts TimeMod) rest
  "birth" -> parseArgs (setTimeMode opts TimeBirth) rest
  "creation" -> parseArgs (setTimeMode opts TimeBirth) rest
  _ -> Left $ "invalid argument '" ++ arg ++ "' for '--time'"
parseArgs opts (opt : rest)
  | "--time=" `isPrefixOf` opt =
      parseArgs opts ("--time" : drop 7 opt : rest)
parseArgs opts ("--time-style" : arg : rest) = case parseTimeStyle arg of
  Just style -> parseArgs opts {optTimeStyle = style} rest
  Nothing -> Left $ "invalid argument '" ++ arg ++ "' for '--time-style'"
parseArgs opts (opt : rest)
  | "--time-style=" `isPrefixOf` opt =
      parseArgs opts ("--time-style" : drop 13 opt : rest)
parseArgs opts ("--tabsize" : arg : rest) = case readMaybe arg of
  Just n -> parseArgs opts {optTabSize = n} rest
  Nothing -> Left $ "invalid tab size: '" ++ arg ++ "'"
parseArgs opts (opt : rest)
  | "--tabsize=" `isPrefixOf` opt =
      let arg = drop 10 opt
       in case readMaybe arg of
            Just n -> parseArgs opts {optTabSize = n} rest
            Nothing -> Left $ "invalid tab size: '" ++ arg ++ "'"
parseArgs opts ("--width" : arg : rest) = case readMaybe arg of
  Just n -> parseArgs opts {optWidth = Just n} rest
  Nothing -> Left $ "invalid line width: '" ++ arg ++ "'"
parseArgs opts (opt : rest)
  | "--width=" `isPrefixOf` opt =
      let arg = drop 8 opt
       in case readMaybe arg of
            Just n -> parseArgs opts {optWidth = Just n} rest
            Nothing -> Left $ "invalid line width: '" ++ arg ++ "'"
parseArgs opts ("--zero" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("--context" : rest) = parseArgs opts {optContext = True} rest
parseArgs opts (('-' : c : shortOpts) : rest)
  | c /= '-' =
      parseShortOpts opts (c : shortOpts) rest
parseArgs _ (opt@('-' : '-' : _) : _) = Left $ "unrecognized option '" ++ opt ++ "'"
parseArgs opts (path : rest) = do
  (opts', paths) <- parseArgs opts rest
  Right (opts', path : paths)

parseShortOpts :: Opts -> String -> [String] -> Either String (Opts, [FilePath])
parseShortOpts opts [] rest = parseArgs opts rest
parseShortOpts opts ('a' : cs) rest = parseShortOpts opts {optAll = True} cs rest
parseShortOpts opts ('A' : cs) rest = parseShortOpts opts {optAlmostAll = True} cs rest
parseShortOpts opts ('b' : cs) rest = parseShortOpts opts {optQuotingStyle = QuoteEscape} cs rest
parseShortOpts opts ('B' : cs) rest = parseShortOpts opts {optIgnoreBackups = True} cs rest
parseShortOpts opts ('c' : cs) rest = parseShortOpts opts {optTimeMode = TimeChange, optSortMode = SortByTime} cs rest
parseShortOpts opts ('C' : cs) rest = parseShortOpts opts {optFormat = FormatColumns} cs rest
parseShortOpts opts ('d' : cs) rest = parseShortOpts opts {optDirectory = True} cs rest
parseShortOpts opts ('D' : cs) rest = parseShortOpts opts {optDired = True} cs rest
parseShortOpts opts ('f' : cs) rest =
  parseShortOpts
    opts
      { optAll = True,
        optSortMode = SortNone,
        optColorMode = ColorNever,
        optIndicator = IndicatorNone,
        optIndicatorWhen = IndicatorAlways,
        optLongFormat = False,
        optSize = False,
        optHideOwner = False,
        optHideGroup = False
      }
    cs
    rest
parseShortOpts opts ('F' : cs) rest = parseShortOpts opts {optIndicator = IndicatorClassify, optIndicatorWhen = IndicatorAlways} cs rest
parseShortOpts opts ('g' : cs) rest = parseShortOpts opts {optHideOwner = True, optLongFormat = True, optFormat = FormatLong} cs rest
parseShortOpts opts ('G' : cs) rest = parseShortOpts opts {optNoGroup = True} cs rest
parseShortOpts opts ('h' : cs) rest = parseShortOpts opts {optHumanReadable = True} cs rest
parseShortOpts opts ('H' : cs) rest = parseShortOpts opts {optDereferenceMode = DerefCmdLine} cs rest
parseShortOpts opts ('i' : cs) rest = parseShortOpts opts {optInode = True} cs rest
parseShortOpts opts ('I' : cs) rest
  | null cs = case rest of
      (pat : rest') -> parseArgs opts {optIgnorePatterns = pat : optIgnorePatterns opts} rest'
      [] -> Left "option requires an argument -- 'I'"
  | otherwise = parseArgs opts {optIgnorePatterns = cs : optIgnorePatterns opts} rest
parseShortOpts opts ('k' : cs) rest = parseShortOpts opts {optKibibytes = True} cs rest
parseShortOpts opts ('l' : cs) rest = parseShortOpts opts {optLongFormat = True, optFormat = FormatLong} cs rest
parseShortOpts opts ('L' : cs) rest = parseShortOpts opts {optDereferenceMode = DerefAlways} cs rest
parseShortOpts opts ('m' : cs) rest = parseShortOpts opts {optFormat = FormatCommas} cs rest
parseShortOpts opts ('n' : cs) rest = parseShortOpts opts {optNumericUID = True, optLongFormat = True, optFormat = FormatLong} cs rest
parseShortOpts opts ('N' : cs) rest = parseShortOpts opts {optQuotingStyle = QuoteLiteral} cs rest
parseShortOpts opts ('o' : cs) rest = parseShortOpts opts {optHideGroup = True, optLongFormat = True, optFormat = FormatLong} cs rest
parseShortOpts opts ('p' : cs) rest = parseShortOpts opts {optIndicator = IndicatorSlash, optIndicatorWhen = IndicatorAlways} cs rest
parseShortOpts opts ('q' : cs) rest = parseShortOpts opts {optLiteralControl = True} cs rest
parseShortOpts opts ('Q' : cs) rest = parseShortOpts opts {optQuotingStyle = QuoteC} cs rest
parseShortOpts opts ('r' : cs) rest = parseShortOpts opts {optReverse = True} cs rest
parseShortOpts opts ('R' : cs) rest = parseShortOpts opts {optRecursive = True} cs rest
parseShortOpts opts ('s' : cs) rest = parseShortOpts opts {optSize = True} cs rest
parseShortOpts opts ('S' : cs) rest = parseShortOpts opts {optSortMode = SortBySize} cs rest
parseShortOpts opts ('t' : cs) rest = parseShortOpts opts {optSortMode = SortByTime} cs rest
parseShortOpts opts ('T' : cs) rest
  | null cs = case rest of
      (arg : rest') -> case readMaybe arg of
        Just n -> parseArgs opts {optTabSize = n} rest'
        Nothing -> Left $ "invalid tab size: '" ++ arg ++ "'"
      [] -> Left "option requires an argument -- 'T'"
  | otherwise = case readMaybe cs of
      Just n -> parseArgs opts {optTabSize = n} rest
      Nothing -> Left $ "invalid tab size: '" ++ cs ++ "'"
parseShortOpts opts ('u' : cs) rest = parseShortOpts opts {optTimeMode = TimeAccess, optSortMode = SortByTime} cs rest
parseShortOpts opts ('U' : cs) rest = parseShortOpts opts {optSortMode = SortNone} cs rest
parseShortOpts opts ('v' : cs) rest = parseShortOpts opts {optSortMode = SortByVersion} cs rest
parseShortOpts opts ('w' : cs) rest
  | null cs = case rest of
      (arg : rest') -> case readMaybe arg of
        Just n -> parseArgs opts {optWidth = Just n} rest'
        Nothing -> Left $ "invalid line width: '" ++ arg ++ "'"
      [] -> Left "option requires an argument -- 'w'"
  | otherwise = case readMaybe cs of
      Just n -> parseArgs opts {optWidth = Just n} rest
      Nothing -> Left $ "invalid line width: '" ++ cs ++ "'"
parseShortOpts opts ('x' : cs) rest = parseShortOpts opts {optFormat = FormatAcross} cs rest
parseShortOpts opts ('X' : cs) rest = parseShortOpts opts {optSortMode = SortByExtension} cs rest
parseShortOpts opts ('Z' : cs) rest = parseShortOpts opts {optContext = True} cs rest
parseShortOpts opts ('1' : cs) rest = parseShortOpts opts {optOnePerLine = True, optFormat = FormatSingleColumn} cs rest
parseShortOpts _ (c : _) _ = Left $ "invalid option -- '" ++ [c] ++ "'"

setTimeMode :: Opts -> TimeMode -> Opts
setTimeMode opts mode = opts {optTimeMode = mode}

parseTimeStyle :: String -> Maybe TimeStyle
parseTimeStyle "full-iso" = Just TimeFullISO
parseTimeStyle "long-iso" = Just TimeLongISO
parseTimeStyle "iso" = Just TimeISO
parseTimeStyle "locale" = Just TimeLocale
parseTimeStyle ('+' : fmt) = Just (TimeCustom fmt)
parseTimeStyle _ = Nothing

parseBlockSizeArg :: String -> Maybe BlockSize
parseBlockSizeArg s =
  let (numPart, suffixPart) = span isDigit s
      n =
        if null numPart
          then 1 :: Integer
          else read numPart
      suffixNorm = map toUpper suffixPart
      showSuffix = null numPart
      mk mult suf =
        let suffix = if showSuffix then suf else ""
         in Just (BlockSize (n * mult) suffix)
   in case suffixNorm of
        "" -> mk 1 ""
        "K" -> mk 1024 "K"
        "M" -> mk (1024 ^ (2 :: Integer)) "M"
        "G" -> mk (1024 ^ (3 :: Integer)) "G"
        "T" -> mk (1024 ^ (4 :: Integer)) "T"
        "P" -> mk (1024 ^ (5 :: Integer)) "P"
        "E" -> mk (1024 ^ (6 :: Integer)) "E"
        "Z" -> mk (1024 ^ (7 :: Integer)) "Z"
        "Y" -> mk (1024 ^ (8 :: Integer)) "Y"
        "KB" -> mk 1000 "KB"
        "MB" -> mk (1000 ^ (2 :: Integer)) "MB"
        "GB" -> mk (1000 ^ (3 :: Integer)) "GB"
        "TB" -> mk (1000 ^ (4 :: Integer)) "TB"
        "PB" -> mk (1000 ^ (5 :: Integer)) "PB"
        "EB" -> mk (1000 ^ (6 :: Integer)) "EB"
        "ZB" -> mk (1000 ^ (7 :: Integer)) "ZB"
        "YB" -> mk (1000 ^ (8 :: Integer)) "YB"
        _ -> Nothing

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox ls [OPTION]... [FILE]...",
        "List information about the FILEs (the current directory by default).",
        "Sort entries alphabetically if none of -cftuvSUX nor --sort is specified.",
        "",
        "  -a, --all                  do not ignore entries starting with .",
        "  -A, --almost-all           do not list implied . and ..",
        "      --author               with -l, print the author of each file",
        "  -b, --escape               print C-style escapes for nongraphic characters",
        "      --block-size=SIZE      scale sizes by SIZE when printing them",
        "  -B, --ignore-backups       do not list implied entries ending with ~",
        "  -c                         with -lt: sort by, and show, ctime (time of last",
        "                               modification of file status information);",
        "                               with -l: show ctime and sort by name;",
        "                               otherwise: sort by ctime, newest first",
        "  -C                         list entries by columns",
        "      --color[=WHEN]         colorize the output; WHEN can be 'always' (default",
        "                               if omitted), 'auto', or 'never'",
        "  -d, --directory            list directories themselves, not their contents",
        "  -D, --dired                generate output designed for Emacs' dired mode",
        "  -f                         do not sort, enable -aU, disable -ls --color",
        "  -F, --classify             append indicator (one of */=>@|) to entries",
        "      --file-type            likewise, except do not append '*'",
        "      --format=WORD          across -x, commas -m, horizontal -x, long -l,",
        "                               single-column -1, verbose -l, vertical -C",
        "      --full-time            like -l --time-style=full-iso",
        "  -g                         like -l, but do not list owner",
        "      --group-directories-first",
        "                             group directories before files",
        "  -G, --no-group             in a long listing, don't print group names",
        "  -h, --human-readable       with -l and -s, print sizes like 1K 234M 2G etc.",
        "      --si                   likewise, but use powers of 1000 not 1024",
        "  -H, --dereference-command-line",
        "                             follow symbolic links listed on the command line",
        "      --dereference-command-line-symlink-to-dir",
        "                             follow each command line symbolic link",
        "                               that points to a directory",
        "      --hide=PATTERN         do not list implied entries matching shell PATTERN",
        "                               (overridden by -a or -A)",
        "      --hyperlink[=WHEN]     hyperlink file names; WHEN can be 'always'",
        "                               (default if omitted), 'auto', or 'never'",
        "      --indicator-style=WORD  append indicator with style WORD to entry names:",
        "                               none (default), slash (-p),",
        "                               file-type (--file-type), classify (-F)",
        "  -i, --inode                print the index number of each file",
        "  -I, --ignore=PATTERN       do not list implied entries matching shell PATTERN",
        "  -k, --kibibytes            default to 1024-byte blocks for disk usage",
        "  -l                         use a long listing format",
        "  -L, --dereference          when showing file information for a symbolic",
        "                               link, show information for the file the link",
        "                               references rather than for the link itself",
        "  -m                         fill width with a comma separated list of entries",
        "  -n, --numeric-uid-gid      like -l, but list numeric user and group IDs",
        "  -N, --literal              print entry names without quoting",
        "  -o                         like -l, but do not list group information",
        "  -p, --indicator-style=slash",
        "                             append / indicator to directories",
        "  -q, --hide-control-chars   print ? instead of nongraphic characters",
        "      --show-control-chars   show nongraphic characters as-is (the default,",
        "                               unless program is 'ls' and output is a terminal)",
        "  -Q, --quote-name           enclose entry names in double quotes",
        "      --quoting-style=WORD   use quoting style WORD for entry names:",
        "                               literal, locale, shell, shell-always,",
        "                               shell-escape, shell-escape-always, c, escape",
        "  -r, --reverse              reverse order while sorting",
        "  -R, --recursive            list subdirectories recursively",
        "  -s, --size                 print the allocated size of each file, in blocks",
        "  -S                         sort by file size, largest first",
        "      --sort=WORD            sort by WORD instead of name: none (-U), size (-S),",
        "                               time (-t), version (-v), extension (-X)",
        "      --time=WORD            with -l, show time as WORD instead of default",
        "                               modification time: atime or access or use (-u);",
        "                               ctime or status (-c); also use specified time",
        "                               as sort key if --sort=time (newest first)",
        "      --time-style=TIME_STYLE  time/date format with -l; see TIME_STYLE below",
        "  -t                         sort by time, newest first; see --time",
        "  -T, --tabsize=COLS         assume tab stops at each COLS instead of 8",
        "  -u                         with -lt: sort by, and show, access time;",
        "                               with -l: show access time and sort by name;",
        "                               otherwise: sort by access time, newest first",
        "  -U                         do not sort; list entries in directory order",
        "  -v                         natural sort of (version) numbers within text",
        "  -w, --width=COLS           set output width to COLS.  0 means no limit",
        "  -x                         list entries by lines instead of by columns",
        "  -X                         sort alphabetically by entry extension",
        "  -Z, --context              print any security context of each file",
        "  -1                         list one file per line",
        "      --zero                 end each output line with NUL, not newline",
        "      --help                 display this help and exit",
        "      --version              output version information and exit",
        "",
        "The SIZE argument is an integer and optional unit (example: 10K is 10*1024).",
        "Units are K,M,G,T,P,E,Z,Y (powers of 1024) or KB,MB,... (powers of 1000).",
        "",
        "The TIME_STYLE argument can be full-iso, long-iso, iso, locale, or +FORMAT.",
        "FORMAT is interpreted like in date(1).",
        "",
        "Exit status:",
        " 0  if OK,",
        " 1  if minor problems (e.g., cannot access subdirectory),",
        " 2  if serious trouble (e.g., cannot access command-line argument)."
      ]
