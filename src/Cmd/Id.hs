{-# LANGUAGE OverloadedStrings #-}

module Cmd.Id (run) where

import Control.Exception (SomeException, catch)
import Data.List (intercalate)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (GroupID, UserID)
import System.Posix.User
  ( getEffectiveGroupID,
    getEffectiveUserID,
    getGroupEntryForID,
    getGroups,
    getUserEntryForID,
    getUserEntryForName,
    groupName,
    userGroupID,
    userID,
    userName,
  )
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox id: " ++ err
    exitFailure
  Right (opts, users)
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "id") >> exitSuccess
    | null users -> printCurrentUser opts
    | otherwise -> mapM_ (printUser opts) users

data Opts = Opts
  { optUser :: !Bool,
    optGroup :: !Bool,
    optGroups :: !Bool,
    optName :: !Bool,
    optReal :: !Bool,
    optZero :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optUser = False,
      optGroup = False,
      optGroups = False,
      optName = False,
      optReal = False,
      optZero = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String (Opts, [String])
parseArgs opts [] = Right (opts, [])
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--user" : rest) = parseArgs opts {optUser = True} rest
parseArgs opts ("--group" : rest) = parseArgs opts {optGroup = True} rest
parseArgs opts ("--groups" : rest) = parseArgs opts {optGroups = True} rest
parseArgs opts ("--name" : rest) = parseArgs opts {optName = True} rest
parseArgs opts ("--real" : rest) = parseArgs opts {optReal = True} rest
parseArgs opts ("--zero" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("-u" : rest) = parseArgs opts {optUser = True} rest
parseArgs opts ("-g" : rest) = parseArgs opts {optGroup = True} rest
parseArgs opts ("-G" : rest) = parseArgs opts {optGroups = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optName = True} rest
parseArgs opts ("-r" : rest) = parseArgs opts {optReal = True} rest
parseArgs opts ("-z" : rest) = parseArgs opts {optZero = True} rest
parseArgs opts ("--" : rest) = Right (opts, rest)
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      'u' -> parseShortFlags o {optUser = True} fs r
      'g' -> parseShortFlags o {optGroup = True} fs r
      'G' -> parseShortFlags o {optGroups = True} fs r
      'n' -> parseShortFlags o {optName = True} fs r
      'r' -> parseShortFlags o {optReal = True} fs r
      'z' -> parseShortFlags o {optZero = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (arg : rest) = do
  (o, users) <- parseArgs opts rest
  Right (o, arg : users)

printCurrentUser :: Opts -> IO ()
printCurrentUser opts = do
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  groups <- getGroups

  if optUser opts
    then printUid opts uid
    else
      if optGroup opts
        then printGid opts gid
        else
          if optGroups opts
            then printAllGroups opts groups
            else printFullId uid gid groups

printUser :: Opts -> String -> IO ()
printUser opts username = do
  result <- catch (Right <$> getUserEntryForName username) handler
  case result of
    Left err -> do
      hPutStrLn stderr $ "haskbox id: '" ++ username ++ "': " ++ err
      exitFailure
    Right entry -> do
      let uid = userID entry
          gid = userGroupID entry
      groups <- getGroups -- NOTE: this gets current user's groups, not target user
      if optUser opts
        then printUid opts uid
        else
          if optGroup opts || optGroups opts
            then printGid opts gid
            else printFullId uid gid groups
  where
    handler :: SomeException -> IO (Either String a)
    handler _ = return $ Left "no such user"

printUid :: Opts -> CUid -> IO ()
printUid opts uid
  | optName opts = do
      entry <- getUserEntryForID uid
      putStrLn $ userName entry
  | otherwise = print uid

printGid :: Opts -> CGid -> IO ()
printGid opts gid
  | optName opts = do
      entry <- getGroupEntryForID gid
      putStrLn $ groupName entry
  | otherwise = print gid

printAllGroups :: Opts -> [CGid] -> IO ()
printAllGroups opts groups
  | optName opts = do
      names <- mapM getGroupName groups
      putStrLn $ unwords names
  | otherwise = putStrLn $ unwords $ map show groups
  where
    getGroupName gid = catch (groupName <$> getGroupEntryForID gid) handler
      where
        handler :: SomeException -> IO String
        handler _ = return $ show gid

printFullId :: CUid -> CGid -> [CGid] -> IO ()
printFullId uid gid groups = do
  uname <- catch (userName <$> getUserEntryForID uid) (const (return "") :: SomeException -> IO String)
  gname <- catch (groupName <$> getGroupEntryForID gid) (const (return "") :: SomeException -> IO String)
  groupStrs <- mapM formatGroup groups

  let uidStr = "uid=" ++ show uid ++ if null uname then "" else "(" ++ uname ++ ")"
      gidStr = "gid=" ++ show gid ++ if null gname then "" else "(" ++ gname ++ ")"
      groupsStr = "groups=" ++ intercalate "," groupStrs

  putStrLn $ uidStr ++ " " ++ gidStr ++ " " ++ groupsStr

formatGroup :: CGid -> IO String
formatGroup gid = do
  name <- catch (groupName <$> getGroupEntryForID gid) handler
  return $ show gid ++ if null name then "" else "(" ++ name ++ ")"
  where
    handler :: SomeException -> IO String
    handler _ = return ""

type CUid = UserID

type CGid = GroupID

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox id [OPTION]... [USER]...",
        "Print user and group information for each specified USER,",
        "or (when USER omitted) for the current user.",
        "",
        "  -g, --group     print only the effective group ID",
        "  -G, --groups    print all group IDs",
        "  -n, --name      print a name instead of a number, for -ugG",
        "  -r, --real      print the real ID instead of the effective ID, with -ugG",
        "  -u, --user      print only the effective user ID",
        "  -z, --zero      delimit entries with NUL characters, not whitespace",
        "      --help      display this help and exit",
        "      --version   output version information and exit"
      ]
