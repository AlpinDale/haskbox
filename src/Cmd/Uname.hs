{-# LANGUAGE OverloadedStrings #-}

module Cmd.Uname (run) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Unistd (SystemID (..), getSystemID)
import Version (versionString)

run :: [String] -> IO ()
run args = case parseArgs defaultOpts args of
  Left err -> do
    hPutStrLn stderr $ "haskbox uname: " ++ err
    exitFailure
  Right opts
    | optShowHelp opts -> printHelp >> exitSuccess
    | optShowVersion opts -> putStrLn (versionString "uname") >> exitSuccess
    | otherwise -> printUname opts

data Opts = Opts
  { optAll :: !Bool,
    optKernel :: !Bool,
    optNodename :: !Bool,
    optRelease :: !Bool,
    optSysVersion :: !Bool,
    optMachine :: !Bool,
    optProcessor :: !Bool,
    optHardware :: !Bool,
    optOperating :: !Bool,
    optShowHelp :: !Bool,
    optShowVersion :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optAll = False,
      optKernel = False,
      optNodename = False,
      optRelease = False,
      optSysVersion = False,
      optMachine = False,
      optProcessor = False,
      optHardware = False,
      optOperating = False,
      optShowHelp = False,
      optShowVersion = False
    }

parseArgs :: Opts -> [String] -> Either String Opts
parseArgs opts [] = Right opts
parseArgs opts ("--help" : rest) = parseArgs opts {optShowHelp = True} rest
parseArgs opts ("--version" : rest) = parseArgs opts {optShowVersion = True} rest
parseArgs opts ("--all" : rest) = parseArgs opts {optAll = True} rest
parseArgs opts ("--kernel-name" : rest) = parseArgs opts {optKernel = True} rest
parseArgs opts ("--nodename" : rest) = parseArgs opts {optNodename = True} rest
parseArgs opts ("--kernel-release" : rest) = parseArgs opts {optRelease = True} rest
parseArgs opts ("--kernel-version" : rest) = parseArgs opts {optSysVersion = True} rest
parseArgs opts ("--machine" : rest) = parseArgs opts {optMachine = True} rest
parseArgs opts ("-a" : rest) = parseArgs opts {optAll = True} rest
parseArgs opts ("-s" : rest) = parseArgs opts {optKernel = True} rest
parseArgs opts ("-n" : rest) = parseArgs opts {optNodename = True} rest
parseArgs opts ("-r" : rest) = parseArgs opts {optRelease = True} rest
parseArgs opts ("-v" : rest) = parseArgs opts {optSysVersion = True} rest
parseArgs opts ("-m" : rest) = parseArgs opts {optMachine = True} rest
parseArgs opts ("-p" : rest) = parseArgs opts {optProcessor = True} rest
parseArgs opts ("-i" : rest) = parseArgs opts {optHardware = True} rest
parseArgs opts ("-o" : rest) = parseArgs opts {optOperating = True} rest
parseArgs opts ("--processor" : rest) = parseArgs opts {optProcessor = True} rest
parseArgs opts ("--hardware-platform" : rest) = parseArgs opts {optHardware = True} rest
parseArgs opts ("--operating-system" : rest) = parseArgs opts {optOperating = True} rest
parseArgs opts ("--" : _) = Right opts
parseArgs _ (('-' : '-' : opt) : _) = Left $ "unrecognized option '--" ++ opt ++ "'"
parseArgs opts (('-' : flags) : rest)
  | not (null flags) = parseShortFlags opts flags rest
  where
    parseShortFlags o [] r = parseArgs o r
    parseShortFlags o (f : fs) r = case f of
      'a' -> parseShortFlags o {optAll = True} fs r
      's' -> parseShortFlags o {optKernel = True} fs r
      'n' -> parseShortFlags o {optNodename = True} fs r
      'r' -> parseShortFlags o {optRelease = True} fs r
      'v' -> parseShortFlags o {optSysVersion = True} fs r
      'm' -> parseShortFlags o {optMachine = True} fs r
      'p' -> parseShortFlags o {optProcessor = True} fs r
      'i' -> parseShortFlags o {optHardware = True} fs r
      'o' -> parseShortFlags o {optOperating = True} fs r
      _ -> Left $ "invalid option -- '" ++ [f] ++ "'"
parseArgs opts (_ : rest) = parseArgs opts rest

printUname :: Opts -> IO ()
printUname opts = do
  sysid <- getSystemID
  let showAll = optAll opts
      showDefault =
        not showAll
          && not (optKernel opts)
          && not (optNodename opts)
          && not (optRelease opts)
          && not (optSysVersion opts)
          && not (optMachine opts)
          && not (optProcessor opts)
          && not (optHardware opts)
          && not (optOperating opts)
      -- On macOS/Darwin (per GNU coreutils):
      -- - processor is "arm" for arm64, "i386" for x86_64 (not machine name)
      -- - hardware platform is "unknown" on macOS (omitted for -a)
      -- - operating system is "Darwin"
      mach = machine sysid
      -- GNU coreutils hardcodes processor based on architecture
      processorName = case mach of
        "arm64" -> "arm"
        "x86_64" -> "i386"
        "i386" -> "i386"
        other -> other
      -- hardwarePlatform is "unknown" on macOS - omitted when using -a
      hardwarePlatform = "unknown"
      operatingSystem = systemName sysid -- Darwin
      -- For -a, omit "unknown" fields (processor and hardware platform may be unknown)
      -- For explicit options like -p or -i, print "unknown"
      parts =
        filter
          (not . null)
          [ if showAll || showDefault || optKernel opts then systemName sysid else "",
            if showAll || optNodename opts then nodeName sysid else "",
            if showAll || optRelease opts then release sysid else "",
            if showAll || optSysVersion opts then version sysid else "",
            if showAll || optMachine opts then machine sysid else "",
            if optProcessor opts || (showAll && processorName /= "unknown") then processorName else "",
            if optHardware opts || (showAll && hardwarePlatform /= "unknown") then hardwarePlatform else "",
            if showAll || optOperating opts then operatingSystem else ""
          ]
  putStrLn $ unwords parts

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox uname [OPTION]...",
        "Print certain system information.",
        "",
        "  -a, --all                print all information",
        "  -s, --kernel-name        print the kernel name",
        "  -n, --nodename           print the network node hostname",
        "  -r, --kernel-release     print the kernel release",
        "  -v, --kernel-version     print the kernel version",
        "  -m, --machine            print the machine hardware name",
        "      --help               display this help and exit",
        "      --version            output version information and exit"
      ]
