module Main (main) where

import Cmd.Basename qualified as Basename
import Cmd.Cat qualified as Cat
import Cmd.Cksum qualified as Cksum
import Cmd.Comm qualified as Comm
import Cmd.Cp qualified as Cp
import Cmd.Cut qualified as Cut
import Cmd.Date qualified as Date
import Cmd.Df qualified as Df
import Cmd.Dirname qualified as Dirname
import Cmd.Dos2unix qualified as Dos2unix
import Cmd.Echo qualified as Echo
import Cmd.Env qualified as Env
import Cmd.Expand qualified as Expand
import Cmd.Factor qualified as Factor
import Cmd.False qualified as False
import Cmd.Fold qualified as Fold
import Cmd.Head qualified as Head
import Cmd.Hostid qualified as Hostid
import Cmd.Id qualified as Id
import Cmd.Link qualified as Link
import Cmd.Ln qualified as Ln
import Cmd.Logname qualified as Logname
import Cmd.Ls qualified as Ls
import Cmd.Mkdir qualified as Mkdir
import Cmd.Mkfifo qualified as Mkfifo
import Cmd.Mktemp qualified as Mktemp
import Cmd.Mv qualified as Mv
import Cmd.Nl qualified as Nl
import Cmd.Nproc qualified as Nproc
import Cmd.Od qualified as Od
import Cmd.Paste qualified as Paste
import Cmd.Printenv qualified as Printenv
import Cmd.Printf qualified as Printf
import Cmd.Pwd qualified as Pwd
import Cmd.Readlink qualified as Readlink
import Cmd.Realpath qualified as Realpath
import Cmd.Rm qualified as Rm
import Cmd.Rmdir qualified as Rmdir
import Cmd.Seq qualified as Seq
import Cmd.Shuf qualified as Shuf
import Cmd.Sleep qualified as Sleep
import Cmd.Sort qualified as Sort
import Cmd.Split qualified as Split
import Cmd.Stat qualified as Stat
import Cmd.Sync qualified as Sync
import Cmd.Tac qualified as Tac
import Cmd.Tail qualified as Tail
import Cmd.Tee qualified as Tee
import Cmd.Test qualified as Test
import Cmd.Timeout qualified as Timeout
import Cmd.Touch qualified as Touch
import Cmd.Tr qualified as Tr
import Cmd.True qualified as True
import Cmd.Truncate qualified as Truncate
import Cmd.Tsort qualified as Tsort
import Cmd.Tty qualified as Tty
import Cmd.Uname qualified as Uname
import Cmd.Unexpand qualified as Unexpand
import Cmd.Uniq qualified as Uniq
import Cmd.Unlink qualified as Unlink
import Cmd.Wc qualified as Wc
import Cmd.Whoami qualified as Whoami
import Cmd.Yes qualified as Yes
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [] -> do
      printUsage
      exitFailure
    ("--help" : _) -> printUsage
    ("--version" : _) -> putStrLn "haskbox 0.1.0"
    (cmd : rest) -> dispatch prog cmd rest

dispatch :: String -> String -> [String] -> IO ()
dispatch _ "basename" args = Basename.run args
dispatch _ "cat" args = Cat.run args
dispatch _ "cksum" args = Cksum.run args
dispatch _ "comm" args = Comm.run args
dispatch _ "cp" args = Cp.run args
dispatch _ "cut" args = Cut.run args
dispatch _ "date" args = Date.run args
dispatch _ "df" args = Df.run args
dispatch _ "dirname" args = Dirname.run args
dispatch _ "dos2unix" args = Dos2unix.run args
dispatch _ "echo" args = Echo.run args
dispatch _ "env" args = Env.run args
dispatch _ "expand" args = Expand.run args
dispatch _ "factor" args = Factor.run args
dispatch _ "false" args = False.run args
dispatch _ "fold" args = Fold.run args
dispatch _ "head" args = Head.run args
dispatch _ "hostid" args = Hostid.run args
dispatch _ "id" args = Id.run args
dispatch _ "link" args = Link.run args
dispatch _ "ln" args = Ln.run args
dispatch _ "logname" args = Logname.run args
dispatch _ "ls" args = Ls.run args
dispatch _ "mkdir" args = Mkdir.run args
dispatch _ "mkfifo" args = Mkfifo.run args
dispatch _ "mktemp" args = Mktemp.run args
dispatch _ "mv" args = Mv.run args
dispatch _ "nl" args = Nl.run args
dispatch _ "nproc" args = Nproc.run args
dispatch _ "od" args = Od.run args
dispatch _ "paste" args = Paste.run args
dispatch _ "printenv" args = Printenv.run args
dispatch _ "printf" args = Printf.run args
dispatch _ "pwd" args = Pwd.run args
dispatch _ "readlink" args = Readlink.run args
dispatch _ "realpath" args = Realpath.run args
dispatch _ "rm" args = Rm.run args
dispatch _ "rmdir" args = Rmdir.run args
dispatch _ "seq" args = Seq.run args
dispatch _ "shuf" args = Shuf.run args
dispatch _ "sleep" args = Sleep.run args
dispatch _ "sort" args = Sort.run args
dispatch _ "split" args = Split.run args
dispatch _ "stat" args = Stat.run args
dispatch _ "sync" args = Sync.run args
dispatch _ "tac" args = Tac.run args
dispatch _ "tail" args = Tail.run args
dispatch _ "tee" args = Tee.run args
dispatch _ "test" args = Test.run args
dispatch _ "[" args = Test.run args
dispatch _ "timeout" args = Timeout.run args
dispatch _ "touch" args = Touch.run args
dispatch _ "tr" args = Tr.run args
dispatch _ "true" args = True.run args
dispatch _ "truncate" args = Truncate.run args
dispatch _ "tsort" args = Tsort.run args
dispatch _ "tty" args = Tty.run args
dispatch _ "uname" args = Uname.run args
dispatch _ "unexpand" args = Unexpand.run args
dispatch _ "uniq" args = Uniq.run args
dispatch _ "unlink" args = Unlink.run args
dispatch _ "wc" args = Wc.run args
dispatch _ "whoami" args = Whoami.run args
dispatch _ "yes" args = Yes.run args
dispatch prog cmd _ = do
  hPutStrLn stderr $ prog ++ ": unknown command '" ++ cmd ++ "'"
  hPutStrLn stderr $ "Run '" ++ prog ++ " --help' for usage."
  exitFailure

printUsage :: IO ()
printUsage =
  putStr $
    unlines
      [ "haskbox - Unix utilities in Haskell",
        "",
        "Usage: haskbox <command> [args...]",
        "",
        "Commands:",
        "  basename   Strip directory from filename",
        "  cat        Concatenate and print files",
        "  cksum      Checksum and count bytes",
        "  comm       Compare sorted files",
        "  cp         Copy files",
        "  cut        Remove sections from lines",
        "  date       Print date and time",
        "  df         Report file system disk space usage",
        "  dirname    Strip filename from path",
        "  dos2unix   Convert DOS line endings to Unix",
        "  echo       Print arguments",
        "  env        Run in modified environment",
        "  expand     Convert tabs to spaces",
        "  factor     Factor integers",
        "  false      Return false",
        "  fold       Wrap lines to fit width",
        "  head       Output first part of files",
        "  hostid     Print host identifier",
        "  id         Print user and group IDs",
        "  link       Create hard link",
        "  ln         Create links",
        "  logname    Print login name",
        "  ls         List directory contents",
        "  mkdir      Create directories",
        "  mkfifo     Create named pipes (FIFOs)",
        "  mktemp     Create temporary file",
        "  mv         Move/rename files",
        "  nl         Number lines",
        "  nproc      Print number of processors",
        "  od         Dump files in octal/hex",
        "  paste      Merge lines of files",
        "  printenv   Print environment variables",
        "  printf     Format and print data",
        "  pwd        Print working directory",
        "  readlink   Print resolved symlink",
        "  realpath   Print resolved path",
        "  rm         Remove files",
        "  rmdir      Remove directories",
        "  seq        Print number sequence",
        "  shuf       Shuffle lines",
        "  sleep      Delay execution",
        "  sort       Sort lines",
        "  split      Split file into pieces",
        "  stat       Display file status",
        "  sync       Sync filesystems",
        "  tac        Reverse cat (last line first)",
        "  tail       Output last part of files",
        "  tee        Copy stdin to files",
        "  test       Check file types/compare values",
        "  timeout    Run with time limit",
        "  touch      Change file timestamps",
        "  tr         Translate characters",
        "  true       Return true",
        "  truncate   Shrink/extend file size",
        "  tsort      Topological sort",
        "  tty        Print terminal name",
        "  uname      Print system info",
        "  unexpand   Convert spaces to tabs",
        "  uniq       Filter duplicate lines",
        "  unlink     Remove file",
        "  wc         Count lines, words, bytes",
        "  whoami     Print effective user name",
        "  yes        Output string repeatedly",
        "",
        "Options:",
        "  --help     Show this help",
        "  --version  Show version",
        "",
        "Run 'haskbox <command> --help' for command-specific help."
      ]
