module Cmd.Hostid (run) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Word (Word32)
import Numeric (showHex)
import System.Exit (exitSuccess)
import Version (versionString)

run :: [String] -> IO ()
run args = case args of
  ["--help"] -> printHelp >> exitSuccess
  ["--version"] -> putStrLn (versionString "hostid") >> exitSuccess
  _ -> do
    hostid <- getHostId
    putStrLn $ padLeft 8 '0' $ showHex hostid ""

getHostId :: IO Word32
getHostId = catch readHostId handler
  where
    readHostId = do
      contents <- BS.readFile "/etc/hostid"
      -- /etc/hostid contains a 4-byte little-endian integer
      if BS.length contents >= 4
        then do
          let b0 = fromIntegral (BS.index contents 0) :: Word32
              b1 = fromIntegral (BS.index contents 1) :: Word32
              b2 = fromIntegral (BS.index contents 2) :: Word32
              b3 = fromIntegral (BS.index contents 3) :: Word32
          return $ b0 + b1 * 256 + b2 * 65536 + b3 * 16777216
        else return 0

    handler :: IOException -> IO Word32
    handler _ = return 0

padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (n - length s) c ++ s

printHelp :: IO ()
printHelp =
  putStr $
    unlines
      [ "Usage: haskbox hostid [OPTION]",
        "Print the numeric identifier (in hexadecimal) for the current host.",
        "",
        "      --help       display this help and exit",
        "      --version    output version information and exit"
      ]
