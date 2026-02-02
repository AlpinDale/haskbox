module Version (version, versionString) where

version :: String
version = "0.1.0"

versionString :: String -> String
versionString cmd = cmd ++ " (haskbox) " ++ version
