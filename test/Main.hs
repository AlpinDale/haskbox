module Main (main) where

import Test.Cmd.DfSpec qualified as DfSpec
import Test.Cmd.File qualified as File
import Test.Cmd.Ls qualified as Ls
import Test.Cmd.LsSpec qualified as LsSpec
import Test.Cmd.Misc qualified as Misc
import Test.Cmd.Path qualified as Path
import Test.Cmd.Sort qualified as Sort
import Test.Cmd.System qualified as System
import Test.Cmd.Text qualified as Text
import Test.Tasty
import Test.Utils (withTestEnv)

main :: IO ()
main = withTestEnv $ \env -> do
  defaultMain $
    testGroup
      "haskbox"
      [ Text.tests env,
        Sort.tests env,
        File.tests env,
        Ls.tests env,
        LsSpec.tests env,
        DfSpec.tests env,
        Path.tests env,
        System.tests env,
        Misc.tests env
      ]
