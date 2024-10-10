module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [
        testCase "running job" $ do
          ref <- newIORef False
          spc <- startSPC
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r <- jobWait spc j
          r @?= Just Done
          x <- readIORef ref
          x @?= True
      ]
