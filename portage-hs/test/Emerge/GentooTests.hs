{-# Language ScopedTypeVariables #-}

module Emerge.GentooTests (gentooTests) where

import qualified Data.Text.IO as T
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Portage.Emerge

gentooTests :: TestTree
gentooTests = testGroup "gentoo system tests"
    [ emergeWorldTest
    ]

-- | Emerge world "test". Currently just writes to
--   @/tmp/emerge-world-test.stdout@,
--   @/tmp/emerge-world-test.stderr@,
--   @/tmp/emerge-world-test.exitcode@
emergeWorldTest :: TestTree
emergeWorldTest = testCase "emerge world \"test\"" $ do
    (c, o, e) <- emergeProcess emergeExe $ upgradeWorldArgs ++ pretendArgs
    T.writeFile "/tmp/emerge-world-test.stdout" o
    T.writeFile "/tmp/emerge-world-test.stderr" e
    writeFile "/tmp/emerge-world-test.exitcode" $ show c
