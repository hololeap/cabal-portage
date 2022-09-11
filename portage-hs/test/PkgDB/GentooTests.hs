
module PkgDB.GentooTests
    ( gentooTests
    ) where

import Internal.Distribution.Portage.PkgDB

import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple

gentooTests :: TestTree
gentooTests = testCase "walk /var/db/pkg" $
    pkgDbEntries >>= pPrintForceColor
