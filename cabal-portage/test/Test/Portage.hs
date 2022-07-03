
module Test.Portage (portageTests) where

import Control.Monad.Except
import Data.Either (isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
-- import Text.Pretty.Simple

import Distribution.Cabal.Portage (scanPkgDir)

portageTests :: TestTree
portageTests = testCaseSteps "portage-specific tests" $ \step -> do
    step "check scanPkgDir"
    assertBool "scanPkgDir returns Right" . isRight =<< runExceptT scanPkgDir
