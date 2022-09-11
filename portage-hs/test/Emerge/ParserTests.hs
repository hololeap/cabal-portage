{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}

{-# Options_GHC -Wno-deprecations #-} -- Don't nag me about ListT

module Emerge.ParserTests
    ( parserTests
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
-- import Text.Megaparsec

import Data.Parsable
import Internal.Distribution.Portage.Types
import Internal.Distribution.Portage.Emerge
import Internal.Distribution.Portage.Emerge.Parser
import Test.Parsable

parserTests :: IO TestTree
parserTests = do
    stdouts <- emergeStdOuts
    pure $ testGroup "test against recorded emerge output" $
        map (uncurry stdoutTest) stdouts

stdoutTest :: FilePath -> StdOut -> TestTree
stdoutTest d o =
    case ecis of
        Left e ->
            testCase d $ assertFailure $
                "Could not parse recorded emerge ouptut:\n"
                ++ "Error:\n" ++ show e
        Right (c, (i,ps)) -> testGroup d
            [ testCase "complete coverage" (checkPC c)
            , testCase "package count" (checkCount i ps)
            , testGroup "round-trip on extracted packages" $
                printableHUnit <$> S.toList ps
            ]
  where
    checkPC :: ParseCoverage -> Assertion
    checkPC = \case
        c@(PartialParse _) ->
            assertFailure $
                "Got a PartialParse when it should be a CompleteParse:\n"
                ++ show c ++ "\n"
        _ -> pure ()

    checkCount :: Integer -> Set Package -> Assertion
    checkCount i ps = assertEqual "reported package count should equal set size" (fromInteger i) (S.size ps)

    ecis :: Either ParseError (ParseCoverage, (Integer, Set Package))
    ecis = runParser (checkCoverage emergeParser) () f o

    f = "test" </> "data" </> d </> "emerge-world-test.stdout"

emergeStdOuts :: IO [(FilePath, StdOut)]
emergeStdOuts = runListT $ do
    projectDir <- lift $ getCurrentDirectory >>= canonicalizePath
    let dataDir = projectDir </> "test" </> "data"

    d <- ListT $ listDirectory dataDir
    f <- ListT $ listDirectory $ dataDir </> d

    guard $ f == "emerge-world-test.stdout"

    o <- lift $ T.readFile $ dataDir </> d </> f

    pure (d, o)
