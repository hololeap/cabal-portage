{-# Language TypeApplications #-}

{-# Options_GHC -Wno-deprecations #-} -- Don't nag me about ListT

module Emerge.ParserTests
    ( parserTests
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
-- import Data.Either
import Data.Void
-- import qualified Data.List as L
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

import Data.Parsable
import Internal.Distribution.Portage.Emerge
import Internal.Distribution.Portage.Emerge.Parser

parserTests :: IO TestTree
parserTests = do
    stdouts <- emergeStdOuts
    pure $ testGroup "test against recorded emerge output" $
        map (uncurry stdoutTest) stdouts

stdoutTest :: FilePath -> StdOut -> TestTree
stdoutTest d o = testCase d $ do
    let r = runParser (runParseResult @Void emergeParser) f o
    case r of
        Left e ->
            assertFailure $
                "Could not parse recorded emerge ouptut:\n"
                ++ "Error:\n" ++ errorBundlePretty e
        Right p@(PartialParse _, _) ->
            assertFailure $
                "Got a PartialParse when it should be a CompleteParse:\n"
                ++ "Output: " ++ show p ++ "\n"
        Right (CompleteParse, s) -> forM_ s $ print . toString
  where
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
