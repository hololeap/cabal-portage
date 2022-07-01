{-# Language DerivingStrategies #-}
{-# Language InstanceSigs #-}
{-# Language TypeApplications #-}

{-# Options_GHC -Wno-orphans #-}

module Main (main) where

import Data.Parsable
import Data.Proxy
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (ParseResult a) where
    arbitrary :: Gen (ParseResult a)
    arbitrary = do
        x1 <- arbitrary
        x2 <- arbitrary
        s <- listOf chooseAny
        elements [CompleteParse x1, PartialParse s x2]

main :: IO ()
main = defaultMain $ testGroup "parsable tests"
    [ classesTest
    ]

classesTest :: TestTree
classesTest = testProperty "quickcheck-classes" $ ioProperty $
    mapM_ (\f -> lawsCheck $ f $ Proxy @ParseResult)
        [ functorLaws
        , foldableLaws
        , traversableLaws
        ]
