{-# Language CPP #-}

module Main (main) where

import Test.Tasty

import Test.Parsable

import qualified Emerge.ParserTests as Emerge
import qualified Types.ValidityTests as Types
import qualified Types.UnitTests as Types
#if defined(GENTOO_TESTS)
--import qualified Emerge.GentooTests as Emerge
import qualified Types.GentooTests as Types
#endif

main :: IO ()
main = do
    typesTests <- sequenceA
        [ atomically Types.validityTests
        , pure Types.unitTests
#if defined(GENTOO_TESTS)
        , Types.gentooTests
#endif
        ]

    emergeTests <- sequenceA
        [ Emerge.parserTests
#if defined(GENTOO_TESTS)
--        , pure Emerge.gentooTests
#endif
        ]

    defaultMain $ testGroup "portage-hs tests"
        [ testGroup "portage types" typesTests
        , testGroup "emerge" emergeTests
        ]
