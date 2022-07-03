{-# Language CPP #-}

module Main (main) where

import Test.Tasty

import Test.Parsable

import ValidityTests
import UnitTests
#if defined(GENTOO_TESTS)
import GentooTests
#endif

main :: IO ()
main = do
    tests <- sequenceA
        [ atomically validityTests
        , pure unitTests
#if defined(GENTOO_TESTS)
        , gentooTests
#endif
        ]
    defaultMain $ testGroup "portage-hs tests" tests
