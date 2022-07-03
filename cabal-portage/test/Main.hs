{-# Language CPP #-}

module Main (main) where

import Test.Tasty

#ifdef GENTOO_TESTS
import Test.Portage
#endif

main :: IO ()
main = defaultMain $
    testGroup "All tests"
        [
#ifdef GENTOO_TESTS
          portageTests
#endif
        ]
