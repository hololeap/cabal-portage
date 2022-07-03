{-|
Module      : Test.Parsable

Test functions for 'Parsable' and 'Printable'.
-}

{-# Language CPP #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module Test.Parsable
    ( parsableTest
    , parsableProp
    , parsableRoundtrip
    , wordsWithSepGen
    , wordGen
    , ParseErrorBundle(..)
    , module Control.Monad.STM
--     , module Control.Concurrent.STM.TChan
    , module Data.Void
    ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Data.Function (fix)
import qualified Data.List as L
import Data.Typeable
import Data.Void
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Parsable hiding (label)

parsableTest :: forall proxy a.
    ( Parsable a (Parsec Void String) String Void
    , Printable a
    , Arbitrary a
    , Eq a
    , Show a
    , Typeable a
    ) => proxy a -> STM TestTree
parsableTest p = do
    c <- newTChan
    pure $ testGroup (show (typeRep p))
        [ testProperty "parsableProp" (parsableProp @a c)
        ]

parsableProp ::
    ( Parsable a (Parsec Void String) String Void
    , Printable a
    , Eq a
    , Show a
    ) => TChan (ParseErrorBundle String Void) -> a -> Property
parsableProp c x = whenFail (printErrors c) $ idempotentIOProperty $ do

#if defined(VERBOSE_TESTS)
    let (s,r) = parsableRoundtrip x
    _ <- either (atomically . writeTChan c) (const (pure ())) r
    pure $ label s $ r === Right (CompleteParse, x)
#else
    let (_,r) = parsableRoundtrip x
    _ <- either (atomically . writeTChan c) (const (pure ())) r
    pure $ r === Right (CompleteParse, x)
#endif

parsableRoundtrip ::
    ( Parsable a (Parsec Void String) String Void
    , Printable a
    ) => a -> (String, Either (ParseErrorBundle String Void) (ParseCoverage, a))
parsableRoundtrip x =
    let s = toString x
    in (s, runParsable "" s)

wordsWithSepGen
    :: [Char -> Bool]
    -> [Char -> Bool]
    -> (Char -> Bool)
    -> Gen String
wordsWithSepGen wordStart wordRest wordSep = do
    pieces <- listOf1 $ wordGen wordStart wordRest
    s <- arbitrary `suchThat` wordSep
    pure $ L.intercalate [s] pieces

wordGen :: [Char -> Bool] -> [Char -> Bool] -> Gen String
wordGen wordStart wordRest = do
    c  <-          arbitrary `suchThat` anySat wordStart
    cs <- listOf $ arbitrary `suchThat` anySat wordRest
    pure $ c : cs
  where
    anySat :: [Char -> Bool] -> Char -> Bool
    anySat l x = or [f x | f <- l]


printErrors :: TChan (ParseErrorBundle String Void) -> IO ()
printErrors c = fix $ \loop -> do
    me <- atomically $ tryReadTChan c
    case me of
        Just e  -> putStrLn (errorBundlePretty e) *> loop
        Nothing -> pure ()
