{-|
Module      : Test.Parsable

Test functions for 'Parsable' and 'Printable'.
-}

{-# Language CPP #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}

module Test.Parsable
    ( parsableTest
    , parsableProp
    , parsableRoundtrip
    , wordsWithSepGen
    , wordGen
    , checkParsable
    , checkCoverage
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
-- import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck hiding (checkCoverage)

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

parsableRoundtrip :: forall a.
    ( Parsable a (Parsec Void String) String Void
    , Printable a
    ) => a -> (String, Either (ParseErrorBundle String Void) (ParseCoverage, a))
parsableRoundtrip x = (s, runParser checkParsable "" s)
    where s = toString x

wordsWithSepGen
    :: [Char -> Bool]
    -> [Char -> Bool]
    -> (Char -> Bool)
    -> Gen String
wordsWithSepGen wordStart wordRest wordSep = do
    pieces <- listOf1 $ wordGen wordStart wordRest
    s <- arbitrary `suchThat` wordSep
    pure $ L.intercalate [s] pieces

checkParsable :: forall a e s m. (Parsable a m s e, Printable (Tokens s)) =>
    m (ParseCoverage, a)
checkParsable = checkCoverage (parser <?> n)
    where n = getParserName $ parserName @a @m

-- | Run the specified parser, then write 'CompleteParse' if we are at
--   'eof', otherwise v'PartialParse'.
checkCoverage :: (Printable (Tokens s), MonadParsec e s m)
    => m a
    -> m (ParseCoverage, a)
checkCoverage p = do
    x <- p
    (,x) <$> choice
        [ CompleteParse <$ eof
        , PartialParse . toString <$> lookAhead (takeWhileP Nothing (const True))
        ]

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
