{-|
Module      : Data.Parsable

This module contains two simple classes, 'Parsable' and 'Printable'.

There is an implicit "soft isomorphism" between 'parser' and 'toString'.
(Successfully parsing a string and then running 'toString' on the result
should result in the original string.)

Data returned from 'parser' must be wrapped inside 'ParseResult'. This can
be done easily with the 'checkCoverage' function.

For instance:

> parser = checkCoverage $ count 5 anyChar

Note that 'runParseResult' will throw an error if no 'ParseCoverage' has
been written (e.g. only functions like 'pure', 'lift', etc. have been called).

=== Language extensions

Because 'parser' does not take any arguments, it may be necessary
to explicitly declare the type of @t@ for these functions.

It may be helpful to enable and use the @TypeApplications@ and possibly
@ScopedTypeVariables@ extensions..

Look at the @Language Extensions@ section of the GHC documentation for
instructions on how to use these extensions.
-}

{-# Language DefaultSignatures #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language QuantifiedConstraints #-}
{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}

module Data.Parsable
    (
    -- * Parsing
      Parsable(..)
    , ParserName(..)
    , runParsableT
    , runParsable
    -- ** Wrappers
    , NaturalParsable(..)
    -- * Partial parses
    , ParseCoverage(..)
    -- ** As a Monad
    , ParseResult
    , runParseResult
    -- * Parsing functions
    , satisfyAny
    , someAllowed
    , wordsWithSep
    , wordsWithSep'
    , checkCoverage
    , readParsec
    -- * Printing
    , Printable(..)
    , toText
    -- ** Wrappers
    , ShowPrintable(..)
    -- * Re-exports
    , module Control.Monad
    , module Control.Monad.Trans.Class
    , module Data.Char
    , module Data.String
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import Control.Applicative hiding (some, many)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict (WriterT(..))
import Data.Char
import Data.Data
import Data.Functor.Identity
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Semigroup (Last(..))
import Data.String
import Data.Text (Text, unpack)
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

-- | If a parse succeeds for the beginning of the input, but then fails, we
--   choose the 'PartialParse' constructor. If the entire parse was successful,
--   we choose 'CompleteParse'.
--
--   This is mostly useful for testing parsers where we need to test them
--   individually (must end with 'CompleteParse') and also composed together
--   (individual parsers may end with 'PartialParse' if they do not conclude
--   the larger parse).
data ParseCoverage
    = PartialParse String
    | CompleteParse
    deriving stock (Show, Eq, Ord)
    deriving Semigroup via Last ParseCoverage

-- | Derives most instances via
--   @('WriterT' ('Maybe' 'ParseCoverage') m a)@
newtype ParseResult e s m a = ParseResult
    { unwrapParseResult :: m (a, Maybe ParseCoverage) }
    deriving stock Functor
    deriving (Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadParsec e s)
        via WriterT (Maybe ParseCoverage) m
    deriving MonadTrans via WriterT (Maybe ParseCoverage)

-- | Extract the result of a 'ParseResult' computation.
--
--   This will throw an error if no 'ParseCoverage' has been written
--   (e.g. only functions like 'pure', 'lift', etc. have been called).
runParseResult :: MonadFail m
    => ParseResult e s m a
    -> m (ParseCoverage, a)
runParseResult = go <=< unwrapParseResult
  where
    go (x, Just c ) = pure (c, x)
    go (_, Nothing) = fail "runParseResult was called, but no ParseCoverage was created!"
{-# Inline runParseResult #-}

newtype ParserName a e s (m :: Type -> Type) = ParserName { getParserName :: String }
    deriving stock (Show, Eq, Ord)
    deriving newtype IsString

-- | Represents types that have a valid Parsec parser.
class MonadParsec e s m => Parsable a (m :: Type -> Type) s e where
    parser :: ParseResult e s m a
    parserName :: ParserName a e s m
    {-# Minimal parser, parserName #-}

newtype NaturalParsable a = NaturalParsable
    { unwrapNaturalParsable :: a }

instance (MonadParsec e s m, MonadFail m, Token s ~ Char, Read a, Integral a, Typeable a)
    => Parsable (NaturalParsable a) m s e where
    parserName = "natural number"
    parser = checkCoverage $ (<?> "natural number") $ do
        ds <- some (satisfy isDigit)
        case readMaybe ds of
            Just i -> pure $ NaturalParsable i
            Nothing -> fail $ "Could not parse as " ++ t ++ ": " ++ show ds
      where
        t = show $ typeRep $ Proxy @a

-- | Convenience function to run a 'Parsable' parser.
runParsableT :: forall a m s e. (Parsable a (ParsecT e s m) s e, Monad m)
    => String -> s -> m (Either (ParseErrorBundle s e) (ParseCoverage, a))
runParsableT = runParserT $ runParseResult (parser <?> n)
    where n = getParserName $ parserName @a @(ParsecT e s m)

runParsable :: forall a e s. Parsable a (ParsecT e s Identity) s e
    => String -> s -> Either (ParseErrorBundle s e) (ParseCoverage, a)
runParsable = runParser $ runParseResult (parser <?> n)
    where n = getParserName $ parserName @a @(ParsecT e s Identity)

-- | Pass a previously-parsed string to this function in order to attempt
--   using 'read'. Produces proper error messages on failure.
readParsec :: forall a e s m. (MonadParsec e s m, MonadFail m, Typeable a, Read a) => String -> m a
readParsec s = (<?> typeName ++ " (Read instance)") $
    case readMaybe s of
        Just x -> pure x
        Nothing -> fail
            $  "unable to parse using "
            ++ typeName
            ++ " Read instance: "
            ++ show s
    where typeName = show $ typeRep $ Proxy @a

-- | Parse a Char that satisfies any of the given predicates
satisfyAny :: MonadParsec e s m => [Token s -> Bool] -> m (Token s)
satisfyAny fs = satisfy $ \c -> or [f c | f <- fs]

-- | One or more characters that satisfy any of the given predicates
someAllowed :: (MonadParsec e s m, Token s ~ Char)
    => [Char -> Bool]
    -> ParseResult e s m String
someAllowed allowed = wordsWithSep allowed allowed []

-- | Best effort parsing of "words" starting with certain allowed characters,
--   then multiple of characters allowed after the start of a word.
--
--   Words are separated by certain allowed characters, such as @\'-\'@ and
--   @\'_\'@.
--
--   Example:
--
--   > wordsWithSep [isAsciiUpper] [isAsciiUpper, isAsciiLower] [(== '-')]
--
--   This would parse a string such as @"SomeAwesome-Thing"@:
--
--       * The first character of each word must be an upper-case ASCII letter.
--
--       * The remaining characters of each word may be upper- or lower-case
--         letters.
--
--       * Words are separated by @'-'@ characters.
--
--  If a @wordsWithSep@ parser fails, it will return v'PartialParse' wrapping
--  the string up to and including the last successfully parsed word.
wordsWithSep :: (MonadParsec e s m, Token s ~ Char)
    => [Char -> Bool]    -- ^ Characters allowed at the start of a word
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a word
    -> [Char -> Bool]    -- ^ Characters that separate words
    -> ParseResult e s m String
wordsWithSep = wordsWithSepG Nothing

-- | The same as 'wordsWithSep', but this takes a parser which will be run in
--   place of normal word detection for the first word. This allows for
--   starting with generally unallowed characters before the first separator.
wordsWithSep' :: (MonadParsec e s m, Token s ~ Char)
    => m String -- ^ Beginning of parser, skips normal beginning
    -> [Char -> Bool]    -- ^ Characters allowed at the start of a word
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a word
    -> [Char -> Bool]    -- ^ Characters that separate words
    -> ParseResult e s m String
wordsWithSep' = wordsWithSepG . Just

-- | General version of 'wordsWithSep'. Not exported.
wordsWithSepG :: (MonadParsec e s m, Token s ~ Char)
    -- | Optional beginning of parser, skips normal beginning
    => Maybe (m String)
    -> [Char -> Bool]    -- ^ Characters allowed at the start of a word
    -> [Char -> Bool]    -- ^ Characters allowed after the start of a word
    -> [Char -> Bool]    -- ^ Characters that separate words
    -> ParseResult e s m String
wordsWithSepG maybeBeg wordStart wordRest wordSep = do
    let beg = flip fromMaybe maybeBeg $ do
                    w <- some $ satisfyAny wordStart
                    r <- many $ satisfyAny wordRest
                    pure $ w ++ r
    choice
        [ try $ do
            b    <- lift beg
            sep  <- satisfyAny wordSep
            next <- wordsWithSepG Nothing wordStart wordRest wordSep
            pure $ b ++ [sep] ++ next
        , checkCoverage beg
        ]

-- | Run the specified parser, then write 'CompleteParse' if we are at
--   'eof', otherwise v'PartialParse'.
checkCoverage :: (MonadParsec e s m, Token s ~ Char)
    => m a
    -> ParseResult e s m a
checkCoverage p = ParseResult $ p >>= \x ->
    (x,) . Just <$> choice
        [ CompleteParse <$ eof
        , PartialParse <$> lookAhead (some anySingle)
        ]

-- | Types that can be converted back to a @String@.
class Printable t where
    toString :: t -> String

    default toString :: Show t => t -> String
    toString = show
    {-# Minimal toString #-}

instance Printable String where
    toString = id

instance Printable Text where
    toString = unpack

-- | Convenience function that will turn a 'Printable' to any 'IsString'.
toText :: (Printable t, IsString s) => t -> s
toText = fromString . toString

-- | Wrapper for types that inherit 'toString' directly from their 'Show' instance.
--
--   It is convenient to use the @DerivingVia@ language extension with this.
--
--   > {-# Language DerivingVia #-}
--   >
--   > newtype MyNum Int
--   >     deriving Printable via (ShowPrintable Int) -- Uses Show instance of Int
newtype ShowPrintable a = ShowPrintable
    { unwrapShowPrintable :: a }
    deriving stock
        ( Read, Show, Eq, Ord, Bounded, Functor, Foldable, Traversable, Generic
        , Generic1, Data
        )
    deriving newtype (Enum, Num, Fractional, Floating, IsString, Semigroup, Monoid)
    deriving (Applicative, Monad) via Identity

-- | Uses 'show' after unwrapping the contents.
instance Show a => Printable (ShowPrintable a) where
    toString = show . unwrapShowPrintable
