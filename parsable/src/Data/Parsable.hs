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
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language QuantifiedConstraints #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Data.Parsable
    (
    -- * Parsing
      Parsable(..)
    , ParserName(..)
    , runParsableT
    , runParsable
    -- ** Wrappers
    , NaturalParsable(..)
    -- * Parsing functions
    , satisfyAny
    , wordAllowed
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
    , module Data.Functor.Identity
    , module Data.String
    , module Text.Parsec
    , module Text.Parsec.Char
    ) where

import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Char
import Data.Data
import Data.Functor.Identity
import Data.Kind
import Data.String
import Data.Text (Text, unpack)
import GHC.Generics
import Text.Parsec
import Text.Parsec.Char
import Text.Read (readMaybe)

newtype ParserName a s u (m :: Type -> Type) = ParserName { getParserName :: String }
    deriving stock (Show, Eq, Ord)
    deriving newtype IsString

-- | Represents types that have a valid Parsec parser.
class Parsable a (m :: Type -> Type) u s where
    parser :: ParsecT s u m a
    parserName :: ParserName a s u m
    {-# Minimal parser, parserName #-}

newtype NaturalParsable a = NaturalParsable
    { unwrapNaturalParsable :: a }

instance (Stream s m Char, Read a, Typeable a)
    => Parsable (NaturalParsable a) m u s where
    parserName = "natural number"
    parser = (<?> "natural number") $ do
        ds <- some (satisfy isDigit)
        case readMaybe ds of
            Just i -> pure $ NaturalParsable i
            Nothing -> fail $ "Could not parse as " ++ t ++ ": " ++ show ds
      where
        t = show $ typeRep $ Proxy @a

-- | Convenience function to run a 'Parsable' parser.
runParsableT :: forall a m s. (Stream s m Char, Parsable a m () s)
    => String -> s -> m (Either ParseError a)
runParsableT = runParserT (parser <?> n) ()
    where n = getParserName $ parserName @a @m @() @s

runParsable :: forall a s. (Stream s Identity Char, Parsable a Identity () s)
    => String -> s -> Either ParseError a
runParsable = runParser (parser <?> n) ()
    where n = getParserName $ parserName @a @Identity @() @s

-- | Pass a previously-parsed string to this function in order to attempt
--   using 'read'. Produces proper error messages on failure.
readParsec :: forall a s u m. (Typeable a, Read a) => String -> ParsecT s u m a
readParsec s = (<?> typeName ++ " (Read instance)") $
    case readMaybe s of
        Just x -> pure x
        Nothing -> fail
            $  "unable to parse using "
            ++ typeName
            ++ " Read instance: "
            ++ show s
    where typeName = show $ typeRep $ Proxy @a

-- | Parse a token that satisfies any of the given predicates
satisfyAny :: Stream s m Char => [Char -> Bool] -> ParsecT s u m Char
satisfyAny fs = satisfy $ \c -> or [f c | f <- fs]

-- | Parsing of "words" which require a list of predicates for the first
--   token, and a list of predicates for any remaining tokens. This always
--   parses at least one token.
wordAllowed :: Stream s m Char
    => [Char -> Bool] -- ^ Tokens that start the word
    -> [Char -> Bool] -- ^ Any subsequent tokens
    -> ParsecT s u m [Char]
wordAllowed beg rest = (:) <$> satisfyAny beg <*> many (satisfyAny rest)

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
