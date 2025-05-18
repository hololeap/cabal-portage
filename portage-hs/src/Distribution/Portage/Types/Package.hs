{-|
Module      : Distribution.Portage.Types

Types for Portage basic portage package atoms (category/name)
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module Distribution.Portage.Types.Package
(     -- * Packages
      Package(..)
    , Category(..)
    , PkgName(..)
      -- * Internal
    , pkgParser
    ) where

import Control.Applicative (Alternative)
import Data.Data (Data)
import GHC.Generics (Generic)

import Data.Parsable

import Distribution.Portage.Types.Version

data Package = Package
    { getCategory   :: Category
    , getPkgName    :: PkgName
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable Package where
    toString (Package c n)
        =  toString c
        ++ "/"
        ++ toString n

instance Stream s m Char => Parsable Package m u s where
    parserName = "portage package"
    parser = do
        c <- parser
        _ <- char '/'
        n <- parser
        pure $ Package c n

newtype Category = Category
    { unwrapCategory :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Stream s m Char => Parsable Category m u s where
    parserName = "portage category"
    parser = Category <$> wordAllowed wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = wordStart ++
            [ (== '+')
            , (== '-')
            , (== '.')
            ]

newtype PkgName = PkgName
    { unwrapPkgName :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance forall m u s. Stream s m Char => Parsable PkgName m u s where
    parserName = "portage package name"
    parser = PkgName <$> pkgParser wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = (== '+') : (== '-') : wordStart

------
-- Internal
------

-- | Both 'PkgName' and 'Repository' have similar parsers
pkgParser :: forall s u m. Stream s m Char
    => [Char -> Bool]
    -> [Char -> Bool]
    -> ParsecT s u m String
pkgParser wordStart wordRest = (:) <$> satisfyAny wordStart <*> goOrEnd
  where
    goOrEnd :: ParsecT s u m String
    goOrEnd = try go <|> end

    -- If 'fauxV' gives back a 'Nothing', it means the wole parse ended with
    -- a Version string and we need to throw the error 'e'.
    go :: ParsecT s u m String
    go = do
        m <- choice
            [ try fauxV
            , Just <$> nextChar -- 'fauxV' did not match at all :)
            ]
        maybe e pure m

    -- Check for a hyphen followed by something that parses as a 'FauxVersion'.
    -- If this is matched, but there isn't a successful 'go' parser after it,
    -- the parser aborts.
    fauxV :: ParsecT s u m (Maybe String)
    fauxV = do
        h <- char '-'
        v <- parser @FauxVersion
        choice
            [ try $ do
                rest <- go -- Don't accept 'end' as a choice at this point
                pure $ Just $ h : toString v ++ rest
            , abort
            ]

    nextChar :: ParsecT s u m String
    nextChar = do
        c <- satisfyAny wordRest
        rest <- goOrEnd
        pure $ c : rest

    end :: ParsecT s u m String
    end = pure ""

    -- 'show' is used here to wrap the string in quotes
    e :: ParsecT s u m String
    e = fail $
            "ends in a hyphen followed by anything "
            ++ "matching the version syntax"

    -- This is a trick to abort without jumping to the next 'Alternative'
    -- choice.
    abort :: Alternative f => f (Maybe a)
    abort = pure Nothing
