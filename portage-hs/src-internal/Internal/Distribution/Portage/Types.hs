{-|
Module      : Distribution.Portage.Internal.Types

Types for internal use
-}

{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Internal.Distribution.Portage.Types
    (
    -- * Types
      Package(..)
    , Category(..)
    , PkgName(..)
    , Slot(..)
    , SubSlot(..)
    , Repository(..)
--     , EBuildFileName (..)
    -- ** Versions
    , Version(..)
    , VersionNum(..)
    , VersionLetter(..)
    , VersionSuffix(..)
    , VersionSuffixNum(..)
    , VersionRevision(..)
    -- * Internal
    , FauxVersion(..)
    , FauxVersionNum(..)
    ) where

import Control.Applicative (Alternative, some)
import Data.Data (Data)
import Data.Function (on)
import Data.Hashable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Data.Parsable

newtype Category = Category
    { unwrapCategory :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)
    deriving anyclass Hashable

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
    deriving anyclass Hashable

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

newtype VersionNum = VersionNum
    { unwrapVersionNum :: NonEmpty (NonEmpty Char) }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving anyclass Hashable

instance IsList VersionNum where
    type instance Item VersionNum = NonEmpty Char
    fromList = VersionNum . fromList
    toList = toList . unwrapVersionNum

instance Printable VersionNum where
    toString = L.intercalate "." . NE.toList . fmap NE.toList . unwrapVersionNum

instance Stream s m Char => Parsable VersionNum m u s where
    parserName = "portage version number"
    parser = versionNumParser (`sepBy1` char '.') (NE.fromList <$>)

newtype VersionLetter = VersionLetter
    { unwrapVersionLetter :: Char }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving anyclass Hashable

instance Printable VersionLetter where
    toString (VersionLetter l) = [l]

instance Stream s m Char => Parsable VersionLetter m u s where
    parserName = "portage version letter"
    parser = VersionLetter <$> satisfy isAsciiLower

data VersionSuffix
    = SuffixAlpha
    | SuffixBeta
    | SuffixPre
    | SuffixRC
    | SuffixP
    deriving stock (Show, Eq, Ord, Data, Generic, Bounded, Enum)
    deriving anyclass Hashable

instance Printable VersionSuffix where
    toString = \case
        SuffixAlpha -> "alpha"
        SuffixBeta  -> "beta"
        SuffixPre   -> "pre"
        SuffixRC    -> "rc"
        SuffixP     -> "p"

instance Stream s m Char => Parsable VersionSuffix m u s where
    parserName = "portage version suffix"
    parser = choice
        [ SuffixAlpha <$ try (string "alpha")
        , SuffixBeta  <$ try (string "beta")
        , SuffixPre   <$ try (string "pre")
        , SuffixRC    <$ try (string "rc")
        , SuffixP     <$ string "p"
        ]

newtype VersionSuffixNum = VersionSuffixNum
    { unwrapVersionSuffixNum :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving anyclass Hashable

instance Printable VersionSuffixNum where
    toString = NE.toList . unwrapVersionSuffixNum

instance Stream s m Char => Parsable VersionSuffixNum m u s where
    parserName = "portage version suffix number"
    parser = VersionSuffixNum . NE.fromList <$> some (satisfy isDigit)

newtype VersionRevision = VersionRevision
    { unwrapVersionRevision :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving anyclass Hashable

instance Printable VersionRevision where
    toString (VersionRevision r) = "r" ++ NE.toList r

instance Stream s m Char => Parsable VersionRevision m u s where
    parserName = "portage version revision"
    parser = do
        _ <- char 'r'
        VersionRevision . NE.fromList <$> some (satisfy isDigit)

data Version = Version
    { getVersionNum :: VersionNum
    , getVersionLetter :: Maybe VersionLetter
    , getVersionSuffixes :: [(VersionSuffix, Maybe VersionSuffixNum)]
    , getVersionRevision :: Maybe VersionRevision
    } deriving stock (Show, Eq, Data, Generic)
    deriving anyclass Hashable

instance Ord Version where
    Version n1 l1 s1 r1 `compare` Version n2 l2 s2 r2
        =  n1 `compare` n2
        <> l1 `compare` l2
        <> s1 `compareSuffixes` s2
        <> r1 `compareRevisions` r2
      where
        compareSuffixes x y = case (null x, null y) of
            (True, False) -> GT -- Empty list of suffixes is greater than non-empty list
            (False, True) -> LT -- non-empty list of suffixes is less than empty list
            _             -> L.sort x `compare` L.sort y
        compareRevisions = compare `on`
            maybe (0 :: Natural) (read . NE.toList . unwrapVersionRevision)

instance Printable Version where
    toString (Version n l s r)
        = toString n
        ++ foldMap toString l
        ++ (s >>= suffixString)
        ++ foldMap (\x -> "-" ++ toString x) r
      where
        suffixString (ss,sn) = "_" ++ toString ss ++ foldMap toString sn

instance Stream s m Char => Parsable Version m u s where
    parserName = "portage version"
    parser = versionParser parser

data Slot = Slot
    { unwrapSlot :: String
    , getSubSlot :: Maybe SubSlot
    }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving anyclass Hashable

instance Printable Slot where
    toString (Slot s mss)
        =  s
        ++ foldMap (\ss -> "/" ++ toString ss) mss

instance Stream s m Char => Parsable Slot m u s where
    parserName = "portage slot"
    parser = do
        s <- slotParser
        mss <- optionMaybe $ try $ char '/' *> parser
        pure $ Slot s mss

newtype SubSlot = SubSlot { unwrapSubSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)
    deriving anyclass Hashable

instance Stream s m Char => Parsable SubSlot m u s where
    parserName = "portage sub-slot"
    parser = SubSlot <$> slotParser

slotParser :: Stream s m Char => ParsecT s u m String
slotParser = wordAllowed wordStart wordRest
  where
    wordStart =
        [ isAsciiUpper
        , isAsciiLower
        , isDigit
        , (== '_')
        ]
    wordRest = wordStart ++
        [ (== '+')
        , (== '.')
        , (== '-')
        ]


newtype Repository = Repository { unwrapRepository :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)
    deriving anyclass Hashable

instance Stream s m Char => Parsable Repository m u s where
    parserName = "portage repository"
    parser = Repository <$> pkgParser wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]

        wordRest = (== '-') : wordStart

data Package = Package
    { getCategory   :: Category
    , getPkgName    :: PkgName
    , getVersion    :: Maybe Version
    , getSlot       :: Maybe Slot
    , getRepository :: Maybe Repository
    }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving anyclass Hashable

instance Printable Package where
    toString (Package c n mv ms mr)
        =  toString c
        ++ "/"
        ++ toString n
        ++ foldMap (\v -> "-"  ++ toString v) mv
        ++ foldMap (\s -> ":"  ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr

instance Stream s m Char => Parsable Package m u s where
    parserName = "portage package"
    parser = do
        c <- parser
        _ <- char '/'
        n <- parser
        v <- optionMaybe $ try $ string "-"  *> parser
        s <- optionMaybe $ try $ string ":"  *> parser
        r <- optionMaybe $ try $ string "::" *> parser
        pure $ Package c n v s r

-- data EBuildFileName = EBuildFileName
--     { ebuildName :: PkgName
--     , ebuildVersion :: Version
--     }
--     deriving stock (Show, Eq, Ord, Data, Generic)
--     deriving anyclass Hashable
--
-- instance Printable EBuildFileName where
--     toString (EBuildFileName n v)
--         =  toString n
--         ++ "-"
--         ++ toString v
--         ++ ".ebuild"
--
-- instance Stream s m Char => Parsable EBuildFileName m u s  where
--     parserName = "portage ebuild file name"
--     parser = do
--         n <- parser
--         _ <- char '-'
--         v <- parser
--         _ <- string ".ebuild"
--         pure $ EBuildFileName n v

-- | A type of Version that also forms valid PkgName and Repository strings:
--
--   "[A PkgName] must not end in a hyphen followed by anything matching the [Version]
--   syntax"
--   <https://projects.gentoo.org/pms/8/pms.html#x1-180003.1.2>
newtype FauxVersion = FauxVersion
    { unwrapFauxVersion :: Version }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype Printable
    deriving anyclass Hashable

instance Stream s m Char => Parsable FauxVersion m u s where
    parserName = "faux portage version"
    parser = FauxVersion <$> versionParser (unwrapFauxVersionNum <$> parser)

-- | A type of VersionNum which always has one digit. This avoids having '.' in
--   the resulting string, as this is an invalid character for both PkgName and
--   Category.
newtype FauxVersionNum = FauxVersionNum
    { unwrapFauxVersionNum :: VersionNum }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype Printable
    deriving anyclass Hashable

instance Stream s m Char => Parsable FauxVersionNum m u s where
    parserName = "faux portage version number"
    parser = FauxVersionNum <$> versionNumParser id pure

versionParser :: Stream s m Char
    => ParsecT s u m VersionNum -> ParsecT s u m Version
versionParser pn = do
        n <- pn
        l <- optionMaybe $ try parser
        s <- many $ do
            _ <- char '_'
            ss <- parser
            sn <- optionMaybe $ try parser
            pure (ss, sn)
        r <- optionMaybe $ try $ string "-" *> parser
        pure $ Version n l s r

versionNumParser :: Stream s m Char
    => (ParsecT s u m [Char] -> ParsecT s u m [a])
    -> (NonEmpty a -> NonEmpty (NonEmpty Char))
    -> ParsecT s u m VersionNum
versionNumParser f g = do
    ns <- f $ some $ satisfy isDigit
    pure $ VersionNum $ g $ NE.fromList ns

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
