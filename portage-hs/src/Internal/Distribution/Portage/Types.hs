{-|
Module      : Distribution.Portage.Internal.Types

Types for internal use
-}

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

import Control.Applicative (Alternative)
import Data.Data (Data)
import Data.Function (on)
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

instance (Token s ~ Char, MonadParsec e s m) => Parsable Category m s e where
    parserName = "portage category"
    parser = Category . concat <$> wordsWithSep wordStart wordRest (const False)
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

instance forall m s e. (Ord e, Token s ~ Char, Stream s, IsString (Tokens s))
    => Parsable PkgName (ParsecT e s m) s e where
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

instance IsList VersionNum where
    type instance Item VersionNum = NonEmpty Char
    fromList = VersionNum . fromList
    toList = toList . unwrapVersionNum

instance Printable VersionNum where
    toString = L.intercalate "." . NE.toList . fmap NE.toList . unwrapVersionNum

instance
    ( Token s ~ Char
    , MonadParsec e s m
    ) => Parsable VersionNum m s e where
    parserName = "portage version number"
    parser = versionNumParser (`sepBy1` single '.') (NE.fromList <$>)

newtype VersionLetter = VersionLetter
    { unwrapVersionLetter :: Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionLetter where
    toString (VersionLetter l) = [l]

instance (Token s ~ Char, MonadParsec e s m) => Parsable VersionLetter m s e where
    parserName = "portage version letter"
    parser = VersionLetter <$> satisfy isAsciiLower

data VersionSuffix
    = SuffixAlpha
    | SuffixBeta
    | SuffixPre
    | SuffixRC
    | SuffixP
    deriving stock (Show, Eq, Ord, Data, Generic, Bounded, Enum)

instance Printable VersionSuffix where
    toString = \case
        SuffixAlpha -> "alpha"
        SuffixBeta  -> "beta"
        SuffixPre   -> "pre"
        SuffixRC    -> "rc"
        SuffixP     -> "p"

instance
    ( Token s ~ Char
    , IsString (Tokens s)
    , MonadParsec e s m
    ) => Parsable VersionSuffix m s e where
    parserName = "portage version suffix"
    parser = choice
        [ SuffixAlpha <$ string "alpha"
        , SuffixBeta  <$ string "beta"
        , SuffixPre   <$ string "pre"
        , SuffixRC    <$ string "rc"
        , SuffixP     <$ string "p"
        ]

newtype VersionSuffixNum = VersionSuffixNum
    { unwrapVersionSuffixNum :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionSuffixNum where
    toString = NE.toList . unwrapVersionSuffixNum

instance
    ( Token s ~ Char
    , MonadParsec e s m
    , MonadFail m
    ) => Parsable VersionSuffixNum m s e where
    parserName = "portage version suffix number"
    parser = VersionSuffixNum . NE.fromList <$> some (satisfy isDigit)

newtype VersionRevision = VersionRevision
    { unwrapVersionRevision :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionRevision where
    toString (VersionRevision r) = "r" ++ NE.toList r

instance
    ( Token s ~ Char
    , MonadParsec e s m
    , MonadFail m
    ) => Parsable VersionRevision m s e where
    parserName = "portage version revision"
    parser = do
        _ <- single 'r'
        VersionRevision . NE.fromList <$> some (satisfy isDigit)

data Version = Version
    { getVersionNum :: VersionNum
    , getVersionLetter :: Maybe VersionLetter
    , getVersionSuffixes :: [(VersionSuffix, Maybe VersionSuffixNum)]
    , getVersionRevision :: Maybe VersionRevision
    } deriving stock (Show, Eq, Data, Generic)

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

instance
    ( Token s ~ Char
    , IsString (Tokens s)
    , MonadParsec e s m
    , MonadFail m
    ) => Parsable Version m s e where
    parserName = "portage version"
    parser = versionParser parser

data Slot = Slot
    { unwrapSlot :: String
    , getSubSlot :: Maybe SubSlot
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable Slot where
    toString (Slot s mss)
        =  s
        ++ foldMap (\ss -> "/" ++ toString ss) mss

instance (Token s ~ Char, MonadParsec e s m) => Parsable Slot m s e where
    parserName = "portage slot"
    parser = do
        s <- slotParser
        mss <- optional $ try $ single '/' *> parser
        pure $ Slot s mss

newtype SubSlot = SubSlot { unwrapSubSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance (Token s ~ Char, MonadParsec e s m) => Parsable SubSlot m s e where
    parserName = "portage sub-slot"
    parser = SubSlot <$> slotParser

slotParser :: (Token s ~ Char, MonadParsec e s m) => m String
slotParser = concat <$> wordsWithSep wordStart wordRest (const False)
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

instance (Token s ~ Char, IsString (Tokens s), MonadParsec e s m, MonadFail m)
    => Parsable Repository m s e where
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

instance Printable Package where
    toString (Package c n mv ms mr)
        =  toString c
        ++ "/"
        ++ toString n
        ++ foldMap (\v -> "-"  ++ toString v) mv
        ++ foldMap (\s -> ":"  ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr

instance
    ( Ord e
    , Token s ~ Char
    , IsString (Tokens s)
    , Stream s
    ) => Parsable Package (ParsecT e s m) s e where
    parserName = "portage package"
    parser = do
        c <- parser
        _ <- char '/'
        n <- parser
        v <- optional $ try $ string "-"  *> parser
        s <- optional $ try $ string ":"  *> parser
        r <- optional $ try $ string "::" *> parser
        pure $ Package c n v s r

-- | A type of Version that also forms valid PkgName and Repository strings:
--
--   "[A PkgName] must not end in a hyphen followed by anything matching the [Version]
--   syntax"
--   <https://projects.gentoo.org/pms/8/pms.html#x1-180003.1.2>
newtype FauxVersion = FauxVersion
    { unwrapFauxVersion :: Version }
    deriving stock (Show, Eq, Ord)
    deriving newtype Printable

instance
    ( Token s ~ Char
    , IsString (Tokens s)
    , MonadParsec e s m
    , MonadFail m
    ) => Parsable FauxVersion m s e where
    parserName = "faux portage version"
    parser = FauxVersion <$> versionParser (unwrapFauxVersionNum <$> parser)

-- | A type of VersionNum which always has one digit. This avoids having '.' in
--   the resulting string, as this is an invalid character for both PkgName and
--   Category.
newtype FauxVersionNum = FauxVersionNum
    { unwrapFauxVersionNum :: VersionNum }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype Printable

instance
    ( Token s ~ Char
    , MonadParsec e s m
    ) => Parsable FauxVersionNum m s e where
    parserName = "faux portage version number"
    parser = FauxVersionNum <$> versionNumParser id pure

versionParser ::
    ( Token s ~ Char
    , IsString (Tokens s)
    , MonadParsec e s m
    , MonadFail m
    ) => m VersionNum -> m Version
versionParser pn = do
        n <- pn
        l <- optional $ try parser
        s <- many $ do
            _ <- char '_'
            ss <- parser
            sn <- optional $ try parser
            pure (ss, sn)
        r <- optional $ try $ string "-" *> parser
        pure $ Version n l s r

versionNumParser ::
    ( Token s ~ Char
    , MonadParsec e s m
    ) => (m [Char] -> m [a]) -> (NonEmpty a -> NonEmpty (NonEmpty Char)) -> m VersionNum
versionNumParser f g = do
    ns <- f $ some $ satisfy isDigit
    pure $ VersionNum $ g $ NE.fromList ns

-- | Both 'PkgName' and 'Repository' have similar parsers
pkgParser :: forall e s m. (Token s ~ Char, IsString (Tokens s), MonadParsec e s m, MonadFail m)
    => [Char -> Bool]
    -> [Char -> Bool]
    -> m String
pkgParser wordStart wordRest = (:) <$> satisfyAny wordStart <*> goOrEnd
  where
    goOrEnd :: m String
    goOrEnd = try go <|> end

    -- If 'fauxV' gives back a 'Nothing', it means the wole parse ended with
    -- a Version string and we need to throw the error 'e'.
    go :: m String
    go = do
        m <- choice
            [ try fauxV
            , Just <$> nextChar -- 'fauxV' did not match at all :)
            ]
        maybe e pure m

    -- Check for a hyphen followed by something that parses as a 'FauxVersion'.
    -- If this is matched, but there isn't a successful 'go' parser after it,
    -- the parser aborts.
    fauxV :: m (Maybe String)
    fauxV = do
        h <- single '-'
        v <- parser @FauxVersion
        choice
            [ do
                rest <- go -- Don't accept 'end' as a choice at this point
                pure $ Just $ h : toString v ++ rest
            , abort
            ]

    nextChar :: m String
    nextChar = do
        c <- satisfyAny wordRest
        rest <- goOrEnd
        pure $ c : rest

    end :: m String
    end = pure ""

    -- 'show' is used here to wrap the string in quotes
    e :: m String
    e = unexpected $ Label $ NE.fromList $ show $
            "ends in a hyphen followed by anything "
            ++ "matching the version syntax"

    -- This is a trick to abort without jumping to the next 'Alternative'
    -- choice.
    abort :: Alternative f => f (Maybe a)
    abort = pure Nothing
