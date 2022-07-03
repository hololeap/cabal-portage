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

module Distribution.Portage.Types.Internal
    (
    -- * Types
      Package(..)
    , Category(..)
    , PkgName(..)
    , Slot(..)
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

-- import Control.Monad.Trans.Writer.CPS
import Data.Data (Data)
import Data.Function (on)
import qualified Data.List as L
-- import Data.Maybe (fromMaybe)
-- import Data.Monoid (Any(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
-- import qualified Data.Set as S
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
    parser = PkgName <$> pkgParser wordStart wordRest wordSep
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = (== '+') : wordStart
        wordSep = '-'

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
    parser = checkCoverage $
        VersionLetter <$> satisfy isAsciiLower

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
    parser = checkCoverage $ choice
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
    parser = checkCoverage $
        VersionSuffixNum . NE.fromList <$> some (satisfy isDigit)

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
    parser = checkCoverage $ do
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

newtype Slot = Slot { unwrapSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance (Token s ~ Char, MonadParsec e s m) => Parsable Slot m s e where
    parserName = "portage slot"
    parser = do
        Slot . concat <$> wordsWithSep wordStart wordRest (const False)
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
    parser = Repository <$> pkgParser letters letters wordSep
      where
        letters =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordSep = '-'

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
        ++ foldMap (\x -> "-"  ++ toString x) mv
        ++ foldMap (\x -> ":"  ++ toString x) ms
        ++ foldMap (\x -> "::" ++ toString x) mr

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
    ) => ParseResult e s m VersionNum -> ParseResult e s m Version
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
    ) => (m [Char] -> m [a]) -> (NonEmpty a -> NonEmpty (NonEmpty Char)) -> ParseResult e s m VersionNum
versionNumParser f g = checkCoverage $ do
    ns <- f $ some $ satisfy isDigit
    pure $ VersionNum $ g $ NE.fromList ns

pkgParser :: forall e s m. (Token s ~ Char, IsString (Tokens s), MonadParsec e s m, MonadFail m)
    => [Char -> Bool]
    -> [Char -> Bool]
    -> Char
    -> ParseResult e s m String
pkgParser wordStart wordRest wordSep = do
    r <- choice
            [ try $ do
                -- Uh oh. This looks like a version string
                v <- parser @FauxVersion
                choice
                    [ try $ do
                        -- But there is hope, because we found a wordSep!
                        sep <- single wordSep
                        -- Keep going and see if we can find an end that doesn't look like a version
                        next <- pkgParser wordStart wordRest wordSep
                        pure $ Just $ toString v ++ [sep] ++ next

                    , try $ do
                        -- There are some other characters here...
                        r <- some $ satisfyAny wordRest
                        choice
                            [ try $ do
                                -- And it keeps going with another wordSep...
                                sep <- single wordSep
                                next <- pkgParser wordStart wordRest wordSep
                                pure $ Just $ toString v ++ r ++ [sep] ++ next
                            , checkCoverage $ pure $ Just $ toString v ++ r -- or not
                            ]

                    ,   -- No wordSep, no other characters, no hope...
                        -- The parser will _succeed_ but returns Nothing. We call an error
                        -- later on.
                        pure Nothing
                    ]
            , do
                -- This string doesn't look like a version. Hurrah!
                s <- some $ satisfyAny wordStart
                r <- many $ satisfyAny wordRest
                let beg = s ++ r
                choice
                    [ try $ do
                        sep <- single wordSep
                        next <- pkgParser wordStart wordRest wordSep
                        pure $ Just $ beg ++ [sep] ++ next
                    , checkCoverage $ pure $ Just beg
                    ]
            ]

    -- Throw an error if we got a Nothing value. This means that it ended
    -- with something matching Version syntax.
    maybe f pure r
  where
    f :: ParseResult e s m String
    f = unexpected $ Label $ NE.fromList $
            "ends in a hyphen followed by anything "
            ++ "matching the version syntax"
