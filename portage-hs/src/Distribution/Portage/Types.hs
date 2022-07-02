{-|
Module      : Distribution.Portage.Types

Types for Portage atoms, etc.
-}

{-# Language ApplicativeDo #-}
{-# Language DerivingVia #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Distribution.Portage.Types
    (
      Category(..)
    , PkgName(..)
    , Version(..)
    , VersionNum(..)
    , VersionLetter(..)
    , VersionSuffix(..)
    , VersionSuffixNum(..)
    , VersionRevision(..)
    , Slot(..)
    , Repository(..)
    , Package(..)
    ) where

-- import Control.Applicative (Alternative)
import Data.Data (Data, Typeable)
import Data.Function (on)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map as M
-- import Data.Maybe (fromMaybe)
import qualified Data.Set as S
-- import Data.Word
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Natural

import Data.Parsable

newtype Category = Category
    { unwrapCategory :: String }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)
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
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)
    deriving newtype (IsString, Printable)

instance forall m s e. (Token s ~ Char, MonadParsec e s m, MonadFail m, IsString (Tokens s))
    => Parsable PkgName m s e where
    parserName = "portage package name"
    parser = PkgName <$> go []
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = (== '+') : wordStart
        wordSep = (== '-')

        go :: [Either Version String] -> ParseResult e s m String
        go stack = do
            let beg :: m String = do
                    s <- some $ satisfyAny wordStart
                    r <- many $ satisfyAny wordRest
                    pure $ s ++ r
            choice
                [ try $ do -- Branch if the current string is a valid Version
                    b <- lift $ lookAhead beg
                    v <- parser
                    choice
                        [ do -- Branch if this ends with a hyphen
                            sep  <- satisfy wordSep
                            next <- go (Left v : stack)
                            pure $ b ++ [sep] ++ next
                        , failure -- Branch if it does not end with a hyphen
                            (Just $ Label $ NE.fromList $
                                "ends in a hyphen followed by anything "
                                ++ "matching the version syntax"
                            )
                            S.empty
                        ]
                , try $ do -- Branch if the current string is not a valid Version and ends with a hyphen
                    b    <- lift beg
                    sep  <- satisfy wordSep
                    next <- go []
                    pure $ L.intercalate [sep] $ reverse (either toString id <$> stack) ++ [b, next]
                , checkCoverage beg -- _
                ]



newtype VersionNum = VersionNum
    { unwrapVersionNum :: NonEmpty (NonEmpty Char) }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)

instance IsList VersionNum where
    type instance Item VersionNum = NonEmpty Char
    fromList = VersionNum . fromList
    toList = toList . unwrapVersionNum

instance Printable VersionNum where
    toString = L.intercalate "." . NE.toList . fmap NE.toList . unwrapVersionNum

instance
    ( Token s ~ Char
    , MonadParsec e s m
    , MonadFail m
    ) => Parsable VersionNum m s e where
    parserName = "portage version number"
    parser = checkCoverage $ do
        ns <- sepBy1 (some (satisfy isDigit)) (single '.')
        pure $ VersionNum $ NE.fromList $ NE.fromList <$> ns

newtype VersionLetter = VersionLetter
    { unwrapVersionLetter :: Char }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)

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
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic, Bounded, Enum)

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
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)

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
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)

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
    } deriving stock (Show, Eq, Data, Typeable, Generic)

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
    parser = do
        n <- parser
        l <- optional $ try parser
        s <- many $ do
            _ <- char '_'
            ss <- parser
            sn <- optional $ try parser
            pure (ss, sn)
        r <- optional $ try $ string "-" *> parser
        pure $ Version n l s r

newtype Slot = Slot { unwrapSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)
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
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)
    deriving newtype (IsString, Printable)

instance (Token s ~ Char, MonadParsec e s m) => Parsable Repository m s e where
    parserName = "portage repository"
    parser = Repository . concat <$> wordsWithSep wordStart wordRest (const False)
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = wordStart ++ [(== '-')]

data Package = Package
    { getCategory   :: Category
    , getPkgName    :: PkgName
    , getVersion    :: Maybe Version
    , getSlot       :: Maybe Slot
    , getRepository :: Maybe Repository
    }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)

instance Printable Package where
    toString (Package c n mv ms mr)
        =  toString c
        ++ "/"
        ++ toString n
        ++ foldMap (\x -> "-"  ++ toString x) mv
        ++ foldMap (\x -> ":"  ++ toString x) ms
        ++ foldMap (\x -> "::" ++ toString x) mr

instance
    ( Token s ~ Char
    , IsString (Tokens s)
    , MonadParsec e s m
    , MonadFail m
    ) => Parsable Package m s e where
    parserName = "portage package"
    parser = do
        c <- parser
        _ <- char '/'
        n <- parser
        v <- optional $ try $ string "-"  *> parser
        s <- optional $ try $ string ":"  *> parser
        r <- optional $ try $ string "::" *> parser
        pure $ Package c n v s r
