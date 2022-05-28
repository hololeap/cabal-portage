{-|
Module      : Distribution.Portage.Types

Types for Portage atoms, etc.
-}

{-# Language ApplicativeDo #-}
{-# Language DerivingVia #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
-- {-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
-- {-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
-- {-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
-- {-# Language UndecidableInstances #-}

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
    ) where

import Data.Data (Data, Typeable)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Parsable
import Data.Map.Strict (Map)
import qualified Data.Map as M
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Natural

newtype Category = Category
    { unwrapCategory :: String }
    deriving (Show, Eq, Ord, IsString, Printable, Data, Typeable, Generic)

instance Parsable Category where
    parser = (<?> "portage category") $
        fmap Category <$>
            wordsWithSep [isAsciiLower] [isAsciiLower, isDigit] [(== '-')]

newtype PkgName = PkgName
    { unwrapPkgName :: String }
    deriving (Show, Eq, Ord, IsString, Printable, Data, Typeable, Generic)

instance Parsable PkgName where
    parser = (<?> "portage package name") $
        fmap PkgName <$> wordsWithSep wordStart wordRest wordSep
      where
        wordStart = [isAsciiUpper, isAsciiLower]
        wordRest  = wordStart ++ [isDigit, (== '+')]
        wordSep   = [(== '-'), (== '_')]

newtype VersionNum = VersionNum
    { unwrapVersionNum :: NonEmpty Natural }
    deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance IsList VersionNum where
    type instance Item VersionNum = Natural
    fromList = VersionNum . fromList
    toList = toList . unwrapVersionNum

instance Printable VersionNum where
    toString = L.intercalate "." . NE.toList . fmap show . unwrapVersionNum

instance Parsable VersionNum where
    parser = (<?> "portage version number") $ do
        ns :: [PartialParse (NaturalParsable Natural)]
            <- sepBy1 parser (char '.')
        let ns' = NE.fromList $ map (fmap unwrapNaturalParsable) ns
        pure $ VersionNum <$> partialParses ns'

newtype VersionLetter = VersionLetter
    { unwrapVersionLetter :: Char }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)

instance Printable VersionLetter where
    toString (VersionLetter l) = [l]

instance Parsable VersionLetter where
    parser = (<?> "portage version letter") $ endPartial $
        VersionLetter <$> satisfy isAsciiLower

data VersionSuffix
    = SuffixAlpha
    | SuffixBeta
    | SuffixPre
    | SuffixRC
    | SuffixP
    deriving (Show, Eq, Ord, Data, Typeable, Generic, Bounded, Enum)

instance Printable VersionSuffix where
    toString = \case
        SuffixAlpha -> "alpha"
        SuffixBeta  -> "beta"
        SuffixPre   -> "pre"
        SuffixRC    -> "rc"
        SuffixP     -> "p"

instance Parsable VersionSuffix where
    parser = (<?> "portage version suffix") $ endPartial $
        choice
            [ SuffixAlpha <$ string "alpha"
            , SuffixBeta  <$ string "beta"
            , SuffixPre   <$ string "pre"
            , SuffixRC    <$ string "rc"
            , SuffixP     <$ string "p"
            ]

newtype VersionSuffixNum = VersionSuffixNum
    { unwrapVersionSuffixNum :: Natural }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)
    deriving newtype Num
    deriving Printable via ShowPrintable VersionSuffixNum

instance Parsable VersionSuffixNum where
    parser = (<?> "portage version suffix number") $ do
        n :: PartialParse (NaturalParsable Natural) <- parser
        pure $ VersionSuffixNum . unwrapNaturalParsable <$> n

newtype VersionRevision = VersionRevision
    { unwrapVersionRevision :: Natural }
    deriving stock (Show, Eq, Ord, Data, Typeable, Generic)
    deriving newtype Num

instance Printable VersionRevision where
    toString (VersionRevision r) = "r" ++ show r

instance Parsable VersionRevision where
    parser = (<?> "portage version revision") $ do
        _ <- char 'r'
        r :: PartialParse (NaturalParsable Natural) <- parser
        pure $ VersionRevision . unwrapNaturalParsable <$> r

data Version = Version
    { getVersionNum :: VersionNum
    , getVersionLetter :: Maybe VersionLetter
    , getVersionSuffixes :: Map VersionSuffix (Maybe VersionSuffixNum)
    , getVersionRevision :: VersionRevision
    } deriving (Show, Eq, Data, Typeable, Generic)

instance Ord Version where
    Version n1 l1 s1 r1 `compare` Version n2 l2 s2 r2
        = n1 `compare` n2
        <> l1 `compare` l2
        <> compareSuffixes
        <> r1 `compare` r2
      where
        compareSuffixes = case (M.null s1, M.null s2) of
            (True, False) -> GT -- Empty Set of suffixes is greater than non-empty set
            (False, True) -> LT -- non-empty Set of suffixes is less than empty set
            _             -> s1 `compare` s2

instance Printable Version where
    toString (Version n l s r)
        = toString n
        ++ foldMap toString l
        ++ (M.toList s >>= suffixString)
        ++ "-" ++ toString r
      where
        suffixString (ss,sn) = "_" ++ toString ss ++ foldMap toString sn

instance Parsable Version where
    parser = (<?> "portage version") $ do
        n :: VersionNum <- parserValue
        l :: Maybe VersionLetter <- optionMaybe parserValue
        s <- many $ do
            _ <- char '_'
            ss :: VersionSuffix <- parserValue
            sn :: Maybe VersionSuffixNum <- optionMaybe parserValue
            pure (ss, sn)
        pr :: PartialParse VersionRevision <- choice
            [ parser
            , endPartial $ pure 0
            ]

        pure $ Version n l (M.fromList s) <$> pr


