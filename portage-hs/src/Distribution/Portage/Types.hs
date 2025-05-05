{-|
Module      : Distribution.Portage.Types

Types for Portage atoms, etc.
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

module Distribution.Portage.Types
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
    -- ** Constraints
    , ConstrainedDep(..)
    , toConstrainedDep
    , fromConstrainedDep
    , doesConstraintMatch
    , Operator(..)
    -- * Internal
    , FauxVersion(..)
    , FauxVersionNum(..)
    ) where

import Control.Applicative (Alternative, some)
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

newtype VersionNum = VersionNum
    { unwrapVersionNum :: NonEmpty (NonEmpty Char) }
    deriving stock (Show, Eq, Data, Generic)

-- See section 3.3 "Version Comparison" of the Package Manager Specification
instance Ord VersionNum where
    VersionNum (s1 :| ss1) `compare` VersionNum (s2 :| ss2)
        = toNat s1 `compare` toNat s2 -- Algorithm 3.2
            <> mconcat ((zipWith compRest `on` fmap trim) ss1 ss2) -- Algorithm 3.3
            <> length ss1 `compare` length ss2
      where
        compRest :: (Bool, NonEmpty Char) -> (Bool, NonEmpty Char) -> Ordering
        -- Neither side had a leading zero so we use numeric comparison
        compRest (False, c1) (False, c2) = toNat c1 `compare` toNat c2
        -- At least one side a had leading zero so we use string comparison
        compRest (_    , c1) (_    , c2) =       c1 `compare`       c2

        -- If a component starts with a '0', mark it True and remove any
        -- trailing 0s
        trim :: NonEmpty Char -> (Bool, NonEmpty Char)
        trim = \case
            ('0' :| xs) -> (True, '0' :| foldr skipTrailing0s [] xs)
            xs -> (False, xs)
          where
            skipTrailing0s '0' [] = []
            skipTrailing0s c cs = c:cs

        toNat :: NonEmpty Char -> Natural
        toNat = read . toList

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

instance Printable VersionSuffixNum where
    toString = NE.toList . unwrapVersionSuffixNum

instance Stream s m Char => Parsable VersionSuffixNum m u s where
    parserName = "portage version suffix number"
    parser = VersionSuffixNum . NE.fromList <$> some (satisfy isDigit)

newtype VersionRevision = VersionRevision
    { unwrapVersionRevision :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

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

-- See section 3.3 "Version Comparison" of the Package Manager Specification
instance Ord Version where
    Version n1 l1 s1 r1 `compare` Version n2 l2 s2 r2
        =  n1 `compare` n2
        <> l1 `compare` l2
        <> s1 `compareSuffixes` s2
        <> r1 `compareRevisions` r2
      where
        -- Algorithm 3.5
        compareSuffixes [] [] = EQ
        compareSuffixes ((SuffixP,_):_) [] = GT
        compareSuffixes ((_,_):_) [] = LT
        compareSuffixes [] ((SuffixP,_):_) = LT
        compareSuffixes [] ((_,_):_) = GT
        compareSuffixes (u1:us1) (u2:us2) =
            (compare `on` fmap toNat) u1 u2
                <> us1 `compareSuffixes` us2
          where
            toNat :: Maybe VersionSuffixNum -> Natural
            toNat = maybe (0 :: Natural) (read . toList . unwrapVersionSuffixNum)

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

data Operator
    = Lesser
    | LesserOrEqual
    | Equal
    | EqualAsterisk
    | EqualIgnoreRevision
    | GreaterOrEqual
    | Greater
    deriving stock (Show, Eq, Ord, Bounded, Enum, Data, Generic)

data ConstrainedDep = ConstrainedDep
    { constrainedOperator   :: Operator
    , constrainedCategory   :: Category
    , constrainedPkgName    :: PkgName
    , constrainedVersion    :: Version
    , constrainedSlot       :: Maybe Slot
    , constrainedRepository :: Maybe Repository
    } deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable ConstrainedDep where
    toString (ConstrainedDep o c n v ms mr)
        =  operStr
        ++ toString c
        ++ "/"
        ++ toString n
        ++ "-"
        ++ toString v
        ++ asterisk
        ++ foldMap (\s -> ":"  ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr
      where
        asterisk = case o of
            EqualAsterisk -> "*"
            _ -> ""

        operStr :: String
        operStr = case o of
            Lesser -> "<"
            LesserOrEqual -> "<="
            Greater -> ">"
            GreaterOrEqual -> ">="
            Equal -> "="
            EqualAsterisk -> "="
            EqualIgnoreRevision -> "~"

instance Stream s m Char => Parsable ConstrainedDep m u s where
    parserName = "portage package with version constraint"
    parser = do
        o <- operParser
        c <- parser
        _ <- char '/'
        n <- parser
        _ <- char '-'
        v <- parser
        o' <- option o $ try $ case o of
            Equal -> EqualAsterisk <$ char '*'
            _ -> char '*' *> fail "Unexpected asterisk (not Equal operator)"
        s <- optionMaybe $ try $ string ":"  *> parser
        r <- optionMaybe $ try $ string "::" *> parser
        pure $ ConstrainedDep o' c n v s r
      where
        operParser = choice
            [ char '>' *> option Greater (try (GreaterOrEqual <$ char '='))
            , char '<' *> option Lesser (try (LesserOrEqual <$ char '='))
            , Equal <$ char '='
            , EqualIgnoreRevision <$ char '~'
            ]

toConstrainedDep :: Operator -> Package -> Maybe ConstrainedDep
toConstrainedDep o (Package c n mv ms mr) =
    (\v -> ConstrainedDep o c n v ms mr) <$> mv

fromConstrainedDep :: ConstrainedDep -> Package
fromConstrainedDep (ConstrainedDep _ c p v ms mr) =
    Package c p (Just v) ms mr

-- | Uses "optimistic" matching, meaning: if a package does not provide
--   information that the constraint requires, it is assumed to be a match.
--
--   For example, the constraint @<dev-haskell/text-2.2:0::haskell@ matches
--   @dev-haskell/text@ but not @dev-haskell/text-2.3@.
doesConstraintMatch :: ConstrainedDep -> Package -> Bool
doesConstraintMatch (ConstrainedDep co cc cn cv cs cr) (Package pc pn pv ps pr)
    | cc /= pc = False
    | cn /= pn = False
    | otherwise
        =  checkConstrained cs ps
        && checkConstrained cr pr
        && case pv of
            Nothing -> True -- No version info on the package means it matches
            Just v -> case co of
                Lesser -> v < cv
                LesserOrEqual -> v <= cv
                Equal -> v == cv
                GreaterOrEqual -> v >= cv
                Greater -> v > cv
                EqualIgnoreRevision ->
                    v { getVersionRevision = Nothing }
                        == cv { getVersionRevision = Nothing }
                EqualAsterisk ->
                    checkEqAst
                        (toList (getVersionNum cv))
                        (toList (getVersionNum v))
  where
    checkEqAst [] [] = True
    checkEqAst (_cv:_cvs) [] = False
    checkEqAst [] (_v:_vs) = True
    checkEqAst (cv':cvs) (v:vs) = cv' == v && checkEqAst cvs vs

    checkConstrained :: Eq a => Maybe a -> Maybe a -> Bool
    checkConstrained m1 m2 = case (m1, m2) of
        -- Constraint specifies, package provides
        (Just x1, Just x2) -> x1 == x2
        -- Package does not specifiy
        _ -> True

data Slot = Slot
    { unwrapSlot :: String
    , getSubSlot :: Maybe SubSlot
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

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
