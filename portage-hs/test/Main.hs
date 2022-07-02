{-# Language CPP #-}
{-# Language DerivingVia #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeApplications #-}

{-# Options_GHC -Wno-orphans #-}

module Main (main) where

-- import Control.Monad.STM
-- import Control.Concurrent.STM.TChan
import Data.Char
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import GHC.Natural
import Test.QuickCheck
import Test.Tasty
-- import Test.Tasty.QuickCheck

import Distribution.Portage.Types
-- import Data.Parsable
import Test.Parsable
#if defined(GENTOO_TESTS)
import GentooTests
#endif

main :: IO ()
main = do
    tests <- sequenceA
        [ atomically quickcheckTests
#if defined(GENTOO_TESTS)
        , gentooTests
#endif
        ]
    defaultMain $ testGroup "portage-hs tests" tests

quickcheckTests :: STM TestTree
quickcheckTests =
    testGroup "parsable validity tests" <$> sequenceA
        [ parsableTest (Proxy @Category)
        , parsableTest (Proxy @PkgName)
        , parsableTest (Proxy @VersionNum)
        , parsableTest (Proxy @VersionLetter)
        , parsableTest (Proxy @VersionSuffix)
        , parsableTest (Proxy @VersionSuffixNum)
        , parsableTest (Proxy @VersionRevision)
        , parsableTest (Proxy @Version)
        , parsableTest (Proxy @Slot)
        , parsableTest (Proxy @Repository)
        , parsableTest (Proxy @Package)
        ]

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural

instance Arbitrary Category where
    arbitrary = Category <$>
        wordsWithSepGen wordStart wordRest wordRest
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

--     arbitrary = elements
--         [ "app-backup"
--         , "app-mobilephone"
--         , "dev-erlang"
--         , "dev-haskell"
--         , "dev-java"
--         , "dev-texlive"
--         , "games-misc"
--         , "gnome-base"
--         , "gnome-extra"
--         , "media-video"
--         , "net-analyzer"
--         , "net-vpn"
--         , "perl-core"
--         , "sys-block"
--         , "sys-boot"
--         , "sys-process"
--         , "x11-apps"
--         , "x11-base"
--         , "x11-plugins"
--         , "x11-themes"
--         ]

instance Arbitrary PkgName where
    arbitrary = PkgName <$> wordsWithSepGen wordStart wordRest wordSep
      where
        wordStart = [isAsciiUpper, isAsciiLower]
        wordRest  = wordStart ++ [isDigit, (== '+')]
        wordSep   = [(== '-'), (== '_')]

--     arbitrary = elements
--         [ "IPC-System-Simple"
--         , "Locale-gettext"
--         , "Text-WrapI18N"
--         , "attica"
--         , "erubi"
--         , "frei0r-plugins"
--         , "gwenview"
--         , "html-entity-map"
--         , "kcontacts"
--         , "libreplaygain"
--         , "libtasn1"
--         , "m4"
--         , "math-functions"
--         , "netcdf"
--         , "plugdev"
--         , "qtquickcontrols"
--         , "rio-prettyprint"
--         , "scim"
--         , "sddm-kcm"
--         , "time-locale-compat"
--         ]

instance Arbitrary VersionNum where
    arbitrary = do
        css <- listOf1 $ listOf1 $ arbitrary `suchThat` isDigit
        pure $ VersionNum $ NE.fromList $ NE.fromList <$> css

instance Arbitrary VersionLetter where
    arbitrary = VersionLetter <$> arbitrary `suchThat` isAsciiLower

instance Arbitrary VersionSuffix where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary VersionSuffixNum where
    arbitrary = do
        cs <- listOf1 $ arbitrary `suchThat` isDigit
        pure $ VersionSuffixNum $ NE.fromList cs

instance Arbitrary VersionRevision where
    arbitrary = do
        cs <- listOf1 $ arbitrary `suchThat` isDigit
        pure $ VersionRevision $ NE.fromList cs

instance Arbitrary Version where
    arbitrary = Version
        <$> arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary (liftArbitrary arbitrary)
        <*> liftArbitrary arbitrary

instance Arbitrary Slot where
    arbitrary = Slot <$> wordsWithSepGen wordStart wordRest wordRest
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

instance Arbitrary Repository where
    arbitrary = Repository <$> wordsWithSepGen wordStart wordRest wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = wordStart ++ [(== '-')]

instance Arbitrary Package where
    arbitrary = Package
        <$> arbitrary
        <*> arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary arbitrary

-- | These are package strings that have caused issues when trying to parse
--   them as a 'Package'.
trickyPackages :: [String]
trickyPackages =
    [ "dev-python/nose-1.3.7_p20211111_p1" -- Two instances of _p
    , "app-text/xdvik-22.87.03-r3"         -- Should not drop the '0' in ".03"
    , "dev-java/log4j-12-api-2.17.2"       -- Number in the middle of the PkgName
    ]
