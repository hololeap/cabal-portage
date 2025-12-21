{-|
Module: Types.UnitTests

Custom-written unit tests for testing basic functionality or
input that has caused problems in the past.
-}

{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module Types.UnitTests (unitTests) where

import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString.Char8 (pack)

import Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

unitTests :: TestTree
unitTests = testGroup "unit tests"
    [ testGroup "basic tests"
        [ "dev-python/nose" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
        , "app-misc/foo-0.1" `negatedParserTest`
            Package
                (Category "app-misc")
                (PkgName "foo")
        , "=dev-python/nose-1" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum (('1':|[]):|[]))
                    Nothing
                    []
                    Nothing
                )
        , "=dev-python/nose-1.23a-r123" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    (Just (VersionLetter 'a'))
                    []
                    (Just (VersionRevision ('1':|"23")))
                )
        , "dev-python/nose:1337" `parserTest`
            UnversionedDepSpec
                Nothing
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Just (Slot "1337" Nothing))
                Nothing
        , "dev-python/nose" `parserTest`
            UnversionedDepSpec
                Nothing
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                Nothing
                Nothing
        ,  "=dev-python/nose-1.23a-r123:1337" `parserTest`
            VersionedDepSpec
                Nothing
                (VPkgEq
                    (Package
                        (Category "dev-python")
                        (PkgName "nose")
                    )
                    (Version
                        (VersionNum (('1':|[]):|['2':|"3"]))
                        (Just (VersionLetter 'a'))
                        []
                        (Just (VersionRevision ('1':|"23")))
                    )
                )
                (Just (Slot "1337" Nothing))
                Nothing
        , "=dev-python/nose-1.23*" `parserTest`
            VPkgEqWildcard
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    Nothing
                    []
                    Nothing
                )
        , "=dev-python/nose-1.23*:*" `parserTest`
            VersionedDepSpec
                Nothing
                (VPkgEqWildcard
                    (Package
                        (Category "dev-python")
                        (PkgName "nose")
                    )
                    (Version
                        (VersionNum (('1':|[]):|['2':|"3"]))
                        Nothing
                        []
                        Nothing
                    )
                )
                (Just AnySlot)
                Nothing
        , "[profile?,-test]" `parserTest`
            UseDependency
                (UseDepMatchIfEnabled "profile" Nothing
                :| [UseDepDisabled "test" Nothing]
                )
        , "[!profile(+)?,test(-)]" `parserTest`
            UseDependency
                (UseDepMatchIfDisabled "profile" (Just UseDefaultEnabled)
                :| [UseDepEnabled "test" (Just UseDefaultDisabled)]
                )
        ]
    , testGroup "tricky tests"
        [ "=dev-python/nose-1.3.7_p20211111_p1" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum
                        (('1':|[]) :| ['3':|[],'7':|[]])
                    )
                    Nothing
                    [ (SuffixP, Just (VersionSuffixNum ('2' :| "0211111")))
                    , (SuffixP, Just (VersionSuffixNum ('1' :| [])))
                    ]
                    Nothing
                )
        , "=app-text/xdvik-22.87.03-r3" `parserTest`
            VPkgEq
                (Package
                    (Category "app-text")
                    (PkgName "xdvik")
                )
                (Version
                    (VersionNum
                        (('2':|"2") :|
                            ['8':|"7"
                            ,'0':|"3"
                            ]
                        )
                    )
                    Nothing
                    []
                    (Just (VersionRevision ('3':|[])))
                )
        , "=dev-java/log4j-12-api-2.17.2" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-java")
                    (PkgName "log4j-12-api")
                )
                (Version
                    (VersionNum
                        (('2':|[]) :|
                            ['1':|"7"
                            ,'2':|[]
                            ]
                        )
                    )
                    Nothing
                    []
                    Nothing
                )
        , "=sec-keys/openpgp-keys-gawk-20220407" `parserTest`
            VPkgEq
                (Package
                    (Category "sec-keys")
                    (PkgName "openpgp-keys-gawk")
                )
                (Version
                    (VersionNum (('2':|"0220407"):|[]))
                    Nothing
                    []
                    Nothing
                )
        , "!!=app-derp/f--98---54.321x_pre1:gawk--" `parserTest`
            VersionedDepSpec
                (Just StrongBlock)
                (VPkgEq
                    (Package
                        (Category "app-derp")
                        (PkgName "f--98--")
                    )
                    (Version
                        (VersionNum (('5':|"4") :| ['3':|"21"]))
                        (Just (VersionLetter 'x'))
                        [ (SuffixPre, Just (VersionSuffixNum ('1' :| []))) ]
                        Nothing
                    )
                )
                (Just (Slot "gawk--" Nothing))
                Nothing
        , "=app-derp/f--98---54.321x_pre1:gawk--" `parserTest`
            VersionedDepSpec
                Nothing
                (VPkgEq
                    (Package
                        (Category "app-derp")
                        (PkgName "f--98--")
                    )
                    (Version
                        (VersionNum (('5':|"4") :| ['3':|"21"]))
                        (Just (VersionLetter 'x'))
                        [ (SuffixPre, Just (VersionSuffixNum ('1' :| []))) ]
                        Nothing
                    )
                )
                (Just (Slot "gawk--" Nothing))
                Nothing
        , "=app-office/hledger-web-1.26.1:0/1.26.1=[profile(+)?,-test]" `parserTest`
            VersionedDepSpec
                Nothing
                (VPkgEq
                    (Package
                        (Category "app-office")
                        (PkgName "hledger-web")
                    )
                    (Version
                        (VersionNum (('1':|[]) :| ['2':|"6", '1':|[]]))
                        Nothing
                        []
                        Nothing
                    )
                )
                (Just (SlotBreakable "0" (Just (SubSlot "1.26.1"))))
                (Just (UseDependency
                    (UseDepMatchIfEnabled "profile" (Just UseDefaultEnabled)
                        :| [UseDepDisabled "test" Nothing]
                    )
                ))
        ]
    , testGroup "failed QuickCheck tests"
        [ "7-r2b" `parserTest` PkgName "7-r2b"
        , "1.3.7_p20211111_p1" `parserTest` Version
                    (VersionNum
                        (('1':|[]) :| ['3':|[],'7':|[]])
                    )
                    Nothing
                    [ (SuffixP, Just (VersionSuffixNum ('2' :| "0211111")))
                    , (SuffixP, Just (VersionSuffixNum ('1' :| [])))
                    ]
                    Nothing
        , "p" `parserTest` SuffixP
        , "F/z-tNk[s(+)?]" `parserTest`
            UnversionedDepSpec
                Nothing
                (Package
                    (Category "F")
                    (PkgName "z-tNk")
                )
                Nothing
                (Just (UseDependency
                    (UseDepMatchIfEnabled (UseFlag "s") (Just UseDefaultEnabled)
                    :| []
                    )
                ))
        ]
    , testGroup "real-world tests"
        [ ">=dev-lang/ghc-9.0.2:= \
          \dev-libs/icu \
          \dev-haskell/text:=[profile?] \
          \|| ( ( >=dev-haskell/text-0.9.1.0 <dev-haskell/text-1.3 ) \
              \( >=dev-haskell/text-2.0 <dev-haskell/text-2.2 ) ) \
          \>=dev-haskell/cabal-3.4.1.0 \
          \virtual/pkgconfig \
          \test? ( >=dev-haskell/hunit-1.2 >=dev-haskell/quickcheck-2.4 \
              \dev-haskell/random >=dev-haskell/test-framework-0.4 \
              \>=dev-haskell/test-framework-hunit-0.2 \
              \>=dev-haskell/test-framework-quickcheck2-0.2 \
              \dev-haskell/text ) hscolour? ( dev-haskell/hscolour )"
            `parserTest`
            DepBlock
                [ Right (
                    VersionedDepSpec
                        Nothing
                        (VPkgGE
                            (Package "dev-lang" "ghc")
                            (Version
                                (VersionNum $ ('9':|[]):|['0':|[],'2':|[]])
                                Nothing
                                []
                                Nothing))
                        (Just AnySlotBreakable)
                        Nothing )
                , Right (
                    UnversionedDepSpec
                        Nothing
                        (Package "dev-libs" "icu")
                        Nothing
                        Nothing )
                , Right (
                    UnversionedDepSpec
                        Nothing
                        (Package "dev-haskell" "text")
                        (Just AnySlotBreakable)
                        (Just (UseDependency (
                            UseDepMatchIfEnabled "profile" Nothing :| [])) ) )
                , Left ( OrGroup $ fromList
                        [ Left ( AndGroup $ fromList
                            [ Right ( VersionedDepSpec
                                Nothing
                                (VPkgGE
                                    (Package "dev-haskell" "text")
                                    (Version
                                        (VersionNum $ fromList <$> fromList ["0","9","1","0"])
                                        Nothing
                                        []
                                        Nothing))
                                Nothing
                                Nothing
                                )
                            , Right ( VersionedDepSpec
                                Nothing
                                (VPkgLT
                                    (Package "dev-haskell" "text")
                                    (Version
                                        (VersionNum $ fromList <$> fromList ["1","3"])
                                        Nothing
                                        []
                                        Nothing))
                                Nothing
                                Nothing
                                )
                            ] )
                        , Left ( AndGroup $ fromList
                            [ Right ( VersionedDepSpec
                                Nothing
                                (VPkgGE
                                    (Package "dev-haskell" "text")
                                    (Version
                                        (VersionNum $ fromList <$> fromList ["2","0"])
                                        Nothing
                                        []
                                        Nothing))
                                Nothing
                                Nothing
                                )
                            , Right ( VersionedDepSpec
                                Nothing
                                (VPkgLT
                                    (Package "dev-haskell" "text")
                                    (Version
                                        (VersionNum $ fromList <$> fromList ["2","2"])
                                        Nothing
                                        []
                                        Nothing))
                                Nothing
                                Nothing
                                )
                            ] )
                        ]
                    )
                , Right (
                    VersionedDepSpec
                        Nothing
                        (VPkgGE
                            (Package "dev-haskell" "cabal")
                            (Version
                                (VersionNum $ fromList <$> fromList ["3","4","1","0"])
                                Nothing
                                []
                                Nothing))
                        Nothing
                        Nothing )
                , Right (
                    UnversionedDepSpec
                        Nothing
                        (Package "virtual" "pkgconfig")
                        Nothing
                        Nothing )
                , Left ( UseGroup ( fromList
                    [ Right (
                        VersionedDepSpec
                            Nothing
                            (VPkgGE
                                (Package "dev-haskell" "hunit")
                                (Version
                                    (VersionNum $ fromList <$> fromList ["1","2"])
                                    Nothing
                                    []
                                    Nothing))
                            Nothing
                            Nothing )
                    , Right (
                        VersionedDepSpec
                            Nothing
                            (VPkgGE
                                (Package "dev-haskell" "quickcheck")
                                (Version
                                    (VersionNum $ fromList <$> fromList ["2","4"])
                                    Nothing
                                    []
                                    Nothing))
                            Nothing
                            Nothing )
                    , Right (
                        UnversionedDepSpec
                            Nothing
                            (Package "dev-haskell" "random")
                            Nothing
                            Nothing )
                    , Right (
                        VersionedDepSpec
                            Nothing
                            (VPkgGE
                                (Package "dev-haskell" "test-framework")
                                (Version
                                    (VersionNum $ fromList <$> fromList ["0","4"])
                                    Nothing
                                    []
                                    Nothing))
                            Nothing
                            Nothing )
                    , Right (
                        VersionedDepSpec
                            Nothing
                            (VPkgGE
                                (Package "dev-haskell" "test-framework-hunit")
                                (Version
                                    (VersionNum $ fromList <$> fromList ["0","2"])
                                    Nothing
                                    []
                                    Nothing))
                            Nothing
                            Nothing )
                    , Right (
                        VersionedDepSpec
                            Nothing
                            (VPkgGE
                                (Package "dev-haskell" "test-framework-quickcheck2")
                                (Version
                                    (VersionNum $ fromList <$> fromList ["0","2"])
                                    Nothing
                                    []
                                    Nothing))
                            Nothing
                            Nothing )
                    , Right (
                        UnversionedDepSpec
                            Nothing
                            (Package "dev-haskell" "text")
                            Nothing
                            Nothing )
                    ] )
                    "test" )
                , Left ( UseGroup ( fromList
                    [ Right (
                        UnversionedDepSpec
                            Nothing
                            (Package "dev-haskell" "hscolour")
                            Nothing
                            Nothing )
                    ] )
                    "hscolour" )
                ]
        ]
    ]

-- Takes a string and an expected result, and creates a TestTree from them
parserTest :: forall a.
    ( Parsable a PureMode String
    , Eq a
    , Show a
    ) => String
    -> a
    -> TestTree
parserTest s x = testCase (show s) $
    Right (CompleteParse, x) @=? extractResult (runParser (checkParsable @a @PureMode @String) (pack s))

-- Takes a string and a bad result, and creates a TestTree from them
negatedParserTest :: forall a.
    ( Parsable a PureMode String
    , Eq a
    , Show a
    , Typeable a
    ) => String
    -> a
    -> TestTree
negatedParserTest s x = testCase (show s ++ " should NOT be " ++ show (typeOf x)) $
    when (e == r) (assertFailure msg)
  where
    e :: Either (Maybe String) (ParseCoverage, a)
    e = Right (CompleteParse, x)

    r :: Either (Maybe String) (ParseCoverage, a)
    r = extractResult (runParser (checkParsable @a @PureMode) (pack s))

    msg :: String
    msg = "not expected: " ++ show e
