{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}

module UnitTests (unitTests) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Void
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Portage.Types.Internal
import Data.Parsable

unitTests :: TestTree
unitTests = testGroup "unit tests"
    [ testGroup "basic tests"
        [ "dev-python/nose" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                Nothing
                Nothing
                Nothing
        , "dev-python/nose-1" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum (('1':|[]):|[]))
                    Nothing
                    []
                    Nothing
                ))
                Nothing
                Nothing
        , "dev-python/nose-1.23a-r123" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    (Just (VersionLetter 'a'))
                    []
                    (Just (VersionRevision ('1':|"23")))
                ))
                Nothing
                Nothing
        , "dev-python/nose:1337" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                Nothing
                (Just (Slot "1337"))
                Nothing
        , "dev-python/nose::uberRepo" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                Nothing
                Nothing
                (Just (Repository "uberRepo"))
        ,  "dev-python/nose-1.23a-r123:1337::uberRepo" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    (Just (VersionLetter 'a'))
                    []
                    (Just (VersionRevision ('1':|"23")))
                ))
                (Just (Slot "1337"))
                (Just (Repository "uberRepo"))
        ]
    , testGroup "tricky tests"
        [ "dev-python/nose-1.3.7_p20211111_p1" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum
                        (('1':|[]) :| ['3':|[],'7':|[]])
                    )
                    Nothing
                    [ (SuffixP, Just (VersionSuffixNum ('2' :| "0211111")))
                    , (SuffixP, Just (VersionSuffixNum ('1' :| [])))
                    ]
                    Nothing
                ))
                Nothing
                Nothing
        , "app-text/xdvik-22.87.03-r3" `parserTest`
            Package
                (Category "app-text")
                (PkgName "xdvik")
                (Just (Version
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
                ))
                Nothing
                Nothing
        , "dev-java/log4j-12-api-2.17.2" `parserTest`
            Package
                (Category "dev-java")
                (PkgName "log4j-12-api")
                (Just (Version
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
                ))
                Nothing
                Nothing
        , "sec-keys/openpgp-keys-gawk-20220407" `parserTest`
            Package
                (Category "sec-keys")
                (PkgName "openpgp-keys-gawk")
                (Just (Version
                    (VersionNum (('2':|"0220407"):|[]))
                    Nothing
                    []
                    Nothing
                ))
                Nothing
                Nothing
        ]
    , testGroup "failed QuickCheck tests"
        [ "7-r2b" `parserTest` Repository "7-r2b"
        , "7-r2b" `parserTest` PkgName "7-r2b"
        ]
    ]

-- Takes a string and an expected result, and creates a TestTree from them
parserTest ::
    ( Parsable a (Parsec Void String) String Void
    , Eq a
    , Show a
    ) => String
    -> a
    -> TestTree
parserTest s x = testCase (show s) $
    Right (CompleteParse, x) @=? runParsable @_ @Void "" s
