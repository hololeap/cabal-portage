{-|
Module: Types.GentooTests

Tests that require a running Gentoo system to test against.
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}

module Types.GentooTests (gentooTests) where

import Conduit
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Conduit.Process
import Data.Proxy
import ListT (ListT, fromFoldable, toList)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

gentooTests :: IO TestTree
gentooTests = testGroup "strings from live Gentoo system" <$> sequenceA
    [ do
        toErr "Gathering info for /var/db/repos parse tests... "
        t <- repoTree
        toErr $ show (length t) ++ " entries found\n"
        pure $ gentooParseTests "/var/db/repos parse tests" t
    , do
        toErr "Gathering info for /var/db/pkg parse tests... "
        t <- pkgTree
        toErr $ show (length t) ++ " entries found\n"
        pure $ gentooParseTests "/var/db/pkg parse tests" t
    , do
        toErr "Gathering info for pquery dump parse tests...\n"
        toErr "    Parsing raw pquery output... "
        dump <- getPqueryDump
        toErr $ show (length dump) ++ " lines\n"
        pure $ pqueryParseTests dump
    ]
  where
    toErr :: String -> IO ()
    toErr = hPutStr stderr

-- | Check all package atoms and make sure they are parsed successfully and
--   pass the "roundtrip" test.
gentooParseTests :: TestName -> [String] -> TestTree
gentooParseTests n pkgStrings = testGroup n $
    parsableHUnit (Proxy @DepSpec) <$> pkgStrings

pqueryParseTests :: [ByteString] -> TestTree
pqueryParseTests bStrings = testGroup "pquery output tests" $
    flatparseHUnit <$> zip [1..] bStrings
  where
    flatparseHUnit :: (Int, ByteString) -> TestTree
    flatparseHUnit (i,bs) = testCaseSteps ("test #" ++ show i) $ \step -> do
        step "Parsing pquery line"

        let pErr s = assertFailure $ unlines
                [ "Error when parsing pquery line: " ++ s
                , "input: " ++ utf8ToStr bs ]

        case runParser (parser @PqueryTestEntry) bs of
            Fail -> pErr "parser returned an uncaught failure"
            Err e -> pErr $ "Parsing entry retuned error: " ++ e
            OK (PqueryTestEntry ds dep rdep bdep pdep idep) rest
                | not (BS.null rest) -> pErr
                    $  "Parser did not fully consume input. Remainder: "
                    ++ utf8ToStr rest
                | otherwise -> do
                    step $ "Good: " ++ toString ds

                    testStep step "DEPEND" dep
                    testStep step "RDEPEND" rdep
                    testStep step "BDEPEND" bdep
                    testStep step "PDEPEND" pdep
                    testStep step "IDEPEND" idep

    testStep :: (Parsable a PureMode String, Printable a, Show a, Eq a)
        => (String -> IO ()) -> String -> a -> IO ()
    testStep step name item = do
        step $ "\nChecking " ++ name
        printableAssertion item
        step $ "Good: " ++ toString item

-- | Scan the entire @/var/db/repos@ tree for ebuilds and form these into
--  pacakge atoms.
repoTree :: IO [String]
repoTree = toList $ do
    let reposDir = "/var/db/repos"
    checkDir reposDir $ do

        repo <- listDir reposDir
        cat  <- listDir $ reposDir </> repo
        n    <- listDir $ reposDir </> repo </> cat
        eb   <- listDir $ reposDir </> repo </> cat </> n

        let (nv,ext) = splitExtension eb

        guard $ ext == ".ebuild"

        pure $ "=" ++ cat ++ "/" ++ nv

-- | Scan the entire @/var/db/pkg@ tree for package atoms in the form
--   @<category>/<pkg-name>-<version>@. This is inherently part of the directory
--   structure inside @/var/db/pkg@.
pkgTree :: IO [String]
pkgTree = toList $ do
    let portagePkgDir = "/var/db/pkg"
    checkDir portagePkgDir $ do

        cat <- listDir portagePkgDir           -- Scan for categories
        nv  <- listDir $ portagePkgDir </> cat -- Scan for names/versions

        case nv of
            '.' : _ -> e cat nv
            '-' : _ -> e cat nv
            []      -> e cat nv
            _       -> pure $ "=" ++ cat ++ "/" ++ nv
  where
    e :: MonadIO m => String -> String -> m a
    e cat nv = liftIO $ fail $ show (cat </> nv) ++ " failed to parse"

listDir :: FilePath -> ListT IO FilePath
listDir d = do
    lift (doesDirectoryExist d) >>= guard
    lift (listDirectory d) >>= fromFoldable

checkDir :: FilePath -> ListT IO a -> ListT IO a
checkDir d rest = do
    b <- lift $ doesDirectoryExist d
    if b
        then rest
        else lift $ assertFailure $
                show d ++ " is not a directory. Is this really a Gentoo system?"

data PqueryTestEntry = PqueryTestEntry
    { pqueryTestEntryDepSpec :: DepSpec
    , pqueryTestEntryDepend :: DepBlock
    , pqueryTestEntryRDepend :: DepBlock
    , pqueryTestEntryBDepend :: DepBlock
    , pqueryTestEntryPDepend :: DepBlock
    , pqueryTestEntryIDepend :: DepBlock
    }
    deriving stock (Show, Eq, Ord)

instance Parsable PqueryTestEntry st String where
    parserName = "pquery output test entry"
    parser = PqueryTestEntry
        <$> parser
        <*> ( $( string " depend=\""    ) *> parser )
        <*> ( $( string "\" rdepend=\"" ) *> parser )
        <*> ( $( string "\" bdepend=\"" ) *> parser )
        <*> ( $( string "\" pdepend=\"" ) *> parser )
        <*> ( $( string "\" idepend=\"" ) *> parser <* $( char '"' ) )

getPqueryDump :: IO [ByteString]
getPqueryDump = do
    exe <- pqueryPath
    let cp = (proc exe args)
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = CreatePipe }
    (ec, outBSs, errBS) <- sourceProcessWithStreams cp inC outC errC
    case ec of
        ExitFailure e -> do
            fail $ unlines
                [ "Failure when running " ++ exe ++ ": Got exit code " ++ show e
                , "stdout: " ++ show outBSs
                , "stdout: " ++ show errBS
                ]
        ExitSuccess -> pure outBSs
  where
    inC :: ConduitT () ByteString IO ()
    inC = pure ()

    outC :: ConduitT ByteString Void IO [ByteString]
    outC = linesUnboundedAsciiC .| sinkList

    errC :: ConduitT ByteString Void IO Lazy.ByteString
    errC = sinkLazy

    args :: [String]
    args =
        [ "--repo", "gentoo"
        , "--raw"
        , "--unfiltered"
        , "--atom"
        , "--cpv"
        , "--slot"
        , "--attr", "depend"
        , "--attr", "rdepend"
        , "--attr", "bdepend"
        , "--attr", "pdepend"
        , "--attr", "idepend"
        ]

pqueryPath :: IO FilePath
pqueryPath = findExecutable "pquery" >>= \case
    Just fp -> pure fp
    Nothing -> fail
        "Could not find pquery executable in $PATH. Install sys-apps/pkgcore first."
