{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

{-# Options_GHC -Wno-deprecations #-} -- Don't nag me about ListT

module GentooTests (gentooTests) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit

import Internal.Distribution.Portage.Types
import Internal.Distribution.Portage.Emerge
import Data.Parsable
import Test.Parsable

gentooTests :: IO TestTree
gentooTests = testGroup "gentoo system tests" <$> sequenceA
    [ gentooParseTests "/var/db/repos parse tests" <$> repoTree
    , gentooParseTests "/var/db/pkg parse tests" <$> pkgTree
    , pure emergeWorldTest
    ]

-- | Emerge world "test". Currently just writes to
--   @/tmp/emerge-world-test.stdout@,
--   @/tmp/emerge-world-test.stderr@,
--   @/tmp/emerge-world-test.exitcode@
emergeWorldTest :: TestTree
emergeWorldTest = testCase "emerge world \"test\"" $ do
    (c, o, e) <- emergeProcess emergeExe $ upgradeWorldArgs ++ pretendArgs
    T.writeFile "/tmp/emerge-world-test.stdout" o
    T.writeFile "/tmp/emerge-world-test.stderr" e
    writeFile "/tmp/emerge-world-test.exitcode" $ show c


-- | Check all package atoms and make sure they are parsed successfully and
--   pass the "roundtrip" test.
gentooParseTests :: TestName -> [String] -> TestTree
gentooParseTests n pkgStrings = testGroup n $ go <$> pkgStrings
  where
    go :: String -> TestTree
    go pkgString = testCase (show pkgString) $ do
        let pkgE = runParsable @Package @Void "" pkgString
        case pkgE of
            Left e ->
                assertFailure $
                       "Could not parse string as a Package:\n"
                    ++ "Input: " ++ show pkgString ++ "\n"
                    ++ "Error:\n" ++ errorBundlePretty e
            Right p@(PartialParse _, _) ->
                assertFailure $
                       "Got a PartialParse when it should be a CompleteParse:\n"
                    ++ "Input: " ++ show pkgString ++ "\n"
                    ++ "Output: " ++ show p ++ "\n"
            Right (CompleteParse,pkg) ->
                assertEqual "roundtrip String->Package->String" pkgString (toString pkg)

-- | Scan the entire @/var/db/repos@ tree for ebuilds and form these into
--  pacakge atoms.
repoTree :: IO [String]
repoTree = runListT $ do
    let reposDir = "/var/db/repos"
    checkDir reposDir $ do

        repo <- listDir reposDir
        cat  <- listDir $ reposDir </> repo
        n    <- listDir $ reposDir </> repo </> cat
        eb   <- listDir $ reposDir </> repo </> cat </> n

        let (nv,ext) = splitExtension eb

        guard $ ext == ".ebuild"

        pure $ cat ++ "/" ++ nv ++ "::" ++ repo

-- | Scan the entire @/var/db/pkg@ tree for package atoms in the form
--   @<category>/<pkg-name>-<version>@. This is inherently part of the directory
--   structure inside @/var/db/pkg@.
pkgTree :: IO [String]
pkgTree = runListT $ do
    let portagePkgDir = "/var/db/pkg"
    checkDir portagePkgDir $ do

        cat <- listDir portagePkgDir           -- Scan for categories
        nv  <- listDir $ portagePkgDir </> cat -- Scan for names/versions

        case nv of
            '.' : _ -> empty
            '-' : _ -> empty
            []      -> empty
            _       -> pure $ cat ++ "/" ++ nv

listDir :: FilePath -> ListT IO FilePath
listDir d = do
    lift (doesDirectoryExist d) >>= guard
    ListT $ listDirectory d

checkDir :: FilePath -> ListT IO a -> ListT IO a
checkDir d rest = do
    b <- lift $ doesDirectoryExist d
    if b
        then rest
        else lift $ assertFailure $
                show d ++ " is not a directory. Is this really a Gentoo system?"
