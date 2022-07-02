{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

{-# Options_GHC -Wno-deprecations #-} -- Don't nag me about ListT

module GentooTests (gentooTests) where

import Control.Applicative
-- import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

gentooTests :: IO TestTree
gentooTests = do
    pkgStrings <- pkgTree
    pure $ testGroup "parse /var/db/pkg tree" $ go <$> pkgStrings
  where
    go :: String -> TestTree
    go pkgString = testCase pkgString $ do
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

-- | Scan the entire @/var/db/pkg@ tree for package atoms in the form
--   @<category>/<pkg-name>-<version>@. This is inherently part of the directory
--   structure inside @/var/db/pkg@.
pkgTree :: IO [String]
pkgTree = runListT $ do
    let portagePkgDir = "/var/db/pkg"
    checkDir portagePkgDir $ do

        cat <- listDir portagePkgDir           -- Scan for categories
        nv  <- listDir (portagePkgDir </> cat) -- Scan for names/versions

        case nv of
            '.' : _ -> empty
            '-' : _ -> empty
            []      -> empty
            _       -> pure $ cat </> nv
  where
    listDir :: FilePath -> ListT IO FilePath
    listDir = ListT . listDirectory

    checkDir :: FilePath -> ListT IO String -> ListT IO String
    checkDir d rest = do
        b <- lift $ doesDirectoryExist d
        if b
            then rest
            else lift $ assertFailure $
                    show d ++ " is not a directory. Is this really a Gentoo system?"
