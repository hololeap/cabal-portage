{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Distribution.Portage.PkgDb
    ( PkgDbEntry(..)
    , pkgDbDirectory
    ) where

import System.FilePath.Posix

import Distribution.Portage.Types

data PkgDbEntry = PkgDbEntry
    { pkgDbEntryPackage :: Package
    , pkgDbEntryPath :: FilePath
    }
    deriving (Show, Eq, Ord)

pkgDbDirectory :: FilePath
pkgDbDirectory = "/var" </> "db" </> "pkg"
