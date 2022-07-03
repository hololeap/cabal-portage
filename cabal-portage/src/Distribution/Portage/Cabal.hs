{-|
Module      : Distribution.Portage.Cabal

Main compatibility layer between Cabal and Portage
-}

{-# Language ApplicativeDo #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language InstanceSigs #-}
{-# Language OverloadedStrings #-}
{-# Language PartialTypeSignatures #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language ViewPatterns #-}

{-# Options_GHC -Wno-deprecations #-} -- Don't nag me about ListT

module Distribution.Portage.Cabal
    ( scanPkgDir
    ) where

import Control.Applicative hiding (many)
import Control.Monad.IO.Class
import Control.Monad.Trans.List
import Control.Monad.Except
import Control.Monad.Trans.Writer.CPS
import Data.Functor.Identity
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup (First(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath.Posix
import System.Process
import Text.Parsec
import Text.Parsec.Text

import Data.Parsable
import Distribution.Portage.Cabal.Error
import qualified Distribution.Portage.Types as P

type MonadCabalPortage m =
    (MonadIO m, MonadError CabalPortageError m)

-- | A @CONTENTS@ file in the portage package directory
newtype ContentsFile = ContentsFile FilePath
    deriving (Show, Eq, Ord, IsString, Printable)

-- | Hardcoded path for portage's package directory
portagePkgDir :: FilePath
portagePkgDir = "/var/db/pkg"

-- | Scan the portage package directory for all valid @CONTENTS@ files,
--   returning each pair containing the portage package and its @CONTENTS@ file
--   path.
scanPkgDir :: forall m. MonadCabalPortage m => m [(_, ContentsFile)]
scanPkgDir = runListT $ do
    -- Error if portage's package directory does not exist
    lift $ checkDir portagePkgDir

    cat <- listDir portagePkgDir           -- Scan for categories
    nv  <- listDir (portagePkgDir </> cat) -- Scan for names/versions

    case nv of
        '.' : _ -> empty
        '-' : _ -> empty
        []      -> empty
        _       -> do
            let pkgS = cat ++ "/" ++ nv -- The full portage package string

            -- Try to parse the package string, otherwise throw PortageParseError
            -- This will help test the parsers
            CompleteParse pkg <- parseCabalPortage (PortageParseError pkgS) "package string" pkgS

            -- Error if CONTENTS file does not exist
            let cf = portagePkgDir </> cat </> nv </> "CONTENTS"
            lift $ checkFile cf

            pure (pkg, ContentsFile cf)
  where
    listDir :: FilePath -> ListT m FilePath
    listDir = ListT . liftIO . listDirectory

-- isCabalPackage :: GhcLibDir -> ContentsFile ->

-- | A file such as
--   /usr/lib64/ghc-9.0.2/gentoo/portage-empty-dev-haskell-comonad-5.0.8.conf
newtype EmptyConf = EmptyConf FilePath
    deriving (Show, Eq, Ord, IsString, Printable)

-- emptyConf :: MonadCabalPortage m => PortagePkg -> m EmptyConf
-- emptyConf (PortagePkg cat pkgName ver) = do
--     GhcLibDir gld <- ghcLibDir
--     let fn = L.intercalate "-"
--             [ "portage-empty"
--             , toString cat
--             , toString pkgName
--             , toString ver
--             ]
--     pure $ EmptyConf $ gld </> fn <.> "conf"

-- | A file such as
--   /usr/lib64/ghc-9.0.2/gentoo/portage-dev-haskell-comonad-5.0.8-comonad-5.0.8.conf
newtype GentooConf = GentooConf FilePath
    deriving (Show, Eq, Ord, IsString, Printable)

-- | A file such as
--   /usr/lib64/ghc-9.0.2/package.conf.d/comonad-5.0.8-52Q6ID8hBaMLv6UpYwuY5V.conf
newtype CabalConf = CabalConf FilePath
    deriving (Show, Eq, Ord, IsString, Printable)

type Contents =
    ( Maybe (First EmptyConf)
    , Maybe (First GentooConf)
    , Maybe (First CabalConf)
    )

-- scanContentsFile :: MonadCabalPortage m => ContentsFile -> m Contents
-- scanContentsFile cf = do
--     c <- liftIO $ T.readFile cf


-- | The path of the library directory, as reported by @ghc --print-libdir@
newtype GhcLibDir = GhcLibDir FilePath
    deriving (Show, Eq, Ord, IsString, Printable)

ghcLibDir :: MonadCabalPortage m => m GhcLibDir
ghcLibDir = do
    dir <- liftIO $ readProcess "ghc" ["--print-libdir"] []
    checkDir dir
    pure $ GhcLibDir dir

checkDir :: MonadCabalPortage m => FilePath -> m ()
checkDir d = liftIO (doesDirectoryExist d)
    >>= flip unless (throwError (NoSuchDirectory d))

checkFile :: MonadCabalPortage m => FilePath -> m ()
checkFile f = liftIO (doesFileExist f)
    >>= flip unless (throwError (NoSuchFile f))
