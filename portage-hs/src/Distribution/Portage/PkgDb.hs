{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

{-# Options_GHC -Wno-deprecations #-}

module Distribution.Portage.PkgDb
    ( PkgDbEntry(..)
    , pkgDbDirectory
    , pkgDbEntries
    ) where

import Control.Monad.Reader
import Control.Monad.Trans.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import System.Directory
import System.FilePath.Posix

import Data.Parsable
import Distribution.Portage.Types

data PkgDbEntry = PkgDbEntry
    { pkgDbEntryPackage :: Package
    , pkgDbEntryPath :: FilePath
    }
    deriving (Show, Eq, Ord)

pkgDbDirectory :: FilePath
pkgDbDirectory = "/var/db/pkg"

pkgDbEntries :: IO (Map (Category, PkgName) (Set PkgDbEntry))
pkgDbEntries
    = fmap (M.fromListWith (<>))
    $ flip runReaderT pkgDbDirectory
    $ runListT
    $ checkDir $ \catDir -> checkDir $ \nameVerDir -> do
        p@(Package c n _ _ _) <- checkParsable (catDir </> nameVerDir)
        d <- ask
        pure ((c,n), S.singleton $ PkgDbEntry p d)
  where
    checkDir :: (FilePath -> ListT (ReaderT FilePath IO) a)
        -> ListT (ReaderT FilePath IO) a
    checkDir m = do
        d0 <- ask
        relD <- ListT $ liftIO $ listDirectory d0
        let d = d0 </> relD

        liftIO (doesDirectoryExist d) >>= guard
        case relD of
            '.' : _ -> mzero
            '-' : _ -> mzero
            []      -> mzero
            _       -> local (</> relD) $ m relD

    checkParsable :: (Parsable a Identity () String, MonadPlus m)
        => String -> m a
    checkParsable = either (const mzero) pure . runParsable ""
