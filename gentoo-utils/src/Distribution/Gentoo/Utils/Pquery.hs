{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Pquery
    ( runPquery
    , findPquery
    ) where

import System.Directory

import Distribution.Gentoo.Utils.Process

-- | Run @pquery@ with the given arguments
runPquery :: (Typeable out, Show out)
    => OutputType out -> [String] -> IO (StdOut out, StdErr out)
runPquery oType args = findPquery >>= \exe -> runSemiTransparent oType exe args

-- | Finds the @pquery@ executable in @PATH@.
--   Throws a fatal error if it is not found.
findPquery :: IO FilePath
findPquery = findExecutable "pquery" >>= \case
    Nothing -> error $ unwords
        [ "Could not find \"pquery\" executable."
        , "Please install sys-apps/pkgcore."
        ]
    Just exe -> pure exe
