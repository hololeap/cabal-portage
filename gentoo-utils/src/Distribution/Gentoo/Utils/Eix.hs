{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Eix
    ( runEix
    , runEixUpdate
    , findEix
    , findEixUpdate
    ) where

import System.Directory

import Distribution.Gentoo.Utils.Process

-- | Run @eix@ with the given arguments
runEix :: [String] -> IO (StdOut, StdErr)
runEix args = findEix >>= \exe -> runOpaque exe args

-- | Run @eix-update@ transparently with the given arguments
runEixUpdate :: [String] -> IO (StdOut, StdErr)
runEixUpdate args = findEixUpdate >>= \exe -> runTransparent exe args

-- | Finds the @eix@ executable in @PATH@.
--   Throws a fatal error if it is not found.
findEix :: IO FilePath
findEix = findExecutable "eix" >>= \case
    Nothing -> error $ unwords
        [ "Could not find \"eix\" executable."
        , "Please install app-portage/eix."
        ]
    Just exe -> pure exe

-- | Finds the @eix-update@ executable in @PATH@.
--   Throws a fatal error if it is not found.
findEixUpdate :: IO FilePath
findEixUpdate = findExecutable "eix-update" >>= \case
    Nothing -> error $ unwords
        [ "Could not find \"eix-update\" executable."
        , "Please install app-portage/eix."
        ]
    Just exe -> pure exe
