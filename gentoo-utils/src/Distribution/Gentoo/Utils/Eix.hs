{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Eix
    ( runEix
    , runEixUpdate
    , findEix
    , findEixUpdate
    ) where

import System.Directory

import Data.Conduit.Run

-- | Run @eix@ with the given arguments
runEix :: (Typeable out, Show out)
    => OutputType out -> [String] -> IO (StdOut out, StdErr out)
runEix oType args = findEix >>= \exe -> runOpaque oType exe args

-- | Run @eix-update@ transparently with the given arguments
runEixUpdate :: (Typeable out, Show out)
    => OutputType out -> [String] -> IO (StdOut out, StdErr out)
runEixUpdate oType args = findEixUpdate >>= \exe -> runTransparent oType exe args

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
