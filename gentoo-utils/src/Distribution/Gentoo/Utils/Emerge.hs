{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Emerge
    ( runEmerge
    , findEmerge
    ) where

import System.Directory

import Distribution.Gentoo.Utils.Process

-- | Run @emerge@ transparently with the given arguments
runEmerge :: [String] -> IO (StdOut, StdErr)
runEmerge args = findEmerge >>= \exe -> runTransparent exe args

-- | Finds the @emerge@ executable in @PATH@.
--   Throws a fatal error if it is not found.
findEmerge :: IO FilePath
findEmerge = findExecutable "emerge" >>= \case
    Nothing -> error $ unwords
        [ "Could not find \"emerge\" executable."
        , "Please install sys-apps/portage."
        ]
    Just exe -> pure exe
