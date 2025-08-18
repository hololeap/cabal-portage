{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Emerge
    ( runEmerge
    , findEmerge
    ) where

import System.Directory

import Distribution.Gentoo.Utils.Process

-- | Run @emerge@ transparently with the given arguments
runEmerge :: (Typeable out, Show out)
    => OutputType out -> [String] -> IO (StdOut out, StdErr out)
runEmerge oType args = findEmerge >>= \exe -> runTransparent oType exe args

-- | Finds the @emerge@ executable in @PATH@.
--   Throws a fatal error if it is not found.
findEmerge :: IO FilePath
findEmerge = findExecutable "emerge" >>= \case
    Nothing -> error $ unwords
        [ "Could not find \"emerge\" executable."
        , "Please install sys-apps/portage."
        ]
    Just exe -> pure exe
