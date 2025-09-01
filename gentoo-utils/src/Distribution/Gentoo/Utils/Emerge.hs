{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Emerge
    ( runEmerge
    , findEmerge
    ) where

import Control.Exception.Safe (throwString)
import System.Directory

import Data.Conduit.Run

-- | Run @emerge@ transparently with the given arguments
runEmerge :: (Typeable out, Show out)
    => OutputType out -> [String] -> IO (StdOut out, StdErr out)
runEmerge oType args = findEmerge >>= \exe -> runTransparent oType exe args

-- | Finds the @emerge@ executable in @PATH@.
--   Throws a fatal error if it is not found.
findEmerge :: IO FilePath
findEmerge = fatal $ findExecutable "emerge" >>= \case
    Nothing -> throwString $ unwords
        [ "Could not find \"emerge\" executable."
        , "Please install sys-apps/portage."
        ]
    Just exe -> pure exe
