{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Emerge
    ( runEmerge
    ) where

import Data.Conduit.Run

import Distribution.Gentoo.Utils.Exe

-- | Run @emerge@ transparently with the given arguments
runEmerge :: (Typeable out, Show out)
    => OutputType out -> [String] -> ExeEnv (StdOut out, StdErr out)
runEmerge oType args = runExe "emerge" $ \exe -> runTransparent oType exe args
