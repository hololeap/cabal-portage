{-# Language LambdaCase #-}

module Distribution.Gentoo.Utils.Eix
    ( runEix
    , runEixUpdate
    ) where

import Data.Conduit.Run

import Distribution.Gentoo.Utils.Exe

-- | Run @eix@ with the given arguments
runEix :: (Typeable out, Show out)
    => OutputType out -> [String] -> ExeEnv (StdOut out, StdErr out)
runEix oType args = runExe "eix" $ \exe -> runOpaque oType exe args

-- | Run @eix-update@ transparently with the given arguments
runEixUpdate :: (Typeable out, Show out)
    => OutputType out -> [String] -> ExeEnv (StdOut out, StdErr out)
runEixUpdate oType args = runExe "eix-update" $ \exe -> runTransparent oType exe args
