{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Distribution.Gentoo.Utils.Pquery
    ( PkgDeps(..)
    , getPqueryDump
    , runPquery
    , module Control.Monad.Reader
    , module Validation
    ) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Validation

import Data.Conduit.Run
import Data.Parsable
import Distribution.Portage.Types

import Distribution.Gentoo.Utils.Exe

-- | A single package and all of its dependency specifications
data PkgDeps = PkgDeps
    { package :: (Package, Version, Slot)
    , depend :: DepBlock
    , rdepend :: DepBlock
    , bdepend :: DepBlock
    , pdepend :: DepBlock
    , idepend :: DepBlock
    }
    deriving (Show, Eq, Ord)

instance Parsable PkgDeps st String where
    parserName = "package depspec from pquery output line"
    parser = PkgDeps
        <$> (parser >>= toPkg)
        <*> ( $( string " depend=\""    ) *> parser )
        <*> ( $( string "\" rdepend=\"" ) *> parser )
        <*> ( $( string "\" bdepend=\"" ) *> parser )
        <*> ( $( string "\" pdepend=\"" ) *> parser )
        <*> ( $( string "\" idepend=\"" ) *> parser <* $( char '"' ) )
      where
        toPkg :: DepSpec -> ParserT st String (Package, Version, Slot)
        toPkg = \case
            VersionedDepSpec Nothing (VPkgEq p v) (Just s) Nothing
                -> pure (p, v, s)
            VersionedDepSpec Nothing (VPkgEq _ _) (Just _) _
                -> err "UseDependency found in PkgDeps parser"
            VersionedDepSpec Nothing (VPkgEq _ _) _ _
                -> err "No slot found in PkgDeps parser"
            VersionedDepSpec Nothing _ _ _
                -> err "VPkgEq not found in PkgDeps parser"
            VersionedDepSpec _ _ _ _
                -> err "Blocker found in PkgDeps parser"
            UnversionedDepSpec _ _ _ _
                -> err "UnversionedDepSpec found in PkgDeps parser"

-- | Run @pquery@ using default arguments plus the specified extra arguments,
--   returning a list of lines from @stdout@, parsed as 'PkgDeps' then converted
--   using the user-specified function.
--
--   Uses the following default arguments:
--
--   > --raw
--   > --unfiltered
--   > --atom
--   > --cpv
--   > --slot
--   > --attr depend
--   > --attr rdepend
--   > --attr bdepend
--   > --attr pdepend
--   > --attr idepend
--
--   This produces lines of output from @pquery@ that look like:
--
--   > =sys-apps/portage-3.0.67-r1:0 depend="..." rdepend="..." bdepend="..." pdepend="..." idepend="..."
getPqueryDump
       -- | Extra arguments to pass to @pquery@
    :: forall a. [String]
    -> (PkgDeps -> a)
    -> ExeEnv (Validation (NonEmpty (Maybe String)) [a])
getPqueryDump extraArgs f =
    parseOutLines <$> runPquery linesOutput (args ++ extraArgs)
  where
    args = [ "--raw"
           , "--unfiltered"
           , "--atom"
           , "--cpv"
           , "--slot"
           , "--attr", "depend"
           , "--attr", "rdepend"
           , "--attr", "bdepend"
           , "--attr", "pdepend"
           , "--attr", "idepend" ]

    parseOutLines
        :: (StdOut [ByteString], StdErr [ByteString])
        -> Validation (NonEmpty (Maybe String)) [a]
    parseOutLines (StdOut outLines, _) = traverse parseLine outLines

    parseLine :: ByteString -> Validation (NonEmpty (Maybe String)) a
    parseLine = either failure pure . fmap f . runParsable

-- | Run @pquery@ with the given arguments. Streams @stderr@ transparently to
--   the terminal.
runPquery :: (Typeable out, Show out)
    => OutputType out -> [String] -> ExeEnv (StdOut out, StdErr out)
runPquery oType args = runExe "pquery" $ \exe -> runSemiTransparent oType exe args
