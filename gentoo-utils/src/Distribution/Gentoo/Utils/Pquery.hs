{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Distribution.Gentoo.Utils.Pquery
    ( PkgDeps(..)
    , getPqueryDump
    , runPquery
    ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Validation

import Data.Conduit.Run
import Data.Parsable
import Distribution.Portage.Types

import Distribution.Gentoo.Utils.Exe

-- | A single package and all of its dependency specifications
data PkgDeps = PkgDeps
    { package :: DepSpec
    , depend :: DepBlock
    , rdepend :: DepBlock
    , bdepend :: DepBlock
    , pdepend :: DepBlock
    , idepend :: DepBlock
    }
    deriving (Show, Eq, Ord)

instance Parsable PkgDeps st String where
    parserName = "pquery output test entry"
    parser = PkgDeps
        <$> parser
        <*> ( $( string " depend=\""    ) *> parser )
        <*> ( $( string "\" rdepend=\"" ) *> parser )
        <*> ( $( string "\" bdepend=\"" ) *> parser )
        <*> ( $( string "\" pdepend=\"" ) *> parser )
        <*> ( $( string "\" idepend=\"" ) *> parser <* $( char '"' ) )

-- | Run @pquery@ using default arguments plus the specified extra arguments,
--   returning a list of lines from @stdout@, parsed as 'PqueryLine'.
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
    :: [String]
    -> ExeEnv (Validation (NonEmpty (Maybe String)) [PkgDeps])
getPqueryDump extraArgs =
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
        -> Validation (NonEmpty (Maybe String)) [PkgDeps]
    parseOutLines (StdOut outLines, _) = traverse parseLine outLines

    parseLine :: ByteString -> Validation (NonEmpty (Maybe String)) PkgDeps
    parseLine = either failure pure . runParsable

-- | Run @pquery@ with the given arguments. Streams @stderr@ transparently to
--   the terminal.
runPquery :: (Typeable out, Show out)
    => OutputType out -> [String] -> ExeEnv (StdOut out, StdErr out)
runPquery oType args = runExe "pquery" $ \exe -> runSemiTransparent oType exe args
