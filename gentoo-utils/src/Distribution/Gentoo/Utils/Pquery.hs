{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Distribution.Gentoo.Utils.Pquery
    ( PkgDeps(..)
    , getPqueryDump
    , runPquery
    , findPquery
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as Lazy
import System.Directory
import Validation

import Distribution.Portage.Types
import Data.Parsable

import Distribution.Gentoo.Utils.Process

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
    -> IO (Validation (NonEmpty (Maybe String)) [PkgDeps])
getPqueryDump extraArgs =
    traverse parseLine . Lazy.lines . fst
        <$> runPquery textOutput (args ++ extraArgs)
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

    parseLine :: Lazy.Text -> Validation (NonEmpty (Maybe String)) PkgDeps
    parseLine = either failure pure . runParsable . encodeUtf8 . Lazy.toStrict

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
