

module Distribution.Portage.Emerge
    ( EmergeArgs
    , StdOut
    , StdErr
    , emergeExe
    , upgradeWorldArgs
    , singleArgs
    , pretendArgs
    , stripPackageVersion
    , emergeProcess
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.Process

import Data.Parsable
import Distribution.Portage.Types

type EmergeArgs = [String]
type StdOut = Text
type StdErr = Text


emergeExe :: FilePath
emergeExe = "/usr/bin/emerge"

upgradeWorldArgs :: EmergeArgs
upgradeWorldArgs = commonEmergeArgs ++
    [ "--deep"
    , "--changed-use"
    , "--update"
    , "--complete-graph"
    , "@world"
    ]

singleArgs :: [Package] -> EmergeArgs
singleArgs ps = commonEmergeArgs ++
    [ "--oneshot"
    , "--complete-graph"
    ] ++
    (packageString <$> ps)

pretendArgs :: EmergeArgs
pretendArgs = [ "--pretend" ]

-- | Add a @'='@ character to the front of the string if the version is
--   specified.
packageString :: Package -> String
packageString p@(Package _ _ v _ _) =
    let pre = case v of
                Just _  -> "="
                Nothing -> ""
    in pre ++ toString p

stripPackageVersion :: Package -> Package
stripPackageVersion (Package c n _ s r) = Package c n Nothing s r

commonEmergeArgs :: EmergeArgs
commonEmergeArgs =
    [ "--ignore-default-opts"
    , "--color=n"
    , "--nospinner"
    , "--verbose"
    ]

emergeProcess :: FilePath -> EmergeArgs -> IO (ExitCode, StdOut, StdErr)
emergeProcess p a = do
    (c, o, e) <- readProcessWithExitCode p a ""
    pure (c, T.pack o, T.pack e)
