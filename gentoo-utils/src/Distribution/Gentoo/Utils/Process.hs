{-|
Module: Distribution.Gentoo.Utils.Process

General convenience functions and error handling for running executables on
the system and capturing the text they output.

NOTE: Because it uses 'sourceProcessWithStreams' from "Data.Conduit.Process",
this module requires a threaded runtime to function properly.
-}

module Distribution.Gentoo.Utils.Process
    ( runTransparent
    , runOpaque
      -- ** Low level
    , tryProcess
    , StdOut
    , StdErr
    , RunProcessException(..)
    ) where

import Conduit
import Control.Exception.Safe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit.Process
import Data.Maybe (fromMaybe)
import System.Exit
import System.IO
import qualified Data.Text.Lazy as Lazy

-- | Run a process while transparently dumping @stdout@ and @stderr@ data
--   streams to their respective handles, capturing the output as lazy text
--   streams for later use. Throws a fatal error in the event of an exception.
runTransparent
    -- | Path of the executable
    :: FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut, StdErr)
runTransparent exe args =
    tryProcess (Just (transDump stdout)) (Just (transDump stderr)) exe args
        >>= either (error . displayException) pure
  where
    transDump :: Handle -> ConduitT ByteString ByteString IO ()
    transDump = iterMC . BS.hPut

-- | Run a process, capturing the output as lazy text streams for later use.
--   Throws a fatal error in the event of an exception.
runOpaque
    -- | Path of the executable
    :: FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut, StdErr)
runOpaque exe args =
    tryProcess Nothing Nothing exe args
        >>= either (error . displayException) pure

-- | Run a process and either return the @stdout@ and @stderr@ lazy text streams,
--   or a 'RunProcessException' in the event of an error.
--
--   Completely ignores @stdin@.
tryProcess
       -- | An optional conduit for manipulating @stdout@
    :: Maybe (ConduitT ByteString ByteString IO ())
       -- | An optional conduit for manipulating @stderr@
    -> Maybe (ConduitT ByteString ByteString IO ())
       -- | Path of the executable
    -> FilePath
       -- | Arguments to pass to the executable
    -> [String]
    -> IO (Either RunProcessException (StdOut, StdErr))
tryProcess mOutC mErrC exe args = do
    let cp = (proc exe args)
                { std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                , delegate_ctlc = True }
    r <- tryAny $ sourceProcessWithStreams cp inC outC errC
    pure $ case r of
        Left e -> Left $ RPEException cp e
        Right (ec, o, e) -> case ec of
            ExitFailure _ -> Left $ RPEFailure cp ec o e
            ExitSuccess -> Right (o, e)

  where
    inC :: ConduitT () ByteString IO ()
    inC = pure ()

    outC :: ConduitT ByteString Void IO Lazy.Text
    outC = fromMaybe passC mOutC .| decodeUtf8C .| sinkLazy

    errC :: ConduitT ByteString Void IO Lazy.Text
    errC = fromMaybe passC mErrC .| decodeUtf8C .| sinkLazy

    passC :: Monad m => ConduitT a a m ()
    passC = mapC id

-- | A captured stream of lazy text from @stdout@
type StdOut = Lazy.Text

-- | A captured stream of lazy text from @stderr@
type StdErr = Lazy.Text

-- | A process either returned an 'ExitFailure' or an exception
data RunProcessException
    = RPEFailure
        { rpeCreateProcess :: CreateProcess
        , rpeExitCode :: ExitCode
        , rpeStdout :: Lazy.Text
        , rpeStderr :: Lazy.Text
        }
    | RPEException
        { rpeCreateProcess :: CreateProcess
        , rpeException :: SomeException
        }
    deriving Show

instance Exception RunProcessException where
    displayException (RPEFailure cp ec out er) = unlines $
        [ "Error when running \"" ++ showCmdSpec cp ++ "\""
        , "Exit code: " ++ show ec
        , "stdout: " ++ show out
        , "stderr: " ++ show er
        ]
    displayException (RPEException cp e) = unlines $
        [ "Error when running \"" ++ showCmdSpec cp ++ "\""
        , displayException e
        ]

showCmdSpec :: CreateProcess -> String
showCmdSpec cp = case cmdspec cp of
    ShellCommand s -> s
    RawCommand fp ss -> unwords $ fp : ss
