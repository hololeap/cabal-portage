{-|
Module: Distribution.Gentoo.Utils.Process

General convenience functions and error handling for running executables on
the system and capturing the text they output.

NOTE: Because it uses 'sourceProcessWithStreams' from "Data.Conduit.Process",
this module requires a threaded runtime to function properly.
-}

{-# Language KindSignatures #-}
{-# Language ScopedTypeVariables #-}

module Distribution.Gentoo.Utils.Process
      -- * Running processes
    ( runTransparent
    , runSemiTransparent
    , runOpaque
      -- ** Low level
    , tryProcess
      -- * Defining output
    , OutputSink
      -- ** Output types
    , TextOutput
    , DataOutput
    , LinesOutput
      -- ** Output sinks
    , textOutput
    , lenientTextOutput
    , dataOutput
    , linesOutput
      -- ** Output stream tags
    , StdOut
    , StdErr
      -- * Handling errors
    , RunProcessException(..)
      -- * Re-exports
    , Typeable
    ) where

import Conduit
import Control.Exception.Safe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString as BS
import Data.Conduit.Process hiding (OutputSink)
import Data.Kind
import System.Exit
import System.IO
import qualified Data.Text.Lazy as Lazy

-- | Run a process while transparently dumping @stdout@ and @stderr@ data
--   streams to their respective handles, capturing the output for later use.
--   Throws a fatal error in the event of an exception.
runTransparent
    :: (Typeable out, Show out)
    -- | A conduit sink that defines what to do with the output
    => OutputSink out
    -- | Path of the executable
    -> FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut out, StdErr out)
runTransparent (OutputSink otr) exe args =
    tryProcess (transDump stdout .| otr) (transDump stderr .| otr) exe args
        >>= returnOrError

-- | Run a process while transparently dumping the @stderr@ data
--   stream to its respective handle, capturing the output for later use.
--   Throws a fatal error in the event of an exception.
runSemiTransparent
    :: (Typeable out, Show out)
    -- | A conduit sink that defines what to do with the output
    => OutputSink out
    -- | Path of the executable
    -> FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut out, StdErr out)
runSemiTransparent (OutputSink otr) exe args =
    tryProcess otr (transDump stderr .| otr) exe args
        >>= returnOrError

-- | Run a process, capturing the output for later use.
--   Throws a fatal error in the event of an exception.
runOpaque
    :: (Typeable out, Show out)
    -- | A conduit sink that defines what to do with the output
    => OutputSink out
    -- | Path of the executable
    -> FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut out, StdErr out)
runOpaque (OutputSink otr) exe args =
    tryProcess otr otr exe args
        >>= returnOrError

-- | Run a process and either return the @stdout@ and @stderr@ streams,
--   or a 'RunProcessException' in the event of an error.
--
--   Completely ignores @stdin@.
tryProcess
    :: forall out.
       -- | A conduit/sink for manipulating @stdout@ and writing to @out@
       --  (using e.g. 'sinkLazy').
        ConduitT ByteString Void IO out
       -- | A conduit/sink for manipulating @stderr@ and writing to @out@
       --  (using e.g. 'sinkLazy').
    -> ConduitT ByteString Void IO out
       -- | Path of the executable
    -> FilePath
       -- | Arguments to pass to the executable
    -> [String]
    -> IO (Either (RunProcessException out) (StdOut out, StdErr out))
tryProcess outC errC exe args = do
    let cp = (proc exe args)
                { std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                , delegate_ctlc = True }
    r <- tryAny $ sourceProcessWithStreams cp (pure ()) outC errC
    pure $ case r of
        Left e -> Left $ RPEException cp e
        Right (ec, o, e) -> case ec of
            ExitFailure _ -> Left $ RPEFailure cp ec o e
            ExitSuccess -> Right (o, e)

-- | > newtype OutputSink out
--   >     = OutputSink (forall m. MonadThrow m => ConduitT ByteString Void m out)
newtype OutputSink out
    = OutputSink (forall m. MonadThrow m => ConduitT ByteString Void m out)

-- | Output in the form of lazy text
type TextOutput = OutputSink Lazy.Text

-- | Output in the form of a lazy bytestring
type DataOutput = OutputSink LazyByteString

-- | Output in the form of a list of strict bytestrings, one per line of input
type LinesOutput = OutputSink [ByteString]

-- | Convert output to 'Data.Text.Text' using 'decodeUtf8C'
textOutput :: TextOutput
textOutput = OutputSink $ decodeUtf8C .| sinkLazy

-- | Convert output to 'Data.Text.Text' using 'decodeUtf8LenientC'
lenientTextOutput :: TextOutput
lenientTextOutput = OutputSink $ decodeUtf8LenientC .| sinkLazy

-- | Pass the 'ByteString' through unchanged
dataOutput :: DataOutput
dataOutput = OutputSink sinkLazy

-- | Split into lines using 'linesUnboundedAsciiC'
linesOutput :: LinesOutput
linesOutput = OutputSink $ linesUnboundedAsciiC .| sinkList

-- | A captured stream from @stdout@
type StdOut (out :: Type) = out

-- | A captured stream from @stderr@
type StdErr (out :: Type) = out

-- | A process either returned an 'ExitFailure' or an exception
data RunProcessException out
    = RPEFailure
        { rpeCreateProcess :: CreateProcess
        , rpeExitCode :: ExitCode
        , rpeStdout :: out
        , rpeStderr :: out
        }
    | RPEException
        { rpeCreateProcess :: CreateProcess
        , rpeException :: SomeException
        }
    deriving Show

instance (Typeable out, Show out) => Exception (RunProcessException out) where
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

-- | Transparently dump the incoming stream to the specified handle
transDump :: Handle -> ConduitT ByteString ByteString IO ()
transDump = iterMC . BS.hPut

returnOrError :: Exception e => Either e a -> IO a
returnOrError = either (error . displayException) pure

showCmdSpec :: CreateProcess -> String
showCmdSpec cp = case cmdspec cp of
    ShellCommand s -> s
    RawCommand fp ss -> unwords $ fp : ss
