{-# Language LambdaCase #-}


module Distribution.Gentoo.Utils.Exe
    ( ExeMap
    , ExeEnv
    , runExeEnv
    , lookupExe
    , runExe
    , findExe
    ) where

import Control.Exception.Safe (throwString)
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import System.Directory

import Data.Conduit.Run

-- | A simple map between executable names and their filepath on the system
type ExeMap = HashMap String FilePath

-- | Allows for memoization of executables and their filepath on the system
type ExeEnv = ReaderT (TVar ExeMap) IO

runExeEnv :: ExeEnv a -> IO a
runExeEnv act = do
    tvar <- newTVarIO HM.empty
    runReaderT act tvar

lookupExe :: String -> ExeEnv (Maybe FilePath)
lookupExe exe = do
    tvar <- ask
    m <- liftIO $ atomically $ readTVar tvar
    pure $ HM.lookup exe m

-- | Run the given executable on the system, using the given action using it's
--   filepath. Memoizes executable file paths as it finds them. Throws a fatal
--   error if an executable is not found in @$PATH@.
runExe :: String -> (FilePath -> IO a) -> ExeEnv a
runExe exeName action = ask >>= \tvar -> liftIO $
    atomically (HM.lookup exeName <$> readTVar tvar) >>= \case
        Just exe -> action exe
        Nothing -> do
            exe <- findExe exeName
            atomically $ modifyTVar tvar $ HM.insert exeName exe
            action exe

-- | Finds the given executable name in @$PATH@.
--   Throws a fatal error if it is not found.
findExe :: String -> IO FilePath
findExe exeName = fatal $ findExecutable exeName >>= \case
    Nothing -> throwString $ unwords $
        [ "Could not find", "\"" ++ exeName ++ "\"", "executable."
        ] ++ pkgWords exeName
    Just exe -> pure exe

  where
    -- | Package installation message (to 'unwords'). If the package name was
    --   not found, emits an empty list.
    pkgWords :: String -> [String]
    pkgWords = foldMap (\pkg -> ["Please install", pkg]) . pkgLookup

    -- | Look up the exe name in the hard-coded map, returning Nothing if it
    --   is not included
    pkgLookup :: String -> Maybe String
    pkgLookup n = HM.lookup n pkgMap

    -- | Hard-coded map of executables to their portage package
    pkgMap :: HashMap String String
    pkgMap = HM.fromList
        [ ("emerge", "sys-apps/portage")
        , ("pquery", "sys-apps/pkgcore")
        , ("eix", "app-portage/eix")
        , ("eix-update", "app-portage/eix")
        ]
