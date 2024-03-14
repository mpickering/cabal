{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.SetupHooks.CallHooksExe
  ( callHooksExe
  , externalSetupHooks
  , buildTypeSetupHooks
  ) where

import Distribution.Compat.Prelude
import qualified Distribution.Compat.Binary as Binary

import Distribution.Simple.BuildPaths
  ( exeExtension )
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule
import Distribution.Simple.Utils
  ( ignoreSigPipe )
import Distribution.System
  ( buildPlatform )
import Distribution.Types.BuildType
  ( BuildType(..) )
import Distribution.Utils.Path
  ( SymbolicPath, FileOrDir(..)
  , interpretSymbolicPath
  )

import Control.Concurrent
    ( MVar, newEmptyMVar, putMVar, takeMVar
    , killThread, forkIO
    )
import Control.Exception
  ( onException, try, mask )
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Writer.CPS as MTL
import Data.ByteString.Lazy as LBS
  ( hGetContents
  , hPut
  )
import Data.Maybe
  ( fromJust
  )
import System.IO (hClose)
import qualified System.Process as P
import System.FilePath
  ( (</>), (<.>) )

import GHC.Stack

type HookIO inputs outputs =
  ( HasCallStack
  , Show inputs, Show outputs
  , Typeable inputs, Typeable outputs
  , Binary inputs, Binary outputs
  )

-- | Call an external hooks executable in order to execute a Cabal Setup hook.
callHooksExe
  :: forall inputs outputs
  .  HookIO inputs outputs
  => FilePath -- ^ path to hooks executable
  -> String   -- ^ name of the hook to run
  -> inputs   -- ^ argument to the hook
  -> IO outputs
callHooksExe hooksExe hookName inputs =
  P.withCreateProcess ((P.proc hooksExe [hookName]){P.std_in = P.CreatePipe, P.std_out = P.CreatePipe, P.std_err = P.Inherit}) $
    \mb_std_in mb_std_out _ ph -> do
      let std_in = fromJust mb_std_in
          std_out = fromJust mb_std_out
      -- fork off a thread to start consuming the output
      putStrLn $ "I am doing " ++ hookName
      output <- hGetContents std_out
      withForkWait (evaluate $ rnf output) $ \waitOut -> do
        -- now write any input
        ignoreSigPipe $ hPut std_in $ Binary.encode inputs
        -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
        ignoreSigPipe $ hClose std_in
        -- wait on the output
        waitOut
        hClose std_out
      ex <- P.waitForProcess ph
      putStrLn $ "I am done waiting on " ++ hookName
      case ex of
        ExitSuccess -> do
          putStrLn $ "Inner process succeeded for " ++ hookName
          let !res = Binary.decode output
          putStrLn $ "Decoding of outputs succeeded for " ++ hookName
          return res
        ExitFailure{} -> error $ "Hooks executable failed to run " ++ hookName
  -- SetupHooks TODO: use a logging handle?

withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

-- | Construct a 'SetupHooks' that runs the hooks of the external hooks executable
-- at the given path through the CLI.
--
-- This should only be used at the final step of compiling a package, when we
-- have all the hooks in hand. The SetupHooks that are returned by this function
-- cannot be combined with any other SetupHooks; they must directly be used to
-- build the package.
externalSetupHooks :: FilePath -> SetupHooks
externalSetupHooks hooksExe =
  SetupHooks
    { configureHooks =
        ConfigureHooks
          { preConfPackageHook = Just $ hook "preConfPackage"
          , postConfPackageHook = Just $ hook "postConfPackage"
          , preConfComponentHook = Just $ hook "preConfComponent"
          }
    , buildHooks =
        BuildHooks
          { preBuildComponentRules = Just $ Rules externalPreBuildRules
          , postBuildComponentHook = Just $ hook "postBuildComponent"
          }
    , installHooks =
        InstallHooks
          { installComponentHook = Just $ hook "installComponent"
          }
    }
  where
    hook :: HookIO inputs outputs => String -> inputs -> IO outputs
    hook = callHooksExe hooksExe
    externalPreBuildRules :: PreBuildComponentInputs -> RulesM ()
    externalPreBuildRules pbci =
      -- Bypass the pre-build rules API, directly returning the pre-build
      -- rules obtained by querying the external hooks executable.
      --
      -- This is OK because we are not going to combine these pre-build rules
      -- with any other pre-build rules at this point; we have the entire
      -- collection of pre-build rules used by the package in hand now.
      RulesT $ do
        (rulesMap, monitors) <- MTL.liftIO $ hook "preBuildRules" pbci
        MTL.put rulesMap
        MTL.tell monitors

buildTypeSetupHooks
  :: Maybe (SymbolicPath "CWD" (Dir "Package"))
  -> SymbolicPath "Package" (Dir "Dist")
  -> BuildType
  -> SetupHooks
buildTypeSetupHooks mbWorkDir distPref = \case
  Hooks -> externalSetupHooks hooksProgFile
  _ -> noSetupHooks
    -- SetupHooks TODO: if any built-in functionality is implemented using SetupHooks,
    -- we also need to include those even when using other build-types.
    -- Examples:
    --   - Configure build type, if implemented using Hooks,
    --   - Pre-processors implemented using pre-build rules

  where
    -- SetupHooks TODO: don't duplicate the following logic...
    hooksProgFile =
      interpretSymbolicPath mbWorkDir distPref
        </> "setup"
        </> "hooks"
        <.> exeExtension buildPlatform
