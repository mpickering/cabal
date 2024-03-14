{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.SetupHooks.HooksExe
  ( hooksMain ) where

import Distribution.Compat.Prelude
import qualified Distribution.Compat.Binary as Binary

import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule
import Distribution.Simple.Utils
  ( dieWithException )
import Distribution.Types.Component
  ( componentName )
import qualified Distribution.Types.LocalBuildConfig as LBC
import qualified Distribution.Verbosity as Verbosity

import Distribution.Client.SetupHooks.Errors

import Data.ByteString.Lazy as LBS
  ( getContents
  , hPutStr
  , putStr
  )
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (stderr)

import GHC.Stack

-- | Create an executable which accepts the name of a hook as the argument,
-- then reads arguments to the hook over stdin and writes the results of the hook
-- to stdout.
hooksMain :: SetupHooks -> IO ()
hooksMain setupHooks = do
  args <- getArgs
  case args of
    [] -> dieWithException Verbosity.normal MissingHooksExeArg
    hookName : _hookArgs ->
      case lookup hookName allHookHandlers of
        Just handleAction -> handleAction setupHooks
        Nothing ->
          dieWithException Verbosity.normal $
            BadHooksExeArgs hookName $
              UnknownHookType
                { knownHookTypes = map fst allHookHandlers
                }
  where

    allHookHandlers =
      [ (nm, action)
      | HookHandler
          { hookName = nm
          , hookHandler = action
          } <-
          hookHandlers
      ]

-- | Implementation of a particular hook in a separate hooks executable,
-- which receives its inputs from stdin and returns outputs to stdout.
runHookHandle
  :: forall inputs outputs
   . (Binary inputs, Binary outputs)
  => String
  -- ^ Hook name
  -> (inputs -> IO outputs)
  -- ^ Hook to run; inputs are passed via stdin
  -> IO ()
runHookHandle hookName hook = do
  inputsData <- LBS.getContents
  hPutStr stderr ("runHook " <> fromString hookName <> ": got stdin\n")
  let mb_inputs = Binary.decodeOrFail inputsData
  case mb_inputs of
    Left _ -> exitWith $ ExitFailure 99
    Right (_, _, inputs) -> do
      -- hPrint stderr inputs
      output <- hook inputs
      hPutStr stderr ("runHook " <> fromString hookName <> ": ran hook\n")
      LBS.putStr $ Binary.encode output

data HookHandler = HookHandler
  { hookName :: !String
  , hookHandler :: SetupHooks -> IO ()
  }

hookHandlers :: [HookHandler]
hookHandlers =
  [ let hookName = "preConfPackage"
        noHook (PreConfPackageInputs{localBuildConfig = lbc}) =
          return $
            PreConfPackageOutputs
              { buildOptions = LBC.withBuildOptions lbc
              , extraConfiguredProgs = Map.empty
              }
     in HookHandler hookName $ \(SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run the package-wide pre-configure hook.
          runHookHandle hookName $ fromMaybe noHook preConfPackageHook
  , let hookName = "postConfPackage"
     in HookHandler hookName $ \(SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run the package-wide post-configure hook.
          for_ postConfPackageHook $ runHookHandle hookName
  , let hookName = "preConfComponent"
        noHook (PreConfComponentInputs{component = c}) =
          return $ PreConfComponentOutputs{componentDiff = emptyComponentDiff $ componentName c}
     in HookHandler hookName $ \(SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run a per-component pre-configure hook; the choice of component
          -- is determined by the input passed to the hook.
          runHookHandle hookName $ fromMaybe noHook preConfComponentHook
  , let hookName = "preBuildRules"
     in HookHandler hookName $ \(SetupHooks{buildHooks = BuildHooks{..}}) ->
          -- Return all pre-build rules.
          runHookHandle hookName $ \preBuildInputs ->
            case preBuildComponentRules of
              Nothing -> return (Map.empty, [])
              Just pbcRules ->
                computeRules Verbosity.normal preBuildInputs pbcRules
  , let hookName = "runPreBuildRuleDeps"
     in HookHandler hookName $ \_ ->
          -- Run the given pre-build rule dependency computation.
          runHookHandle hookName $ \(ruleId, ruleDeps) ->
            case runRuleDynDepsCmd ruleDeps of
              Nothing -> dieWithException Verbosity.normal $ BadHooksExeArgs hookName $ NoDynDepsCmd ruleId
              Just getDeps -> getDeps
  , let hookName = "runPreBuildRule"
     in HookHandler hookName $ \_ ->
          -- Run the given pre-build rule.
          runHookHandle hookName $ \(_ruleId :: RuleId, rExecCmd) ->
            runRuleExecCmd rExecCmd
  , let hookName = "postBuildComponent"
     in HookHandler hookName $ \(SetupHooks{buildHooks = BuildHooks{..}}) ->
          -- Run the per-component post-build hook.
          for_ postBuildComponentHook $ runHookHandle hookName
  , let hookName = "installComponent"
     in HookHandler hookName $ \(SetupHooks{installHooks = InstallHooks{..}}) ->
          -- Run the per-component copy/install hook.
          for_ installComponentHook $ runHookHandle hookName
  ]
