{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

{-|
Module: Distribution.Simple.SetupHooks
Description: Interface for the @Hooks@ @build-type@.

This module defines the interface for the @Hooks@ @build-type@.

To write a package that implements @build-type: Hooks@, you should define
a module @SetupHooks.hs@ which exports a value @setupHooks :: 'SetupHooks'@.
This is a record that declares actions to hook into the cabal build process.

See 'SetupHooks' for more details.
-}
module Distribution.Simple.SetupHooks
  ( -- * Hooks

    -- $setupHooks
    SetupHooks(..)
  , noSetupHooks

     -- * Configure hooks

     -- $configureHooks
  , ConfigureHooks(..)
  , noConfigureHooks
    -- ** Per-package configure hooks
  , PreConfPackageInputs(..)
  , PreConfPackageOutputs(..) -- See Note [Not hiding SetupHooks constructors]
  , noPreConfPackageOutputs
  , PreConfPackageHook
  , PostConfPackageInputs(..)
  , PostConfPackageHook
    -- ** Per-component configure hooks
  , PreConfComponentInputs(..)
  , PreConfComponentOutputs(..) -- See Note [Not hiding SetupHooks constructors]
  , noPreConfComponentOutputs
  , PreConfComponentHook
  , ComponentDiff(..), emptyComponentDiff, buildInfoComponentDiff
  , LibraryDiff, ForeignLibDiff, ExecutableDiff
  , TestSuiteDiff, BenchmarkDiff
  , BuildInfoDiff

    -- * Build hooks

  , BuildHooks(..), noBuildHooks
  , BuildingWhat(..), buildingWhatVerbosity, buildingWhatDistPref

    -- ** Pre-build rules

    -- $preBuildRules
  , PreBuildComponentInputs(..)
  , PreBuildComponentRules

    -- ** Post-build hooks
  , PostBuildComponentInputs(..)
  , PostBuildComponentHook

    -- ** Rules
  , Rules(..) -- See Note [Not hiding SetupHooks constructors]
  , rules
  , noRules
  , Rule(..) -- See Note [Not hiding SetupHooks constructors]
  , simpleRule
    -- *** Rule inputs/outputs

    -- $rulesDemand
  , Location
  , findFileInDirs
  , autogenComponentModulesDir
  , componentBuildDir
  , MonitoredValue
  , MonitorFileOrDir(..)
  , MonitorKindFile(..)
  , MonitorKindDir(..)
    -- *** Actions
  , Action(..) -- See Note [Not hiding SetupHooks constructors]
  , simpleAction
  , ActionId

    -- *** Rules API

    -- $rulesAPI
  , RulesM
  , registerRule, registerAction
  , addRuleMonitors

    -- **** Local name generation for t'ActionId'
  , FreshT
    -- *** Convenience pre-build rules for common use cases
  , generateModules, AutogenFileContents, ModuleVisibility(..)

    -- * Install hooks
  , InstallHooks(..), noInstallHooks
  , InstallComponentInputs(..), InstallComponentHook

    -- * Re-exports

    -- ** Hooks
    -- *** Configure hooks
  , ConfigFlags(..)
    -- *** Build hooks
  , BuildFlags(..), ReplFlags(..), HaddockFlags(..), HscolourFlags(..)
    -- *** Install hooks
  , CopyFlags(..)

    -- ** @Hooks@ API
    --
    -- | These are functions provided as part of the @Hooks@ API.
    -- It is recommended to import them from this module as opposed to
    -- manually importing them from inside the Cabal module hierarchy.
  , installFileGlob, addKnownPrograms

    -- ** General @Cabal@ datatypes
  , Verbosity, Compiler(..), Platform(..), Suffix(..)

    -- *** Package information
  , LocalBuildConfig, LocalBuildInfo, PackageBuildDescr
      -- SetupHooks TODO: we can't simply re-export all the fields of
      -- LocalBuildConfig etc, due to the presence of duplicate record fields.
      -- Ideally we'd like to e.g. re-export LocalBuildConfig
      -- qualified, but qualified re-exports aren't a thing currently.

  , PackageDescription(..)

    -- *** Component information
  , Component(..), ComponentName(..), componentName
  , BuildInfo(..), emptyBuildInfo
  , TargetInfo(..), ComponentLocalBuildInfo(..)

    -- **** Components
  , Library(..), ForeignLib(..), Executable(..)
  , TestSuite(..), Benchmark(..)
  , LibraryName(..)
  , emptyLibrary, emptyForeignLib, emptyExecutable
  , emptyTestSuite, emptyBenchmark

    -- ** Programs
  , Program, ConfiguredProgram, ProgramDb, ProgArg

  )
where

import qualified Distribution.Compat.Binary as Binary
import qualified Distribution.Compat.Lens as Lens

import Distribution.ModuleName
  ( ModuleName, toFilePath )
import Distribution.PackageDescription
  ( PackageDescription(..)
  , Library(..), ForeignLib(..)
  , Executable(..), TestSuite(..), Benchmark(..)
  , emptyLibrary, emptyForeignLib
  , emptyExecutable, emptyBenchmark, emptyTestSuite
  , BuildInfo(..), emptyBuildInfo
  , ComponentName(..), LibraryName(..)
  )
import Distribution.Simple.Build
  ( AutogenFileContents )
import Distribution.Simple.Compiler
  ( Compiler(..) )
import Distribution.Simple.Install
  ( installFileGlob )
import Distribution.Simple.LocalBuildInfo
  ( componentBuildDir )
import Distribution.Simple.PreProcess.Types
  ( Suffix(..) )
import Distribution.Simple.Program.Db
  ( ProgramDb, addKnownPrograms )
import Distribution.Simple.Program.Types
  ( Program, ConfiguredProgram, ProgArg )
import Distribution.Simple.Setup
  ( BuildFlags(..)
  , ConfigFlags(..)
  , CopyFlags(..)
  , HaddockFlags(..)
  , HscolourFlags(..)
  , ReplFlags(..)
  )
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule as Rule
import Distribution.Simple.Utils
  ( findFirstFile, rewriteFileLBS )
import Distribution.System
  ( Platform(..) )
import qualified Distribution.Types.BuildInfo.Lens as Lens
import Distribution.Types.Component
  ( Component(..), componentName )
import Distribution.Types.ComponentId
  ( ComponentId )
import Distribution.Types.ComponentLocalBuildInfo
  ( ComponentLocalBuildInfo(..) )
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo(..) )
import Distribution.Types.LocalBuildConfig
  ( LocalBuildConfig, PackageBuildDescr )
import Distribution.Types.TargetInfo
  ( TargetInfo(..) )
import Distribution.Verbosity
  ( Verbosity )

import Control.Monad.IO.Class
  ( liftIO )
import Data.Foldable
  ( for_ )
import Data.Functor.Identity
  ( Identity(..) )
import Data.IORef
  ( IORef, newIORef, readIORef, atomicModifyIORef' )
import Data.List
  ( nub )
import qualified Data.List.NonEmpty as NE
  ( nonEmpty )
import Data.Map.Strict as Map
  ( Map, assocs, empty, insert, keys, lookup, lookupMax, mapMaybe )
import System.FilePath
  ( (</>) )
import System.IO.Unsafe
  ( unsafePerformIO )
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)

--------------------------------------------------------------------------------
-- Haddocks for the SetupHooks API

{- $setupHooks
A Cabal package with @Hooks@ @build-type@ must define the Haskell module
@SetupHooks@ which defines a value @setupHooks :: 'SetupHooks'@.

These *setup hooks* allow package authors to customise the configuration and
building of a package by providing certain hooks that get folded into the
general package configuration and building logic within @Cabal@.

This mechanism replaces the @Custom@ @build-type@, providing better
integration with the rest of the Haskell ecosystem.

Usage example:

> -- In your .cabal file
> build-type: Hooks
>
> custom-setup
>   setup-depends:
>     base        >= 4.18 && < 5,
>     Cabal-hooks >= 0.1  && < 0.3

> -- In SetupHooks.hs, next to your .cabal file
> module SetupHooks where
> import Distribution.Simple.SetupHooks ( SetupHooks, noSetupHooks )
>
> setupHooks :: SetupHooks
> setupHooks =
>  noSetupHooks
>    { configureHooks = myConfigureHooks
>    , buildHooks = myBuildHooks }

Note that 'SetupHooks' can be monoidally combined, e.g.:

> module SetupHooks where
> import Distribution.Simple.SetupHooks
> import qualified SomeOtherLibrary ( setupHooks )
>
> setupHooks :: SetupHooks
> setupHooks = SomeOtherLibrary.setupHooks <> mySetupHooks
>
> mySetupHooks :: SetupHooks
> mySetupHooks = ...
-}

{- $configureHooks
Configure hooks can be used to augment the Cabal configure logic with
package-specific logic. The main principle is that the configure hooks can
feed into updating the 'PackageDescription' of a @cabal@ package. From then on,
this package configuration is set in stone, and later hooks (e.g. hooks into
the build phase) can no longer modify this configuration; instead they will
receive this configuration in their inputs, and must honour it.

Configuration happens at two levels:

  * global configuration covers the entire package,
  * local configuration covers a single component.

Once the global package configuration is done, all hooks work on a
per-component level. The configuration hooks thus follow a simple philosophy:

  * All modifications to global package options must use `preConfPackageHook`.
  * All modifications to component configuration options must use `preConfComponentHook`.

For example, to generate modules inside a given component, you should:

  * In the per-component configure hook, declare the modules you are going to
    generate by adding them to the `autogenModules` field for that component
    (unless you know them ahead of time, in which case they can be listed
    textually in the @.cabal@ file of the project).
  * In the build hooks, describe the actions that will generate these modules.
-}

{- $preBuildRules
Pre-build hooks are specified in the form of a collection of pre-build 'Rules'.

Pre-build rules are specified by two pieces of information:

  - A collection of rules. Each t'Rule' declares its dependencies, its outputs,
    and refers to an action to run in order to execute the rule, in the form
    of an 'ActionId'.
  - A collection of actions. Each t'Action' is a function that takes in locations
    of dependencies and outputs as arguments, and returns an @IO@ action to
    execute.

To explain this structure, let us simplify the types for the time being and
remove the indirection of referring to an t'Action' by its t'ActionId'. We can
then think of rules as being specified by the following information:

> type Rules env = env -> IO [Rule]
> data Rule = Rule
>   { dependencies :: [Location]
>   , results :: [Location]
>   , action :: IO ()
>   }

That is, each rule declares dependencies, results, and an @IO@ action that
is given access to the declared dependencies and is expected to produce the
results at the specified locations.

In practice, the API is slightly complicated by the fact that each 'Rule'
indirectly stores the t'ActionId' of the t'Action' that executes it. Moreover,
a rule can additionally monitor certain paths and values, which determines when
the rule should be re-run. To construct a t'Rule' or a t'Action', you should use
the corresponding 'simpleRule' or 'simpleAction' smart constructor,
respectively.

See t'Rules' for a precise overview of how to define rules.
-}

{- $rulesDemand
Rules can declare various kinds of dependencies:

  - 'dependencies': files a rule depends on,
  - 'monitoredValue': a value to monitor,
  - 'MonitoredFileOrDir': additional files or directories to monitor.

Rules are considered __out-of-date__ precisely when any of the following
conditions apply:

  [O1] there has been a relevant change in the set of files and
       directories monitored by the rules.
  [O2] the environment passed to the computation of rules has changed,

If the rules are out-of-date, the build system is expected to re-run the
computation that computes all rules.

A rule is considered __stale__ if, after re-running the computation of all
of the rules, any of following conditions apply:

  [S1] a dependency of the rule has been modified/created/deleted,
       or a (transitive) rule dependency of the rule is itself stale.
  [S2] the monitored value is stale, i.e. either:

         * the 'monitoredValue' of the rule changed, or
         * the rule declares @monitoredValue = Nothing@.

A stale rule becomes no longer stale once we run its associated action; the
build system is responsible for re-running the actions associated with
each stale rule, in dependency order. This means the build system is expected
to behave as follows:

  1. Any time the rules are out-of-date, query the rules to obtain
     up-to-date rules.
  2. Re-run stale rules.
-}

{- $rulesAPI
Defining pre-build rules can be done in the following style:

> myPreBuildRules :: PreBuildComponentRules
> myPreBuildRules = rules $ \ preBuildEnvironment -> do
>   -- Pure code only here
>   let xyz = ... preBuildEnvironment
>   action1 <- registerAction $ simpleAction $ \ inLocs outLocs -> do { .. }
>   action2 <- registerAction $ simpleAction $ \ inLocs outLocs -> do { .. }
>   return $ do
>     -- IO actions allowed here
>     myData <- liftIO someIOAction
>     addRuleMonitors [ MonitorDir "someSearchDir" DirContents ]
>     registerRule $ simpleRule action1 deps1 outs1
>     registerRule $ simpleRule action1 deps2 outs2
>     registerRule $ simpleRule action1 deps3 outs3
>     registerRule $ simpleRule action2 deps4 outs4

Here we use the 'rules', 'simpleRule' and 'simpleAction' smart constructors,
rather than directly using the v'Rules', v'Rule' and v'Action' constructors,
which insulates us from internal changes to the t'Rules', t'Rule' and t'Action'
datatypes, respectively.

We use 'addRuleMonitorss' to declare a monitored directory that the collection
of rules as a whole depends on. In this case, we declare that they depend on the
contents of the "searchDir" directory. This means that the rules will be
computed anew whenever the contents of this directory change.

Additional convenience functions are also provided, such as the 'generateModules'
function which can be used to generate a collection of modules ex nihilo without
going through the above API. This doesn't preclude defining additional hooks,
e.g.:

> setupHooks :: SetupHooks
> setupHooks = generateModules f g <> myOtherSetupHooks
-}

{- Note [Not hiding SetupHooks constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We would like to hide as many datatype constructors from the API as possible
and provide smart constructors instead, so that hook authors don't end up
depending on internal implementation details that are subject to change.

However, doing so significantly degrades the Haddock documentation. So we
instead opt for exposing the constructor, but suggesting users use the
corresponding smart constructor instead.
-}

--------------------------------------------------------------------------------
-- Convenience pre-build rules for common use cases.

data ModuleVisibility
  = Exposed | Hidden

-- | Hooks for generating modules:
--
-- - a per-component configure hook that declares which autogenerated modules
--   to add to the package description,
-- - pre-build rules that generate the modules.
--
-- __Note__: if you know ahead of time which modules you are generating,
-- then you can include their names in the @.cabal@ file. You can then pass
-- @const $ return Map.empty@ as the first argument to this function.
generateModules
  :: (PreConfComponentInputs -> IO (Map ModuleName ModuleVisibility))
      -- ^ which autogen modules should be added to the package description?
  -> (PreBuildComponentInputs -> IO (Map ModuleName AutogenFileContents))
      -- ^ computation of generated module contents
  -> SetupHooks
generateModules getModNames getModsContents =
  noSetupHooks
    { configureHooks = noConfigureHooks
      { preConfComponentHook = Just declareModulesPreConfHook }
    , buildHooks = noBuildHooks
      { preBuildComponentRules = Just genModulesRules }
    }
  where
    declareModulesPreConfHook inputs@(PreConfComponentInputs { component = comp }) = do
      autogenMods <- getModNames inputs
      let compName = componentName comp
          ComponentDiff emptyCompDiff = emptyComponentDiff compName
          compDiff = ComponentDiff $
            Lens.set Lens.buildInfo
              (emptyBuildInfo{autogenModules = Map.keys autogenMods})
              (addExposedModules emptyCompDiff)
          addExposedModules c = case c of
            CLib lib -> CLib $ lib { exposedModules = newExposedMods }
            _
              | null newExposedMods
              -> c
              | otherwise
              -> error $
                   "generateModules: cannot add exposed-modules to non-library " ++ show (componentName c)
          newExposedMods = Map.keys $ Map.mapMaybe exposedMb autogenMods
          exposedMb Exposed = Just ()
          exposedMb Hidden  = Nothing
      return $
        PreConfComponentOutputs
          { componentDiff = compDiff }

    genModulesRules = Rules $
      \ inputs@( PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) -> do
        let verb = buildingWhatVerbosity what
            clbi = targetCLBI tgt
            compId = componentComponentId clbi
            autogenDir = autogenComponentModulesDir lbi clbi
        genModsActionId <- mdo
          actId <- registerAction $
            simpleAction $ \ _ _ -> do

              allContents <- updateComponentsGeneratedMods False
                               (compId, actId) (getModsContents inputs)
              for_ (Map.assocs allContents) $ \ (modNm, modContents) -> do
                let modFp = toFilePath modNm
                rewriteFileLBS verb (autogenDir </> modFp) modContents
          return actId
        return $ do
          mods <- liftIO $ updateComponentsGeneratedMods True
                              (compId, genModsActionId) (getModsContents inputs)
          case NE.nonEmpty $ Map.keys mods of
            Nothing -> error "generateModules: empty map of module contents"
            Just modNms -> do
              registerRule $
                (simpleRule genModsActionId
                  [] -- TODO: could generalise to allow deps
                  ( fmap ( \ modNm -> ( autogenDir, toFilePath modNm ) ) modNms )
                ) { monitoredValue = Just $ Binary.encode () }
  -- SetupHooks TODO: this currently only allows generating Haskell modules.
  -- It would be better to generalise this:
  --  - generate .lhs files (OK not very compelling)
  --  - generate .hs-boot files (I believe the Vulkan library can't use a
  --    Custom setup to generate modules because of this restriction)
  --  - generate non-Haskell files

-- TODO: explain this ad-hoc sharing mechanism which ensures that the IO action
-- to generate modules only gets re-run when we query the Rules, not every time
-- we run the Action.
componentsGeneratedMods :: IORef ( Map ( ComponentId, ActionId ) ( Map ModuleName AutogenFileContents ) )
componentsGeneratedMods = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE componentsGeneratedMods #-}

updateComponentsGeneratedMods
  :: Bool -- ^ always re-run the IO action?
  -> ( ComponentId, ActionId )
  -> IO ( Map ModuleName AutogenFileContents )
  -> IO ( Map ModuleName AutogenFileContents )
updateComponentsGeneratedMods alwaysRerun compActId getModsContents =
  if alwaysRerun
  then doIOAndModifyIORef
  else do
    compGenMods <- readIORef componentsGeneratedMods
    case Map.lookup compActId compGenMods of
      Just contents -> return contents
      Nothing -> doIOAndModifyIORef
  where
      -- Run the IO action and store its result in the IORef.
    doIOAndModifyIORef = do
      modsContents <- getModsContents
      atomicModifyIORef' componentsGeneratedMods $ \ mods ->
        (Map.insert compActId modsContents mods, ())
      return modsContents

