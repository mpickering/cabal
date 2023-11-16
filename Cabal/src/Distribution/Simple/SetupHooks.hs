{-# LANGUAGE DuplicateRecordFields #-}

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
  ( -- * The setup hooks datatype
    SetupHooks(..), noSetupHooks

     -- * Configure hooks
  , ConfigureHooks(..), noConfigureHooks
    -- ** Per-package configure hooks
  , PreConfPackageHook, PostConfPackageHook
    -- ** Per-component configure hooks
  , PreConfComponentHook, PostConfComponentHook
  , ComponentDiff(..), emptyComponentDiff, buildInfoComponentDiff
  , LibraryDiff, ForeignLibDiff, ExecutableDiff
  , TestSuiteDiff, BenchmarkDiff
  , BuildInfoDiff

    -- * Build hooks
  , BuildHooks(..), noBuildHooks
  , BuildingWhat(..), buildingWhatVerbosity, buildingWhatDistPref
  , BuildComponentHook

    -- * Copy hooks
  , CopyHooks(..), noCopyHooks
  , CopyComponentHook

    -- * Clean hooks
  , CleanHooks(..), noCleanHooks
  , CleanPackageHook

    -- * Test hooks
  , TestHooks(..), noTestHooks
  , TestPackageHook, TestComponentHook

    -- * Bench hooks
  , BenchmarkHooks(..), noBenchmarkHooks
  , BenchmarkPackageHook, BenchmarkComponentHook

    -- * Re-exports

    -- ** Hooks
    -- *** Configure hooks
  , ConfigFlags(..)
    -- *** Build hooks
  , BuildFlags(..), ReplFlags(..), HaddockFlags(..), HscolourFlags(..)
    -- *** Copy hooks
  , CopyFlags(..)
    -- *** Clean hooks
  , CleanFlags(..)
    -- *** Test hooks
  , TestFlags(..)
    -- *** Benchmark hooks
  , BenchmarkFlags(..)

    -- ** @Hooks@ API
    --
    -- | These are functions provided as part of the @Hooks@ API.
    -- It is recommended to import them from this module as opposed to
    -- manually importing them from inside the Cabal module hierarchy.
  , installFileGlob, addKnownPrograms

    -- ** General @Cabal@ datatypes
  , Compiler(..), Platform(..)

    -- *** Package information
  , LocalBuildConfig, LocalBuildInfo, PackageBuildDescr
      -- SetupHooks TODO: we can't simply re-export all the fields of
      -- LocalBuildConfig etc, due to the presence of duplicate record fields.
      -- Ideally we'd like to e.g. re-export LocalBuildConfig
      -- qualified, but qualified re-exports aren't a thing currently.

  , PackageDescription(..), ProgramDb

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

  )
where

import Distribution.PackageDescription
  ( PackageDescription(..)
  , Library(..), ForeignLib(..)
  , Executable(..), TestSuite(..), Benchmark(..)
  , emptyLibrary, emptyForeignLib
  , emptyExecutable, emptyBenchmark, emptyTestSuite
  , BuildInfo(..), emptyBuildInfo
  , ComponentName(..), LibraryName(..)
  )
import Distribution.Simple.Compiler
  ( Compiler(..) )
import Distribution.Simple.Install
  ( installFileGlob )
import Distribution.Simple.Program.Db
  ( ProgramDb, addKnownPrograms )
import Distribution.Simple.Setup
  ( ReplFlags(..), HscolourFlags (..) )
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.Setup.Benchmark
  ( BenchmarkFlags(..) )
import Distribution.Simple.Setup.Build
  ( BuildFlags(..) )
import Distribution.Simple.Setup.Clean
  ( CleanFlags(..) )
import Distribution.Simple.Setup.Config
  ( ConfigFlags(..) )
import Distribution.Simple.Setup.Copy
  ( CopyFlags(..) )
import Distribution.Simple.Setup.Haddock
  ( HaddockFlags(..) )
import Distribution.Simple.Setup.Test
  ( TestFlags(..) )
import Distribution.System
  ( Platform(..) )
import Distribution.Types.Component
  ( Component(..), componentName )
import Distribution.Types.ComponentLocalBuildInfo
  ( ComponentLocalBuildInfo(..) )
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo(..) )
import Distribution.Types.LocalBuildConfig
  ( LocalBuildConfig, PackageBuildDescr )
import Distribution.Types.TargetInfo
  ( TargetInfo(..) )
