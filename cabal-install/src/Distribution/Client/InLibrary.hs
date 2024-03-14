{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Client.InLibrary
  ( libraryConfigureInputsFromElabPackage
  , configure
  , build
  , haddock
  , copy
  , register
  , repl
  , test
  , bench
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad
import qualified Distribution.Client.SetupHooks.CallHooksExe as ExternalHooksExe
  ( buildTypeSetupHooks )
import Distribution.Client.Types

import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Build (build_setupHooks, repl_setupHooks)
import qualified Distribution.Simple.Configure as Cabal
import Distribution.Simple.LocalBuildInfo
  ( Component
  , componentName, mbWorkDirLBI
  )
import qualified Distribution.Simple.PreProcess as Cabal
import qualified Distribution.Simple.Program.Db as Cabal
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.Test as Cabal
import qualified Distribution.Simple.Bench as Cabal
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Haddock (haddock_setupHooks)
import Distribution.Simple.Install (install_setupHooks)
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.Utils
import Distribution.System (Platform)
import Distribution.Types.BuildType
import Distribution.Types.ComponentRequestedSpec
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.LocalBuildInfo
import Distribution.Simple ( Compiler, PackageDBStack )

import qualified Data.Set as Set
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Configure

data LibraryConfigureInputs
  = LibraryConfigureInputs
  { compiler :: Compiler
  , platform :: Platform
  , buildType :: BuildType
  , compRequested :: Maybe PD.ComponentName
  , localBuildConfig :: LBC.LocalBuildConfig
  , packageDBStack :: PackageDBStack
  , packageDescription :: PD.PackageDescription
  , flagAssignment :: PD.FlagAssignment
  }

libraryConfigureInputsFromElabPackage
  :: ElaboratedSharedConfig
  -> ElaboratedReadyPackage
  -> LibraryConfigureInputs
libraryConfigureInputsFromElabPackage
  ElaboratedSharedConfig
    { pkgConfigPlatform = platform
    , pkgConfigCompiler = compiler
    , pkgConfigCompilerProgs = progdb }
  (ReadyPackage pkg)
  = LibraryConfigureInputs
  { compiler
  , platform
  , buildType = PD.buildType pkgDescr
  , compRequested =
      case elabPkgOrComp pkg of
        ElabComponent elabComp
          | Just elabCompNm <- compComponentName elabComp ->
              Just elabCompNm
        _ -> Nothing
  , localBuildConfig =
    LBC.LocalBuildConfig
      { LBC.extraConfigArgs = []
      , LBC.withPrograms = progdb
      , LBC.withBuildOptions = elabBuildOptions pkg
      }
  , packageDBStack = elabBuildPackageDBStack pkg
  , packageDescription = pkgDescr
  , flagAssignment = elabFlagAssignment pkg
  }
  where
    pkgDescr = elabPkgDescription pkg

configure
  :: LibraryConfigureInputs
  -> Cabal.ConfigFlags
  -> IO LocalBuildInfo
configure
  LibraryConfigureInputs
    { platform, compiler
    , buildType = bt
    , compRequested = mbComp
    , localBuildConfig = lbc0
    , packageDBStack = packageDBs
    , packageDescription = pkgDesc
    , flagAssignment
    }
  cfg =
    -- TODO: the following code should not live in cabal-install.
    -- We should be able to directly call into the library,
    -- similar to what we do for other phases (see e.g. inLibraryBuild).
    --
    -- The issue is mainly about 'finalizeAndConfigurePackage' vs 'configurePackage'.
    do
      let verbosity = Cabal.fromFlag $ Cabal.configVerbosity cfg
          mbWorkDir = Cabal.flagToMaybe $ Cabal.configWorkingDir cfg
          distPref = Cabal.fromFlag $ Cabal.configDistPref cfg
          confHooks = configureHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt

      -- Configure package

      -- SetupHooks TODO: we should avoid re-doing package-wide things
      -- over and over in the per-component world, e.g.
      --   cabal build comp1 && cabal build comp2
      -- should only run the per-package configuration (including hooks) a single time.
      lbc1 <- case preConfPackageHook confHooks of
        Nothing -> return lbc0
        Just hk -> Cabal.runPreConfPackageHook cfg compiler platform lbc0 hk
      let compRequested = case mbComp of
            Just compName -> OneComponentRequestedSpec compName
            Nothing ->
              ComponentRequestedSpec
                { testsRequested = Cabal.fromFlag (Cabal.configTests cfg)
                , benchmarksRequested = Cabal.fromFlag (Cabal.configBenchmarks cfg)
                }
      (lbc2, pbd2) <-
        Cabal.configurePackage
          cfg
          lbc1
          pkgDesc
          flagAssignment
          compRequested
          compiler
          platform
          packageDBs
      for_ (postConfPackageHook confHooks) $ Cabal.runPostConfPackageHook lbc2 pbd2
      let pkg_descr2 = LBC.localPkgDescr pbd2

      -- Configure component(s)
      pkg_descr <-
        applyComponentDiffs
          verbosity
          ( \comp ->
              if wantComponent compRequested comp
                then traverse (Cabal.runPreConfComponentHook lbc2 pbd2 comp) $ preConfComponentHook confHooks
                else return Nothing
          )
          pkg_descr2
      let pbd3 = pbd2{LBC.localPkgDescr = pkg_descr}

      -- SetupHooks TODO: not calling "finalCheckPackage" from Cabal,
      -- as that works with a generic package description.
      -- Is this OK?

      -- SetupHooks TODO: the following is a huge amount of faff, just
      -- in order to call 'configureComponents'. Do we really need to
      -- do all of this? This complexity is bad, as it risks going out
      -- of sync with the implementation in Cabal.
      let progdb = LBC.withPrograms lbc2
          promisedDeps = Cabal.mkPromisedDepsSet (Cabal.configPromisedDependencies cfg)
      installedPkgs <- Cabal.getInstalledPackages verbosity compiler mbWorkDir packageDBs progdb
      (_, depsMap) <-
        either (dieWithException verbosity) return $
          Cabal.combinedConstraints
            (Cabal.configConstraints cfg)
            (Cabal.configDependencies cfg)
            installedPkgs
      let pkg_info =
            Cabal.PackageInfo
              { internalPackageSet = Set.fromList (map PD.libName (PD.allLibraries pkg_descr))
              , promisedDepsSet = promisedDeps
              , installedPackageSet = installedPkgs
              , requiredDepsMap = depsMap
              }
          useExternalInternalDeps = case compRequested of
            OneComponentRequestedSpec{} -> True
            ComponentRequestedSpec{} -> False
      externalPkgDeps <- Cabal.configureDependencies verbosity useExternalInternalDeps pkg_info pkg_descr compRequested
      lbi <- Cabal.configureComponents lbc2 pbd3 installedPkgs promisedDeps externalPkgDeps
      -- Write the LocalBuildInfo to disk. This is needed, for instance, if we
      -- skip re-configuring; we retrieve the LocalBuildInfo stored on disk from
      -- the previous invocation of 'configure' and pass it to 'build'.
      Cabal.writePersistBuildConfig mbWorkDir distPref lbi
      return lbi

wantComponent :: ComponentRequestedSpec -> Component -> Bool
wantComponent compReq comp = case compReq of
  ComponentRequestedSpec {} -> True
  OneComponentRequestedSpec reqComp ->
    componentName comp == reqComp
      -- NB: this might match multiple components,
      -- due to Backpack instantiations.

--------------------------------------------------------------------------------
-- Build

build
  :: Cabal.BuildFlags
  -> LocalBuildInfo
  -> IO [MonitorFilePath]
build flags lbi = do
  putStrLn $
    unlines
      [ "In library build"
      , "unconfigured progs: " ++ show (Cabal.unconfiguredProgs $ withPrograms lbi)
      , "configured progs: " ++ show (Map.keys $ Cabal.configuredProgs $ withPrograms lbi) ]
  build_setupHooks hooks pkgDescr lbi flags Cabal.knownSuffixHandlers
  where
    hooks = buildHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.buildDistPref flags

--------------------------------------------------------------------------------
-- Haddock

haddock
  :: Cabal.HaddockFlags
  -> LocalBuildInfo
  -> IO [MonitorFilePath]
haddock flags lbi =
  haddock_setupHooks hooks pkgDescr lbi Cabal.knownSuffixHandlers flags
  where
    hooks = buildHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.haddockDistPref flags

--------------------------------------------------------------------------------
-- Repl

repl
  :: Cabal.ReplFlags
  -> LocalBuildInfo
  -> IO ()
repl flags lbi =
  repl_setupHooks hooks pkgDescr lbi flags Cabal.knownSuffixHandlers []
  where
    hooks = buildHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.replDistPref flags

--------------------------------------------------------------------------------
-- Copy

copy
  :: Cabal.CopyFlags
  -> LocalBuildInfo
  -> IO ()
copy flags lbi =
  install_setupHooks hooks pkgDescr lbi flags
  where
    hooks = installHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.copyDistPref flags

--------------------------------------------------------------------------------
-- Test, bench, register.
--
-- NB: no hooks into these phases

test
  :: Cabal.TestFlags
  -> LocalBuildInfo
  -> IO ()
test flags lbi =
  Cabal.test [] pkgDescr lbi flags
  -- SetupHooksTODO: is args = [] OK here?
  where
    pkgDescr = localPkgDescr lbi

bench
  :: Cabal.BenchmarkFlags
  -> LocalBuildInfo
  -> IO ()
bench flags lbi =
  Cabal.bench [] pkgDescr lbi flags
  -- SetupHooksTODO: is args = [] OK here?
  where
    pkgDescr = localPkgDescr lbi

register
  :: Cabal.RegisterFlags
  -> LocalBuildInfo
  -> IO ()
register flags lbi = Cabal.register pkgDescr lbi flags
  where
    pkgDescr = localPkgDescr lbi
