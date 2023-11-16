{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module: Distribution.Simple.SetupHooks.Internal

Internal implementation module.
Users of @build-type: Hooks@ should import "Distribution.Simple.SetupHooks"
instead.
-}
module Distribution.Simple.SetupHooks.Internal
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


    -- * Internals
    -- ** Per-component hook utilities
  , applyComponentDiffs, forComponents_

    -- ** HookedBuildInfo compatibility code
  , hookedBuildInfoComponents, hookedBuildInfoComponentDiff_maybe

  )
where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens ((.~))
import Distribution.PackageDescription
import Distribution.Simple.Compiler ( Compiler(..) )
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import Distribution.Simple.Setup
  ( ReplFlags(..), HscolourFlags (..) )
import Distribution.Simple.Setup.Benchmark ( BenchmarkFlags(..) )
import Distribution.Simple.Setup.Build ( BuildFlags(..) )
import Distribution.Simple.Setup.Clean ( CleanFlags(..) )
import Distribution.Simple.Setup.Config ( ConfigFlags(..) )
import Distribution.Simple.Setup.Copy ( CopyFlags(..) )
import Distribution.Simple.Setup.Haddock ( HaddockFlags(..) )
import Distribution.Simple.Setup.Test ( TestFlags(..) )
import Distribution.Simple.Utils
import Distribution.System ( Platform(..) )
import Distribution.Types.Component ( Component(..), componentName )
import Distribution.Types.ComponentLocalBuildInfo ( ComponentLocalBuildInfo(..) )
import Distribution.Types.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.TargetInfo
import Distribution.Verbosity
import qualified Distribution.Types.BuildInfo.Lens as BI (buildInfo)

import Data.Coerce ( coerce )

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- SetupHooks

-- | Hooks into the @cabal@ build phases.
--
-- Usage:
--
--  - In your @.cabal@ file, declare @build-type: Hooks@.
--  - Provide a @SetupHooks.hs@ module next to your @.cabal@ file;
--    it must export @setupHooks :: SetupHooks@.
--  - In your @.cabal@ file, include a @custom-setup@ stanza
--    which declares the dependencies of your @SetupHooks@ module.
--
--
-- For example:
--
-- > -- In your .cabal file
-- > build-type: Hooks
-- >
-- > setup-depends:
-- >   base      >= 4.18 && < 5,
-- >   Cabal     >= 3.12 && < 4
--
-- > -- In SetupHooks.hs, next to your .cabal file
-- > module SetupHooks where
-- > import Distribution.Simple.SetupHooks ( SetupHooks, noSetupHooks )
-- >
-- > setupHooks :: SetupHooks
-- > setupHooks = noSetupHooks
data SetupHooks = SetupHooks
  { configureHooks :: ConfigureHooks
     -- ^ Hooks into the configure phase.
  , buildHooks     :: BuildHooks
     -- ^ Hooks into the build phase.
     --
     -- These hooks are relevant to any build-like phase,
     -- such as repl or haddock.
  , copyHooks      :: CopyHooks
     -- ^ Hooks into the copy/install phase.
  , cleanHooks     :: CleanHooks
     -- ^ Hooks into the clean phase.
  , testHooks      :: TestHooks
     -- ^ Hooks into the test phase.
  , benchmarkHooks :: BenchmarkHooks
     -- ^ Hooks into the benchmark phase.
  }

instance Semigroup SetupHooks where
  SetupHooks
    { configureHooks = conf1
    , buildHooks     = build1
    , copyHooks      = copy1
    , cleanHooks     = clean1
    , testHooks      = test1
    , benchmarkHooks = bench1 }
    <>
    SetupHooks
      { configureHooks = conf2
      , buildHooks     = build2
      , copyHooks      = copy2
      , cleanHooks     = clean2
      , testHooks      = test2
      , benchmarkHooks = bench2 }
    = SetupHooks
        { configureHooks = conf1 <> conf2
        , buildHooks     = build1 <> build2
        , copyHooks      = copy1 <> copy2
        , cleanHooks     = clean1 <> clean2
        , testHooks      = test1 <> test2
        , benchmarkHooks = bench1 <> bench2 }

instance Monoid SetupHooks where
  mempty = noSetupHooks

-- | Empty hooks.
noSetupHooks :: SetupHooks
noSetupHooks = SetupHooks
  { configureHooks = noConfigureHooks
  , buildHooks     = noBuildHooks
  , copyHooks      = noCopyHooks
  , cleanHooks     = noCleanHooks
  , testHooks      = noTestHooks
  , benchmarkHooks = noBenchmarkHooks
  }

--------------------------------------------------------------------------------
-- Configure hooks.

-- | Package-wide pre-configure step.
--
-- Perform side effects, and return a modification to the 'LocalBuildConfig'
-- passed in.
type PreConfPackageHook =
  ConfigFlags -> LocalBuildConfig -> Compiler -> Platform -> IO LocalBuildConfig

-- | Package-wide post-configure step.
--
-- Perform side effects. Last opportunity for any package-wide logic;
-- any subsequent hooks work per-component.
type PostConfPackageHook =
  LocalBuildConfig -> PackageBuildDescr -> IO ()

-- | Per-component pre-configure step.
--
-- For each component of the package, this hook can perform side effects,
-- and return a diff to the passed in component, e.g. to declare additional
-- autogenerated modules.
type PreConfComponentHook =
  LocalBuildConfig -> PackageBuildDescr -> Component -> IO ComponentDiff

-- | Per-component post-configure step.
--
-- Perform side effects for each component of the package.
type PostConfComponentHook =
  LocalBuildInfo -> Component -> IO ()

-- | Configure-time hooks.
--
-- Order of execution:
--
--  - 'preConfPackageHook',
--  - configure the package
--  - 'postConfPackageHook',
--  - 'preConfComponentHook',
--  - configure the components
--  - 'postConfComponentHook'.
data ConfigureHooks
  = ConfigureHooks
  { preConfPackageHook   :: Maybe PreConfPackageHook
     -- ^ Package-wide pre-configure hook. See 'PreConfPackageHook'.
  , postConfPackageHook  :: Maybe PostConfPackageHook
     -- ^ Package-wide post-configure hook. See 'PostConfPackageHook'.
  , preConfComponentHook :: Maybe PreConfComponentHook
     -- ^ Per-component pre-configure hook. See 'PreConfComponentHook'.
  , postConfComponentHook :: Maybe PostConfComponentHook
     -- ^ Per-component post-configure hook. See 'PostConfComponentHook'.
  }
-- SetupHooks TODO: we might want to change the type of per-component hooks
-- to be something like "Map ComponentName Hook".


instance Semigroup ConfigureHooks where
  ConfigureHooks
    { preConfPackageHook    = prePkg1
    , postConfPackageHook   = postPkg1
    , preConfComponentHook  = preComp1
    , postConfComponentHook = postComp1
    }
    <>
    ConfigureHooks
      { preConfPackageHook    = prePkg2
      , postConfPackageHook   = postPkg2
      , preConfComponentHook  = preComp2
      , postConfComponentHook = postComp2
      }
    = ConfigureHooks
        { preConfPackageHook    =
            coerce ((<>) @(Maybe PreConfPkgSemigroup))
              prePkg1 prePkg2
        , postConfPackageHook   = postPkg1 <> postPkg2
        , preConfComponentHook  = preComp1 <> preComp2
        , postConfComponentHook = postComp1 <> postComp2
        }

instance Monoid ConfigureHooks where
  mempty = noConfigureHooks

-- | Empty configure phase hooks.
noConfigureHooks :: ConfigureHooks
noConfigureHooks =
  ConfigureHooks
    { preConfPackageHook    = Nothing
    , postConfPackageHook   = Nothing
    , preConfComponentHook  = Nothing
    , postConfComponentHook = Nothing
    }

-- | A newtype to hang off the @Semigroup PreConfPackageHook@ instance.
newtype PreConfPkgSemigroup = PreConfPkgSemigroup PreConfPackageHook
instance Semigroup PreConfPkgSemigroup where
  PreConfPkgSemigroup f1 <> PreConfPkgSemigroup f2
    = PreConfPkgSemigroup $
        \ cfg lbi1 comp plat ->
          do { lbi2 <- f1 cfg lbi1 comp plat
             ; f2 cfg lbi2 comp plat }

--------------------------------------------------------------------------------
-- Build setup hooks.

-- | What kind of build phase are we hooking into?
--
-- Is this a normal build, or is it perhaps for running an interactive
-- session or Haddock?
data BuildingWhat
  -- | A normal build.
  = BuildNormal   BuildFlags
  -- | Build steps for an interactive session.
  | BuildRepl     ReplFlags
  -- | Build steps for generating documentation.
  | BuildHaddock  HaddockFlags
  -- | Build steps for Hscolour.
  | BuildHscolour HscolourFlags

buildingWhatVerbosity :: BuildingWhat -> Verbosity
buildingWhatVerbosity = \case
  BuildNormal   flags -> fromFlag $ buildVerbosity    flags
  BuildRepl     flags -> fromFlag $ replVerbosity     flags
  BuildHaddock  flags -> fromFlag $ haddockVerbosity  flags
  BuildHscolour flags -> fromFlag $ hscolourVerbosity flags

buildingWhatDistPref :: BuildingWhat -> FilePath
buildingWhatDistPref = \case
  BuildNormal   flags -> fromFlag $ buildDistPref    flags
  BuildRepl     flags -> fromFlag $ replDistPref     flags
  BuildHaddock  flags -> fromFlag $ haddockDistPref  flags
  BuildHscolour flags -> fromFlag $ hscolourDistPref flags

-- | A per-component build-time hook,
-- which can only perform side effects (e.g. creating files).
--
-- Will run in the following build-like phases:
--
--  - build
--  - haddock
--  - repl
--  - hscolour
type BuildComponentHook
  =  BuildingWhat -- ^ what kind of build phase are we hooking into?
  -> LocalBuildInfo -- ^ information about the package
  -> TargetInfo -- ^ information about an individual component
  -> IO ()

-- | Build-time hooks.
data BuildHooks
  = BuildHooks
  { preBuildComponentHook  :: Maybe BuildComponentHook
     -- ^ Per-component pre-build hook.
  , postBuildComponentHook :: Maybe BuildComponentHook
     -- ^ Per-component post-build hook.
  }

instance Semigroup BuildHooks where
  BuildHooks
    { preBuildComponentHook  = pre1
    , postBuildComponentHook = post1
    }
    <>
    BuildHooks
      { preBuildComponentHook  = pre2
      , postBuildComponentHook = post2
      }
    = BuildHooks
        { preBuildComponentHook  = pre1 <> pre2
        , postBuildComponentHook = post1 <> post2
        }

instance Monoid BuildHooks where
  mempty = noBuildHooks

-- | Empty build hooks.
noBuildHooks :: BuildHooks
noBuildHooks =
  BuildHooks
    { preBuildComponentHook  = Nothing
    , postBuildComponentHook = Nothing
    }

-- SetupHooks TODO: introduce some kind of recompilation logic
-- that allows the hook to declare a list of dependencies which is then
-- used in Cabal to decide when it needs to re-run the build hooks.
--
-- To do this, we should look at how cabal handles recompilation logic.

--------------------------------------------------------------------------------
-- Copy setup hooks.

-- | A per-component copy hook,
-- which can only perform side effects (e.g. copying files).
type CopyComponentHook
  =  LocalBuildInfo -- ^ information about the package
  -> CopyFlags
  -> TargetInfo -- ^ information about an individual component
  -> IO ()

-- | Copy (and install) hooks.
data CopyHooks
  = CopyHooks
  { preCopyComponentHook  :: Maybe CopyComponentHook
     -- ^ Per-component pre-copy hook.
  , postCopyComponentHook :: Maybe CopyComponentHook
     -- ^ Per-component post-copy hook.
  }

instance Semigroup CopyHooks where
  CopyHooks
    { preCopyComponentHook  = pre1
    , postCopyComponentHook = post1
    }
    <>
    CopyHooks
      { preCopyComponentHook  = pre2
      , postCopyComponentHook = post2
      }
    = CopyHooks
        { preCopyComponentHook  = pre1 <> pre2
        , postCopyComponentHook = post1 <> post2
        }

instance Monoid CopyHooks where
  mempty = noCopyHooks

-- | Empty copy/install hooks.
noCopyHooks :: CopyHooks
noCopyHooks =
  CopyHooks
    { preCopyComponentHook  = Nothing
    , postCopyComponentHook = Nothing
    }

--------------------------------------------------------------------------------
-- Clean setup hooks.

-- | A package-wide clean hook,
-- which can only perform side effects (e.g. removing files).
type CleanPackageHook
  =  PackageDescription -- ^ information about the package
  -> CleanFlags
  -> IO ()

-- | Clean hooks.
data CleanHooks
  = CleanHooks
  { cleanPackageHook   :: Maybe CleanPackageHook
     -- ^ Package-wide clean hook.
  }

instance Semigroup CleanHooks where
  CleanHooks
    { cleanPackageHook = pkg1 }
    <>
    CleanHooks
      { cleanPackageHook = pkg2 }
    = CleanHooks
        { cleanPackageHook = pkg1 <> pkg2 }

instance Monoid CleanHooks where
  mempty = noCleanHooks

-- | Empty clean hooks.
noCleanHooks :: CleanHooks
noCleanHooks =
  CleanHooks
    { cleanPackageHook = Nothing
    }

--------------------------------------------------------------------------------
-- Testsuite setup hooks.

-- | A per-package testing hook,
-- which can only perform side effects (e.g. creating files).
type TestPackageHook
  =  [String]       -- ^ additional test command-line arguments
  -> LocalBuildInfo -- ^ information about the package
  -> TestFlags
  -> IO ()

-- | A per-component testing hook,
-- which can only perform side effects (e.g. creating files).
type TestComponentHook
  =  [String]       -- ^ additional test command-line arguments
  -> LocalBuildInfo -- ^ information about the package
  -> TestFlags
  -> TestSuite      -- ^ test-suite being run
  -> ComponentLocalBuildInfo
       -- ^ local build information for the test-suite being run
  -> IO ()

-- | Testing hooks.
--
-- Order of execution:
--
--  - 'preTestPackageHook',
--  - 'preTestComponentHook',
--  - running the test suites,
--  - 'postTestComponentHook'
--  - 'postTestPackageHook',
data TestHooks
  = TestHooks
  { preTestPackageHook    :: Maybe TestPackageHook
     -- ^ Package-wide pre-test hook.
  , preTestComponentHook  :: Maybe TestComponentHook
     -- ^ Per-component pre-test hook.
  , postTestComponentHook :: Maybe TestComponentHook
     -- ^ Per-component post-test hook.
  , postTestPackageHook   :: Maybe TestPackageHook
     -- ^ Package-wide post-test hook.
  }

instance Semigroup TestHooks where
  TestHooks
    { preTestPackageHook    = prePkg1
    , preTestComponentHook  = preComp1
    , postTestComponentHook = postComp1
    , postTestPackageHook   = postPkg1
    }
    <>
    TestHooks
      { preTestPackageHook    = prePkg2
      , preTestComponentHook  = preComp2
      , postTestComponentHook = postComp2
      , postTestPackageHook   = postPkg2
      }
    = TestHooks
        { preTestPackageHook    = prePkg1 <> prePkg2
        , preTestComponentHook  = preComp1 <> preComp2
        , postTestComponentHook = postComp1 <> postComp2
        , postTestPackageHook   = postPkg1 <> postPkg2
        }

instance Monoid TestHooks where
  mempty = noTestHooks

-- | Empty test hooks.
noTestHooks :: TestHooks
noTestHooks =
  TestHooks
    { preTestPackageHook    = Nothing
    , preTestComponentHook  = Nothing
    , postTestComponentHook = Nothing
    , postTestPackageHook   = Nothing
    }

--------------------------------------------------------------------------------
-- Benchmark setup hooks.

-- | A per-package benchmarking hook,
-- which can only perform side effects (e.g. creating files).
type BenchmarkPackageHook
  =  [String]       -- ^ additional test command-line arguments
  -> LocalBuildInfo -- ^ information about the package
  -> BenchmarkFlags
  -> IO ()

-- | A per-component benchmarking hook,
-- which can only perform side effects (e.g. creating files).
type BenchmarkComponentHook
  =  [String]       -- ^ additional test command-line arguments
  -> LocalBuildInfo -- ^ information about the package
  -> BenchmarkFlags
  -> Benchmark      -- ^ benchmark being run
  -> ComponentLocalBuildInfo
       -- ^ local build information for the benchmark being run
  -> IO ()

-- | Benchmarking hooks.
--
-- Order of execution:
--
--  - 'preBenchPackageHook',
--  - 'preBenchComponentHook',
--  - running the benchmarks,
--  - 'postBenchComponentHook'
--  - 'postBenchPackageHook',
data BenchmarkHooks
  = BenchmarkHooks
  { preBenchPackageHook    :: Maybe BenchmarkPackageHook
     -- ^ Package-wide pre-benchmark hook.
  , preBenchComponentHook  :: Maybe BenchmarkComponentHook
     -- ^ Per-component pre-benchmark hook.
  , postBenchComponentHook :: Maybe BenchmarkComponentHook
     -- ^ Per-component post-benchmark hook.
  , postBenchPackageHook   :: Maybe BenchmarkPackageHook
     -- ^ Package-wide post-benchmark hook.
  }

instance Semigroup BenchmarkHooks where
  BenchmarkHooks
    { preBenchPackageHook    = prePkg1
    , preBenchComponentHook  = preComp1
    , postBenchComponentHook = postComp1
    , postBenchPackageHook   = postPkg1
    }
    <>
    BenchmarkHooks
      { preBenchPackageHook    = prePkg2
      , preBenchComponentHook  = preComp2
      , postBenchComponentHook = postComp2
      , postBenchPackageHook   = postPkg2
      }
    = BenchmarkHooks
        { preBenchPackageHook    = prePkg1 <> prePkg2
        , preBenchComponentHook  = preComp1 <> preComp2
        , postBenchComponentHook = postComp1 <> postComp2
        , postBenchPackageHook   = postPkg1 <> postPkg2
        }

instance Monoid BenchmarkHooks where
  mempty = noBenchmarkHooks

-- | Empty benchmark hooks.
noBenchmarkHooks :: BenchmarkHooks
noBenchmarkHooks =
  BenchmarkHooks
    { preBenchPackageHook    = Nothing
    , preBenchComponentHook  = Nothing
    , postBenchComponentHook = Nothing
    , postBenchPackageHook   = Nothing
    }

--------------------------------------------------------------------------------
-- Per-component configure hook implementation details.

type LibraryDiff    = Library
type ForeignLibDiff = ForeignLib
type ExecutableDiff = Executable
type TestSuiteDiff  = TestSuite
type BenchmarkDiff  = Benchmark
type BuildInfoDiff  = BuildInfo

-- | A diff to a Cabal 'Component', that gets combined monoidally into
-- an existing 'Component'.
newtype ComponentDiff = ComponentDiff { componentDiff :: Component }
  deriving Semigroup

emptyComponentDiff :: ComponentName -> ComponentDiff
emptyComponentDiff name = ComponentDiff $
  case name of
    CLibName   {} -> CLib   emptyLibrary
    CFLibName  {} -> CFLib  emptyForeignLib
    CExeName   {} -> CExe   emptyExecutable
    CTestName  {} -> CTest  emptyTestSuite
    CBenchName {} -> CBench emptyBenchmark

buildInfoComponentDiff :: ComponentName -> BuildInfo -> ComponentDiff
buildInfoComponentDiff name bi = ComponentDiff $ BI.buildInfo .~ bi $
  case name of
    CLibName   {} -> CLib   emptyLibrary
    CFLibName  {} -> CFLib  emptyForeignLib
    CExeName   {} -> CExe   emptyExecutable
    CTestName  {} -> CTest  emptyTestSuite
    CBenchName {} -> CBench emptyBenchmark

applyLibraryDiff :: Verbosity -> Library -> LibraryDiff -> IO Library
applyLibraryDiff verbosity lib diff =
  case illegalLibraryDiffReasons lib diff of
    [] -> return $ lib <> diff
    (r:rs) -> dieWithException verbosity
            $ SetupHooksException
            $ CannotApplyComponentDiff
            $ IllegalComponentDiff (r NE.:| rs)

illegalLibraryDiffReasons :: Library -> LibraryDiff -> [IllegalComponentDiffReason]
illegalLibraryDiffReasons lib
  Library
    { libName = nm
    , libExposed = e
    , libVisibility = vis
    , libBuildInfo = bi
    } =  [ CannotChangeName
         | not $ nm == libName emptyLibrary || nm == libName lib ]
      ++ [ CannotChangeComponentField "libExposed"
         | not $ e == libExposed emptyLibrary || e == libExposed lib ]
      ++ [ CannotChangeComponentField "libVisibility"
         | not $ vis == libVisibility emptyLibrary || vis == libVisibility lib ]
      ++ illegalBuildInfoDiffReasons (libBuildInfo lib) bi

applyForeignLibDiff :: Verbosity -> ForeignLib -> ForeignLibDiff -> IO ForeignLib
applyForeignLibDiff verbosity flib diff =
  case illegalForeignLibDiffReasons flib diff of
    [] -> return $ flib <> diff
    (r:rs) -> dieWithException verbosity
            $ SetupHooksException
            $ CannotApplyComponentDiff
            $ IllegalComponentDiff (r NE.:| rs)

illegalForeignLibDiffReasons :: ForeignLib -> ForeignLibDiff -> [IllegalComponentDiffReason]
illegalForeignLibDiffReasons flib
  ForeignLib
    { foreignLibName = nm
    , foreignLibType = ty
    , foreignLibOptions = opts
    , foreignLibVersionInfo = vi
    , foreignLibVersionLinux = linux
    , foreignLibModDefFile = defs
    , foreignLibBuildInfo = bi
    } =  [ CannotChangeName
         | not $ nm == foreignLibName emptyForeignLib || nm == foreignLibName flib ]
      ++ [ CannotChangeComponentField "foreignLibType"
         | not $ ty == foreignLibType emptyForeignLib || ty == foreignLibType flib ]
      ++ [ CannotChangeComponentField "foreignLibOptions"
         | not $ opts == foreignLibOptions emptyForeignLib || opts == foreignLibOptions flib ]
      ++ [ CannotChangeComponentField "foreignLibVersionInfo"
         | not $ vi == foreignLibVersionInfo emptyForeignLib || vi == foreignLibVersionInfo flib ]
      ++ [ CannotChangeComponentField "foreignLibVersionLinux"
         | not $ linux == foreignLibVersionLinux emptyForeignLib || linux == foreignLibVersionLinux flib ]
      ++ [ CannotChangeComponentField "foreignLibModDefFile"
         | not $ defs == foreignLibModDefFile emptyForeignLib || defs == foreignLibModDefFile flib ]
      ++ illegalBuildInfoDiffReasons (foreignLibBuildInfo flib) bi

applyExecutableDiff :: Verbosity -> Executable -> ExecutableDiff -> IO Executable
applyExecutableDiff verbosity exe diff =
  case illegalExecutableDiffReasons exe diff of
    [] -> return $ exe <> diff
    (r:rs) -> dieWithException verbosity
            $ SetupHooksException
            $ CannotApplyComponentDiff
            $ IllegalComponentDiff (r NE.:| rs)

illegalExecutableDiffReasons :: Executable -> ExecutableDiff -> [IllegalComponentDiffReason]
illegalExecutableDiffReasons exe
  Executable
    { exeName = nm
    , modulePath = path
    , exeScope = scope
    , buildInfo = bi
    } =  [ CannotChangeName
         | not $ nm == exeName emptyExecutable || nm == exeName exe ]
      ++ [ CannotChangeComponentField "modulePath"
         | not $ path == modulePath emptyExecutable || path == modulePath exe ]
      ++ [ CannotChangeComponentField "exeScope"
         | not $ scope == exeScope emptyExecutable || scope == exeScope exe ]
      ++ illegalBuildInfoDiffReasons (buildInfo exe) bi

applyTestSuiteDiff :: Verbosity -> TestSuite -> TestSuiteDiff -> IO TestSuite
applyTestSuiteDiff verbosity test diff =
  case illegalTestSuiteDiffReasons test diff of
    [] -> return $ test <> diff
    (r:rs) -> dieWithException verbosity
            $ SetupHooksException
            $ CannotApplyComponentDiff
            $ IllegalComponentDiff (r NE.:| rs)

illegalTestSuiteDiffReasons :: TestSuite -> TestSuiteDiff -> [IllegalComponentDiffReason]
illegalTestSuiteDiffReasons test
  TestSuite
    { testName = nm
    , testInterface = iface
    , testCodeGenerators = gens
    , testBuildInfo = bi
    } =  [ CannotChangeName
         | not $ nm == testName emptyTestSuite || nm == testName test ]
      ++ [ CannotChangeComponentField "testInterface"
         | not $ iface == testInterface emptyTestSuite || iface == testInterface test ]
      ++ [ CannotChangeComponentField "testCodeGenerators"
         | not $ gens == testCodeGenerators emptyTestSuite || gens == testCodeGenerators test ]
      ++ illegalBuildInfoDiffReasons (testBuildInfo test) bi

applyBenchmarkDiff :: Verbosity -> Benchmark -> BenchmarkDiff -> IO Benchmark
applyBenchmarkDiff verbosity bench diff =
  case illegalBenchmarkDiffReasons bench diff of
    [] -> return $ bench <> diff
    (r:rs) -> dieWithException verbosity
            $ SetupHooksException
            $ CannotApplyComponentDiff
            $ IllegalComponentDiff (r NE.:| rs)

illegalBenchmarkDiffReasons :: Benchmark -> BenchmarkDiff -> [IllegalComponentDiffReason]
illegalBenchmarkDiffReasons bench
  Benchmark
    { benchmarkName = nm
    , benchmarkInterface = iface
    , benchmarkBuildInfo = bi
    } =  [ CannotChangeName
         | not $ nm == benchmarkName emptyBenchmark || nm == benchmarkName bench ]
      ++ [ CannotChangeComponentField "benchmarkInterface"
         | not $ iface == benchmarkInterface emptyBenchmark || iface == benchmarkInterface bench ]
      ++ illegalBuildInfoDiffReasons (benchmarkBuildInfo bench) bi

illegalBuildInfoDiffReasons :: BuildInfo -> BuildInfoDiff -> [IllegalComponentDiffReason]
illegalBuildInfoDiffReasons bi
  BuildInfo
    { buildable = can_build
    , buildTools = build_tools
    , buildToolDepends = build_tools_depends
    , pkgconfigDepends = pkgconfig_depends
    , frameworks = fworks
    , targetBuildDepends = target_build_depends
    } = map CannotChangeBuildInfoField
      $  [ "buildable"
         | not $ can_build == buildable bi || can_build == buildable emptyBuildInfo ]
      ++ [ "buildTools"
         | not $ build_tools == buildTools bi || build_tools == buildTools emptyBuildInfo ]
      ++ [ "buildToolsDepends"
         | not $ build_tools_depends == buildToolDepends bi || build_tools_depends == buildToolDepends emptyBuildInfo ]
      ++ [ "pkgconfigDepends"
         | not $ pkgconfig_depends == pkgconfigDepends bi || pkgconfig_depends == pkgconfigDepends emptyBuildInfo ]
      ++ [ "frameworks"
         | not $ fworks == frameworks bi || fworks == frameworks emptyBuildInfo ]
      ++ [ "targetBuildDepends"
         | not $ target_build_depends == targetBuildDepends bi || target_build_depends == targetBuildDepends emptyBuildInfo ]

-- | Traverse the components of a 'PackageDescription'.
--
-- The function must preserve the component type, i.e. map a 'CLib' to a 'CLib',
-- a 'CExe' to a 'CExe', etc.
traverseComponents :: Applicative m
                   => (Component -> m Component)
                   -> PackageDescription -> m PackageDescription
traverseComponents f pd =
  upd_pd <$> traverse f_lib   (library      pd)
         <*> traverse f_lib   (subLibraries pd)
         <*> traverse f_flib  (foreignLibs  pd)
         <*> traverse f_exe   (executables  pd)
         <*> traverse f_test  (testSuites   pd)
         <*> traverse f_bench (benchmarks   pd)
  where
    f_lib   lib   = \case { CLib   lib'   -> lib'  ; c -> mismatch (CLib   lib)   c } <$> f (CLib   lib)
    f_flib  flib  = \case { CFLib  flib'  -> flib' ; c -> mismatch (CFLib  flib)  c } <$> f (CFLib  flib)
    f_exe   exe   = \case { CExe   exe'   -> exe'  ; c -> mismatch (CExe   exe)   c } <$> f (CExe   exe)
    f_test  test  = \case { CTest  test'  -> test' ; c -> mismatch (CTest  test)  c } <$> f (CTest  test)
    f_bench bench = \case { CBench bench' -> bench'; c -> mismatch (CBench bench) c } <$> f (CBench bench)

    upd_pd lib sublibs flibs exes tests benchs =
      pd { library      = lib
         , subLibraries = sublibs
         , foreignLibs  = flibs
         , executables  = exes
         , testSuites   = tests
         , benchmarks   = benchs }

    -- This is a panic, because we maintain this invariant elsewhere:
    -- see 'componentDiffError' in 'applyComponentDiff', which catches an
    -- invalid per-component configure hook.
    mismatch c1 c2 =
      error $ "Mismatched component types: " ++ showComponentName (componentName c1)
                                      ++ " " ++ showComponentName (componentName c2) ++ "."
{-# INLINEABLE traverseComponents #-}

applyComponentDiffs :: Verbosity -> (Component -> IO ComponentDiff) -> PackageDescription -> IO PackageDescription
applyComponentDiffs verbosity f = traverseComponents apply_diff
  where
    apply_diff :: Component -> IO Component
    apply_diff c = do { diff <- f c
                      ; applyComponentDiff verbosity c diff }

forComponents_ :: (Component -> IO ()) -> PackageDescription -> IO ()
forComponents_ f pd = getConst $ traverseComponents (Const . f) pd

applyComponentDiff :: Verbosity
                   -> Component
                   -> ComponentDiff
                   -> IO Component
applyComponentDiff verbosity comp (ComponentDiff diff)
  | CLib lib <- comp
  , CLib lib_diff <- diff
  = CLib <$> applyLibraryDiff verbosity lib lib_diff
  | CFLib flib <- comp
  , CFLib flib_diff <- diff
  = CFLib <$> applyForeignLibDiff verbosity flib flib_diff
  | CExe exe <- comp
  , CExe exe_diff <- diff
  = CExe <$> applyExecutableDiff verbosity exe exe_diff
  | CTest test <- comp
  , CTest test_diff <- diff
  = CTest <$> applyTestSuiteDiff verbosity test test_diff
  | CBench bench <- comp
  , CBench bench_diff <- diff
  = CBench <$> applyBenchmarkDiff verbosity bench bench_diff
  | otherwise
  = componentDiffError $ MismatchedComponentTypes comp diff
  where
    -- The per-component configure hook specified a diff of the wrong type,
    -- e.g. tried to apply an executable diff to a library.
    componentDiffError err = dieWithException verbosity
                           $ SetupHooksException
                           $ CannotApplyComponentDiff err

--------------------------------------------------------------------------------
-- Compatibility with HookedBuildInfo.
--
-- NB: assumes that the components in HookedBuildInfo are:
--  - an (optional) main library,
--  - executables.
--
-- No support for named sublibraries, foreign libraries, tests or benchmarks,
-- because the HookedBuildInfo datatype doesn't specify what type of component
-- each component name is (so we assume they are executables).

hookedBuildInfoComponents :: HookedBuildInfo -> Set ComponentName
hookedBuildInfoComponents (mb_mainlib, exes)
  = Set.fromList $
    (case mb_mainlib of { Nothing -> id; Just {} -> (CLibName LMainLibName :) })
    [CExeName exe_nm | (exe_nm, _) <- exes]

hookedBuildInfoComponentDiff_maybe :: HookedBuildInfo -> ComponentName -> Maybe (IO ComponentDiff)
hookedBuildInfoComponentDiff_maybe (mb_mainlib, exes) comp_nm =
  case comp_nm of
    CLibName lib_nm ->
      case lib_nm of
        LMainLibName   -> return . ComponentDiff . CLib . buildInfoLibraryDiff <$> mb_mainlib
        LSubLibName {} -> Nothing
    CExeName exe_nm ->
      let mb_exe = lookup exe_nm exes
      in return . ComponentDiff . CExe . buildInfoExecutableDiff <$> mb_exe
    CFLibName {} -> Nothing
    CTestName {} -> Nothing
    CBenchName {} -> Nothing

buildInfoLibraryDiff :: BuildInfo -> LibraryDiff
buildInfoLibraryDiff bi = emptyLibrary { libBuildInfo = bi }

buildInfoExecutableDiff :: BuildInfo -> ExecutableDiff
buildInfoExecutableDiff bi = emptyExecutable { buildInfo = bi }

--------------------------------------------------------------------------------
