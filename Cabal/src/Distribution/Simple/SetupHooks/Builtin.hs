{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Distribution.Simple.SetupHooks.Builtin where

import Control.Monad
import Control.Monad.IO.Class
import Distribution.Simple.Flag
import Distribution.Simple.Program.GHC
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.Utils

import Distribution.Types.Component
import Distribution.Types.BuildInfo
import Distribution.Types.TargetInfo
import Distribution.Simple.Program.Builtin (ghcProgram)

import Distribution.Simple.SetupHooks.Internal hiding (compiler)
import Distribution.Simple.SetupHooks.Rule as Rule
import Distribution.Simple.GHC.Build
import System.FilePath
import Distribution.Simple.LocalBuildInfo
import qualified Data.List.NonEmpty as NE
import Distribution.Simple.Program (requireProgram)
import Distribution.Verbosity (Verbosity)
import Distribution.Types.ComponentName (componentNameRaw)

-- ROMES:TODO: We also need to add the sources to the autogen modules s.t. they
-- are demanded... but can I add a .o file to the autogen modules list?
builtinBuildHooks :: BuildHooks
builtinBuildHooks = noBuildHooks
  { preBuildComponentRules = Just $ mconcat
      [ buildCSources
      , buildCxxSources
      , buildJsSources
      , buildAsmSources
      , buildCmmSources
      ] }

-- ROMES:TODO:
-- unless (not hasJsSupport || null jsSrcs) $ ... and (not has_code)
-- where has_code = not (componentIsIndefinite clbi)

-- ROMES:PATCH:NOTE: Worry about mimicking the current behaviour first, and only
-- later worry about dependency tracking and ghc -M, gcc -M, or ghc -optc-MD ...

-- ROMES:TODO:
-- How should we handle a C source depending on a stub generated from a foreign export?

buildCSources, buildCxxSources, buildJsSources
  , buildAsmSources, buildCmmSources :: PreBuildComponentRules
-- An executable main file may be a Cxx or C file. We consider the main file a
-- Cxx source if it is a .cpp/.cxx/.c++ file, and consider it a C source otherwise.
buildCSources   = buildExtraSources Internal.componentCcGhcOptions  True  (\f -> not (isHaskell f || isCxx f)) cSources
buildCxxSources = buildExtraSources Internal.componentCxxGhcOptions True  isCxx         cxxSources
buildJsSources  = buildExtraSources Internal.componentJsGhcOptions  False (const False) jsSources
buildAsmSources = buildExtraSources Internal.componentAsmGhcOptions True  (const False) asmSources
buildCmmSources = buildExtraSources Internal.componentCmmGhcOptions True  (const False) cmmSources

-- | Create 'PreBuildComponentRules' for a given type of extra build sources
-- which are compiled via a GHC invocation with the given options. Used to
-- define built-in extra sources, such as, C, Cxx, Js, Asm, and Cmm sources.
buildExtraSources :: (Verbosity -> LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo -> FilePath -> FilePath -> GhcOptions)
                  -- ^ Function to determine the @'GhcOptions'@ for the
                  -- invocation of GHC when compiling these extra sources (e.g.
                  -- @'Internal.componentCxxGhcOptions'@,
                  -- @'Internal.componentCmmGhcOptions'@)
                  -> Bool
                  -- ^ Want dynamic?
                  -> (FilePath -> Bool)
                  -- ^ Whether to build the given executable entry point (main
                  -- source file) together with the extra sources with the same
                  -- rule action. For non-@'Executable'@ components this
                  -- function is not even called, and is typically @const
                  -- False@. However, if the main file is a C source, we will
                  -- want to create a rule for it as we do for other C sources.
                  -> (BuildInfo -> [FilePath])
                  -- ^ View the extra sources from the build info (e.g. @'asmSources'@, @'cSources'@)
                  -> PreBuildComponentRules
buildExtraSources componentSourceGhcOptions wantDyn mainSourceToo viewSources = rules $ \PreBuildComponentInputs{buildingWhat, localBuildInfo=lbi, targetInfo} -> do
  let bi = componentBuildInfo (targetComponent targetInfo)
      verbosity = buildingWhatVerbosity buildingWhat
      clbi = targetCLBI targetInfo

      comp = compiler lbi
      platform = hostPlatform lbi
      isGhcDynamic = isDynamic comp
      doingTH = usesTemplateHaskellOrQQ bi
      forceSharedLib = doingTH && isGhcDynamic

  buildAction <- registerAction $ simpleAction $ curry $ \case
    ([(_,sourceFile)], ((cbuildDir, _ofile) NE.:| [])) -> do
      (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
      let runGhcProg = runGHC verbosity ghcProg comp platform

      let baseSrcOpts =
            componentSourceGhcOptions
              verbosity
              lbi
              bi
              clbi
              cbuildDir
              sourceFile
          vanillaSrcOpts
            -- Dynamic GHC requires C sources to be built
            -- with -fPIC for REPL to work. See #2207.
            | isGhcDynamic && wantDyn = baseSrcOpts{ghcOptFPic = toFlag True}
            | otherwise = baseSrcOpts
          profSrcOpts =
            vanillaSrcOpts
              `mappend` mempty
                { ghcOptProfilingMode = toFlag True
                }
          sharedSrcOpts =
            vanillaSrcOpts
              `mappend` mempty
                { ghcOptFPic = toFlag True
                , ghcOptDynLinkMode = toFlag GhcDynamicOnly
                }
          -- TODO: Placing all Haskell, C, & C++ objects in a single directory
          --       Has the potential for file collisions. In general we would
          --       consider this a user error. However, we should strive to
          --       add a warning if this occurs.
          odir = fromFlag (ghcOptObjDir vanillaSrcOpts)
          compileIfNeeded opts = do
            needsRecomp <- checkNeedsRecompilation sourceFile opts
            when needsRecomp $ runGhcProg opts

      createDirectoryIfMissingVerbose verbosity True odir
      case targetComponent targetInfo of
        -- For libraries, we compile extra objects in the three ways: vanilla, shared, and profiled.
        -- We suffix shared objects with .dyn_o and profiled ones with .p_o.
        --
        -- ROMES:TODO: Should we use those suffixes for extra sources for
        -- executables too? We use those suffixes for haskell objects for
        -- executables ... (see gbuild)
        CLib _lib
          -- Unless for repl, in which case we only need the vanilla way
          | BuildRepl _ <- buildingWhat
          -> compileIfNeeded vanillaSrcOpts
          | otherwise
          -> do
          compileIfNeeded vanillaSrcOpts
          when (wantDyn && (forceSharedLib || withSharedLib lbi)) $
            compileIfNeeded sharedSrcOpts{ghcOptObjSuffix=toFlag"dyn_o"}
          when (withProfLib lbi) $
            compileIfNeeded profSrcOpts{ghcOptObjSuffix=toFlag"p_o"}

        -- For foreign libraries, we determine with which options to build the
        -- objects (vanilla vs shared vs profiled)
        CFLib flib
          | withProfExe lbi -- ROMES: hmm... doesn't sound right.
          -> compileIfNeeded profSrcOpts
          | flibIsDynamic flib
          -> compileIfNeeded sharedSrcOpts
          | otherwise
          -> compileIfNeeded vanillaSrcOpts

        -- For the remaining component types (Exec, Test, Bench), we also
        -- determine with which options to build the objects (vanilla vs shared vs
        -- profiled), but predicate is the same for the three kinds.
        _exeLike
          | withProfExe lbi
          -> compileIfNeeded profSrcOpts
          | withDynExe lbi
          -> compileIfNeeded sharedSrcOpts
          | otherwise
          -> compileIfNeeded vanillaSrcOpts
    _inouts -> error "buildExtraSources: unexpected build rule inputs and outputs"

  return $ do

    -- Until we get rid of the "exename-tmp" directory within the executable
    -- build dir, we need to accommodate that fact (see eg @tmpDir@ in @gbuild@)
    -- This is a workaround for #9498 until it is fixed.
    let cname = componentName (targetComponent targetInfo)
    let buildDir'
          | CLibName{} <- cname
          = componentBuildDir lbi clbi
          | CNotLibName{} <- cname
          = componentBuildDir lbi clbi </>
              componentNameRaw cname <> "-tmp"

    extraMainSource
      <- case targetComponent targetInfo of
           CExe exe -> liftIO $ do
             main <- findExecutableMain verbosity buildDir' exe
             if mainSourceToo main
                then return [main]
                else return []
           _other   -> pure []

    let sources = viewSources bi ++ extraMainSource

    -- build any sources
    unless (null sources) $ do
      liftIO $ do
        -- ROMES:TODO: Message custom for each build source type
        info verbosity "Determining build rules for extra sources..."
        print sources

      forM_ sources $ \source -> do

        registerRule $ simpleRule buildAction [("", source)] (NE.singleton (buildDir', source -<.> "o"))
        -- ROMES:TODO: Is source the path to the source from the .cabal root
        -- or something else? Document in SetupHooks haddocks (e.g. here we're
        -- using "")

