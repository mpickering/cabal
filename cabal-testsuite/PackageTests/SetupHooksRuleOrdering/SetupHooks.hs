{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SetupHooks where

import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils ( rewriteFileEx, warn )

import Data.Foldable ( for_ )
import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )
import Data.Traversable ( for )

import System.FilePath
  ( (<.>), (</>) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just preBuildRules
          }
    }

data T a = T a a a
  deriving (Functor, Foldable, Traversable)

-- Register three rules:
--
-- r1: B --> C
-- r2: A --> B
-- r3: C --> D
--
-- and check that we run them in dependency order, i.e. r2, r1, r3.
preBuildRules :: Rules PreBuildComponentInputs
preBuildRules = rules $ \ (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) -> do
  let verbosity = buildingWhatVerbosity what
      clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
  actIds <-
    for (T ("B", "C") ("A", "B") ("C", "D")) $ \ (inMod, outMod) -> do
      actId <- registerAction $ simpleAction $ \ _ locs -> do
        warn verbosity $ "Running rule: " ++ inMod ++ " --> " ++ outMod
        let loc = autogenDir </> outMod <.> "hs"
        rewriteFileEx verbosity loc $
          "module " ++ outMod ++ " where { import " ++ inMod ++ " }"
      return (actId, inMod, outMod)
  return $ do
    for_ actIds $ \ (actId, inMod, outMod) -> do
      let inLoc = if inMod == "A"
                  then "."
                  else autogenDir
      registerRule $
        simpleRule actId
          [ (inLoc, inMod <.> "hs") ]
          ( ( autogenDir, outMod <.> "hs" ) NE.:| [] )
