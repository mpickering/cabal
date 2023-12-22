{-# LANGUAGE DuplicateRecordFields #-}

module SetupHooks where

import Distribution.Simple.SetupHooks

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just cyclicPreBuildRules
          }
    }

cyclicPreBuildRules :: Rules PreBuildComponentInputs
cyclicPreBuildRules = rules $ \ (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) -> do
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
  actId <- registerAction $ simpleAction $ \ _ _ -> error "This should not run"
  return $ do
    registerRule $
      simpleRule actId
        [ ( autogenDir, "G1.hs") ]
        ( ( autogenDir, "G2.hs" ) NE.:| [] )
    registerRule $
      simpleRule actId
        [ ( autogenDir, "G2.hs" ) ]
        ( ( autogenDir, "G1.hs" ) NE.:| [] )
