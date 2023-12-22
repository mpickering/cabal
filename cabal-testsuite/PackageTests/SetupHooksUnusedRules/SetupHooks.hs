{-# LANGUAGE DuplicateRecordFields #-}

module SetupHooks where

import Distribution.Simple.SetupHooks

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just unusedPreBuildRules
          }
    }

unusedPreBuildRules :: Rules PreBuildComponentInputs
unusedPreBuildRules = rules $ \ (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) -> do
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
  actId <- registerAction $ simpleAction $ \ _ _ -> error "This should not run"
  return $ do
    registerRule $
      simpleRule actId []
        ( ( autogenDir, "X.hs" ) NE.:| [ ( autogenDir, "Y.hs" ) ] )
    registerRule $
      simpleRule actId []
        ( ( autogenDir, "Z.what" ) NE.:| [] )
