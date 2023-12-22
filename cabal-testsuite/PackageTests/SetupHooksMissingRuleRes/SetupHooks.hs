{-# LANGUAGE DuplicateRecordFields #-}

module SetupHooks where

import Distribution.Simple.SetupHooks

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just missingResRules
          }
    }

missingResRules :: Rules PreBuildComponentInputs
missingResRules = rules $ \ (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) -> do
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
  actId <- registerAction $ simpleAction $ \ _ _ -> return ()
  return $
    registerRule $
      simpleRule actId
        [ ]
        ( ( autogenDir, "G.hs" ) NE.:| [] )
