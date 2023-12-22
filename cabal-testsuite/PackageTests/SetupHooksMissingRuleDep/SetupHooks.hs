{-# LANGUAGE DuplicateRecordFields #-}

module SetupHooks where

import Distribution.Simple.SetupHooks

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just missingDepRules
          }
    }

missingDepRules :: Rules PreBuildComponentInputs
missingDepRules = rules $ \ (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) -> do
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
  actId <- registerAction $ simpleAction $ \ _ _ -> error "This should not run"
  return $
    registerRule $
      simpleRule actId
        [ ( ".", "Missing.hs" ) ]
        ( ( autogenDir, "G.hs" ) NE.:| [] )
