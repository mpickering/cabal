{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-- Module      :  Distribution.Simple.SetupHooks.Errors
-- Copyright   :
-- License     :
--
-- Maintainer  :
-- Portability :
--
-- Exceptions for the Hooks build-type.

module Distribution.Simple.SetupHooks.Errors
  ( SetupHooksException (..)
  , CannotApplyComponentDiffReason (..)
  , IllegalComponentDiffReason (..)
  , setupHooksExceptionCode
  , setupHooksExceptionMessage
  , showLocs
  ) where

import Distribution.PackageDescription
import Distribution.Simple.SetupHooks.Rule
import qualified Distribution.Simple.SetupHooks.Rule as Rule
import Distribution.Types.Component

import qualified Data.Graph as Graph
import qualified Data.List.NonEmpty as NE
import qualified Data.Tree as Tree

import System.FilePath (normalise, (</>))

--------------------------------------------------------------------------------

-- | An error involving the @SetupHooks@ module of a package with
-- Hooks build-type.
data SetupHooksException
  = -- | Cannot apply a diff to a component in a per-component configure hook.
    CannotApplyComponentDiff CannotApplyComponentDiffReason
  | -- | There are cycles in the dependency graph of fine-grained rules.
    CyclicRuleDependencies
      (NE.NonEmpty (Rule, NE.NonEmpty (Graph.Tree Rule)))
  | -- | When executing fine-grained rules compiled into the external hooks
    -- executable, we failed to find dependencies of a rule that we expected
    -- to have already been generated.
    CantFindSourceForRuleDependencies
      Rule
      (NE.NonEmpty Rule.Location)
      -- ^ missing dependencies
  | -- | When executing fine-grained rules compiled into the external hooks
    -- executable, a rule failed to generate the outputs it claimed it would.
    MissingRuleOutputs
      Rule
      (NE.NonEmpty Rule.Location)
      -- ^ missing outputs
  deriving (Show)

data CannotApplyComponentDiffReason
  = MismatchedComponentTypes Component Component
  | IllegalComponentDiff Component (NE.NonEmpty IllegalComponentDiffReason)
  deriving (Show)

data IllegalComponentDiffReason
  = CannotChangeName
  | CannotChangeComponentField String
  | CannotChangeBuildInfoField String
  deriving (Show)

setupHooksExceptionCode :: SetupHooksException -> Int
setupHooksExceptionCode = \case
  CannotApplyComponentDiff rea ->
    cannotApplyComponentDiffCode rea
  CyclicRuleDependencies{} -> 9077
  CantFindSourceForRuleDependencies{} -> 1071
  MissingRuleOutputs{} -> 3498

setupHooksExceptionMessage :: SetupHooksException -> String
setupHooksExceptionMessage = \case
  CannotApplyComponentDiff reason ->
    cannotApplyComponentDiffMessage reason
  CyclicRuleDependencies cycles ->
    unlines $
      ("Hooks: cycle" ++ plural ++ " in dependency structure of rules:")
        : map showCycle (NE.toList cycles)
    where
      plural :: String
      plural
        | NE.length cycles >= 2 =
            "s"
        | otherwise =
            ""
      showCycle :: (Rule, NE.NonEmpty (Graph.Tree Rule)) -> String
      showCycle (r, rs) =
        unlines . map ("  " ++) . lines $
          Tree.drawTree $
            fmap showRule $
              Tree.Node r (NE.toList rs)
      showRule :: Rule -> String
      showRule (Rule{dependencies = deps, results = reslts}) =
        "Rule: " ++ showLocs deps ++ " --> " ++ showLocs (NE.toList reslts)
  CantFindSourceForRuleDependencies _r deps ->
    unlines $
      ("Pre-build rules: can't find source for rule " ++ what ++ ":")
        : map (\d -> "  - " <> locPath d) depsL
    where
      depsL = NE.toList deps
      what
        | length depsL == 1 =
            "dependency"
        | otherwise =
            "dependencies"
  MissingRuleOutputs _r reslts ->
    unlines $
      ("Pre-build rule did not generate expected result" <> plural <> ":")
        : map (\res -> "  - " <> locPath res) resultsL
    where
      resultsL = NE.toList reslts
      plural
        | length resultsL == 1 =
            ""
        | otherwise =
            "s"

locPath :: Location -> String
locPath (base, fp) = normalise $ base </> fp

showLocs :: [Location] -> String
showLocs [] = "[]"
showLocs (x : xs) = '[' : ' ' : locPath x ++ showl xs
  where
    showl [] = " ]"
    showl (y : ys) = ',' : ' ' : locPath y ++ showl ys

cannotApplyComponentDiffCode :: CannotApplyComponentDiffReason -> Int
cannotApplyComponentDiffCode = \case
  MismatchedComponentTypes{} -> 9491
  IllegalComponentDiff{} -> 7634

cannotApplyComponentDiffMessage :: CannotApplyComponentDiffReason -> String
cannotApplyComponentDiffMessage = \case
  MismatchedComponentTypes comp diff ->
    unlines
      [ "Hooks: mismatched component types in per-component configure hook."
      , "Trying to apply " ++ what ++ " diff to " ++ to ++ "."
      ]
    where
      what = case diff of
        CLib{} -> "a library"
        CFLib{} -> "a foreign library"
        CExe{} -> "an executable"
        CTest{} -> "a testsuite"
        CBench{} -> "a benchmark"
      to = case componentName comp of
        nm@(CExeName{}) -> "an " ++ showComponentName nm
        nm -> "a " ++ showComponentName nm
  IllegalComponentDiff comp reasons ->
    unlines $
      ("Hooks: illegal component diff in per-component pre-configure hook for " ++ what ++ ":")
        : map mk_rea (NE.toList reasons)
    where
      mk_rea err = "  - " ++ illegalComponentDiffMessage err ++ "."
      what = case componentName comp of
        CLibName LMainLibName -> "main library"
        nm -> showComponentName nm

illegalComponentDiffMessage :: IllegalComponentDiffReason -> String
illegalComponentDiffMessage = \case
  CannotChangeName ->
    "cannot change the name of a component"
  CannotChangeComponentField fld ->
    "cannot change component field '" ++ fld ++ "'"
  CannotChangeBuildInfoField fld ->
    "cannot change BuildInfo field '" ++ fld ++ "'"
