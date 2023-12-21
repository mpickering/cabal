{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Distribution.Simple.SetupHooks.Rule
--
-- Internal module that defines fine-grained rules for setup hooks.
-- Users should import 'Distribution.Simple.SetupHooks' instead.
module Distribution.Simple.SetupHooks.Rule
  ( -- * Rules

    -- ** Rule
    Rule (..)
  , simpleRule

    -- * Action
  , Action (..)
  , simpleAction
  , ActionId (..)

    -- ** Collections of rules
  , Rules (..)
  , rules
  , noRules

    -- ** Rule inputs/outputs
  , Location

    -- ** Monitoring
  , MonitoredValue
  , MonitorFileOrDir (..)
  , MonitorKindFile (..)
  , MonitorKindDir (..)

    -- *** Monadic API for generation of 'ActionId'
  , FreshT (..)
  , runFreshT
  , RulesM
  , ActionsM
  , RulesT
  , computeRules
  )
where

import Distribution.Compat.Prelude

import Control.Monad.Fix
  ( MonadFix
  )
import Control.Monad.Trans
  ( MonadIO
  , MonadTrans
  )
import qualified Control.Monad.Trans.State as State
#if MIN_VERSION_transformers(0,5,6)
import qualified Control.Monad.Trans.Writer.CPS as Writer
#else
import qualified Control.Monad.Trans.Writer.Strict as Writer
#endif
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
  ( empty
  )

--------------------------------------------------------------------------------

{- Note [Fine-grained hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To best understand how the framework of fine-grained build rules
fits into Cabal and the greater Haskell ecosystem, it is helpful to think
that we want build tools (such as cabal-install or HLS) to be able to call
individual build rules on-demand, so that e.g. when a user modifies a .xyz file
the associated preprocessor is re-run.

To do this, we need to perform two different kinds of invocations:

  Query: query the package for the rules that it provides, with their
         dependency information. This allows one to determine when each
         rule should be rerun.

         (For example, if one rule preprocesses *.xyz into *.hs, we need to
         re-run the rule whenever *.xyz is modified.)

  Run: run the relevant action, once one has determined that the rule
       has gone stale.

To do this, any Cabal package with Hooks build-type provides a SetupHooks
module which supports these queries; for example it can be compiled into
a separate executable which can be invoked in the manner described above.
The function that implements this API in Cabal is
Distribution.Simple.UserHooks.hooksMain.
-}

-- | A unique identifier for an t'Action'.
newtype ActionId = ActionId Word64
  deriving (Show, Eq, Ord, Binary, Structured)

-- | A monitored value.
--
-- Use this to declare a dependency on a certain projection of the
-- environment passed in to a computation.
--
-- A computation is considered out-of-date when the environment passed to
-- it changes and this value also changes.
--
-- A value of 'Nothing' means: always consider the computation to be out-of-date.
type MonitoredValue = Maybe LBS.ByteString

-- | A monitored file or directory.
--
-- Use this to declare a dependency of the computation of rules. For example,
-- returning @[MonitorDir DirContents "abc"]@ will ensure that rules are
-- re-computed whenever the contents of directory @abc@ changes.
data MonitorFileOrDir
  = MonitorFile
      { monitorKindFile :: !MonitorKindFile
      , monitorPath :: !FilePath
      }
  | MonitorDir
      { monitorKindDir :: !MonitorKindDir
      , monitorDirLocation :: !FilePath
      }
  deriving (Generic, Eq, Show)

instance Binary MonitorFileOrDir
instance Structured MonitorFileOrDir

data MonitorKindFile
  = FileExists
  | FileModTime
  | FileHashed
  | FileNotExists
  deriving (Generic, Eq, Show)

instance Binary MonitorKindFile
instance Structured MonitorKindFile

data MonitorKindDir
  = DirExists
  | DirContents
  | DirNotExists
  deriving (Generic, Eq, Show)

instance Binary MonitorKindDir
instance Structured MonitorKindDir

-- | A rule consists of:
--
--  - an action to run, indirectly referenced by the 'ActionId' of
--    the 'Action'
--  - a description of the action:
--
--      - what inputs the action depends on,
--      - what outputs will result from the execution of the action.
--
-- The location of each dependency and of each output will get passed to the
-- action when executing the rule.
--
-- Use 'simpleRule' to construct a rule, overriding specific fields, rather
-- than directly using the 'Rule' constructor.
--
-- __Requirements:__ the t'Action' whose t'ActionId' is stored in the 'actionId'
-- field of a 'Rule' must satisfy the following:
--
--   - the t'Action' expects exactly as many elements in its first argument
--     as there are values stored in the 'dependencies' field of the 'Rule's;
--   - the t'Action' expects exactly as many elements in its second argument
--     as there are values stored in the 'results' field of the rule;
--     the execution of the action must produce results in these specified
--     locations.
data Rule
  = -- | Please use the 'simpleRule' smart constructor instead of
    -- this constructor, in order to avoid relying on internal implementation
    -- details.
    Rule
    { dependencies :: ![Location]
    -- ^ Dependencies of this rule.
    --
    -- When the build system executes the action associated to this rule,
    -- it will pass these locations as the first argument to the action.
    , results :: !(NE.NonEmpty Location)
    -- ^ Results of this rule; see t'Result'.
    , actionId :: !ActionId
    -- ^ To run this rule, which t'Action' should we execute?
    --
    -- The t'Action' will receive exactly as many elements in its first
    -- argument as there are 'dependencies' to this t'Rule', and exactly
    -- as many elements in its second argument as there are 'results'
    -- of this rule.
    , monitoredValue :: !MonitoredValue
    -- ^ A monitored value. The rule should be re-run whenever this value
    -- changes.
    --
    -- Use this to declare a dependency on a certain projection of the
    -- environment passed to the rule.
    --
    -- A value of 'Nothing' means: always re-run the rule when the
    -- environment passes to it changes.
    }
  deriving (Generic, Show)

-- | A simple rule, with no additional monitoring.
--
-- Prefer using this smart constructor instead of v'Rule' whenever possible.
simpleRule :: ActionId -> [Location] -> NE.NonEmpty Location -> Rule
simpleRule actId dep res =
  Rule
    { dependencies = dep
    , results = res
    , monitoredValue = Nothing
    , actionId = actId
    }

instance Binary Rule
instance Structured Rule

-- | A (fully resolved) location of a dependency or result of a rule,
-- consisting of an absolute path and of a file path relative to that base path.
--
-- In practice, this will be something like @( dir, toFilePath modName )@,
-- where:
--
--  - for a dependency, @dir@ is one of the Cabal search directories,
--  - for an output, @dir@ is a directory such as @autogenComponentModulesDir@
--    or @componentBuildDir@.
type Location = (FilePath, FilePath)

-- The reason for splitting it up this way is that some pre-processors don't
-- simply generate one output @.hs@ file from one input file, but have
-- dependencies on other generated files (notably @c2hs@, where building one
-- @.hs@ file may require reading other @.chi@ files, and then compiling the
-- @.hs@ file may require reading a generated @.h@ file).
-- In these cases, the generated files need to embed relative path names to each
-- other (eg the generated @.hs@ file mentions the @.h@ file in the FFI imports).
-- This path must be relative to the base directory where the generated files
-- are located; it cannot be relative to the top level of the build tree because
-- the compilers do not look for @.h@ files relative to there, ie we do not use
-- @-I .@, instead we use @-I dist/build@ (or whatever dist dir has been set
-- by the user).

-- | An action to run to execute a 'Rule', for example an invocation
-- of an external executable such as @happy@ or @alex@.
--
-- Use 'simpleAction' to construct an action, overriding specific fields,
-- rather than directly using the 'Action' constructor.
--
-- Arguments:
--
--  1. The locations of the __dependencies__ of this action,
--     as declared by the rule that this action is executing.
--     There will be as many values passed in this list as there are
--     'dependencies' values declared in the 'Rule' that calls the action.
--
--  2. The locations in which the __results__ of this action should be put.
--     There will be as many values passed in this list as there are
--     'results' values declared in the 'Rule' that calls the action.
newtype Action
  = -- | Please use the 'simpleAction' smart constructor instead of
    -- this constructor, in order to avoid relying on internal implementation
    -- details.
    Action
    { action
        :: [Location]
        -- \^ Locations of the __dependencies__ of this action,
        -- as declared by the rule that this action is executing.
        -> NE.NonEmpty Location
        -- \^ Locations in which the __results_ of this action
        -- should be put.
        -> IO ()
    }

-- | A simple action, with no additional monitoring.
--
-- Prefer using this smart constructor over v'Action' whenever possible.
simpleAction :: ([Location] -> NE.NonEmpty Location -> IO ()) -> Action
simpleAction f = Action{action = f}

-- | A collection of t'Rule's and t'Action's for executing them.
--
-- Use the 'rules' smart constructor instead of directly using the v'Rules'
-- constructor.
--
-- Actions are registered using 'registerAction', and rules are registered
-- using 'registerRule'. Additional rule dependencies (whose changes should
-- trigger rule recompilation) are declared using 'declareRuleDependencies'.
--
-- The @env@ type parameter represents an extra argument, which usually
-- consists of information known to Cabal such as 'LocalBuildInfo' and
-- 'ComponentLocalBuildInfo'.
--
-- Even though t'Rule's and t'Action's are registered separately, the nature
-- of the arguments passed to an t'Action' corresponds with the description
-- of the 'Rule', e.g. the action will receive as many 'Location's in its first
-- argument as the rule declared 'dependencies'.
--
-- See t'Rule' for more information.
newtype Rules env
  = -- | Return a collection of t'Action's and their t'ActionId's, and then,
    -- in the inner computation, a collection of t'Rule's, each of which
    -- specifies its dependencies, results, and the t'ActionId' of the t'Action'
    -- to run in order to execute the rule, together with a list of
    -- monitored files or directories (when any of these change, the rules
    -- are out-of-date and must be recomputed).
    --
    -- You can think of this type signature as:
    --
    -- > env -> ( Map ActionId Action, IO ( [ Rule ], [ MonitorFileOrDir ] ) )
    --
    -- except that it is structured in such a way as to avoid having
    -- to manually create t'ActionId' values.
    Rules {runRules :: env -> RulesM ()}

-- | __Warning__: this 'Semigroup' instance is not commutative.
instance Semigroup (Rules env) where
  (Rules rs1) <> (Rules rs2) =
    Rules $ \inputs -> do
      x1 <- rs1 inputs
      x2 <- rs2 inputs
      return $ do
        y1 <- x1
        y2 <- x2
        return $ y1 <> y2

instance Monoid (Rules env) where
  mempty = Rules $ const noRules

-- | An empty collection of rules.
noRules :: RulesM ()
noRules = pure $ pure ()

-- | Construct a collection of rules.
--
-- Prefer using this smart constructor over v'Rules' whenever possible.
rules :: (env -> RulesM ()) -> Rules env
rules f = Rules{runRules = f}

-- | Internal function: run the monadic 'Rules' computations in order
-- to obtain all the 'Action's and 'Rule's.
computeRules
  :: env
  -> Rules env
  -> (Map ActionId Action, IO ([Rule], [MonitorFileOrDir]))
computeRules inputs (Rules rs) =
  (actionFromId, Writer.execWriterT getRules)
  where
    (getRules, actionFromId) = runIdentity $ runFreshT $ rs inputs

--------------------------------------------------------------------------------
-- API for keeping track of all declared rules

-- | Monadic API for constructing rules.
type RulesM a = ActionsM (RulesT IO a)

-- | Monad transformer for defining rules. Usually wraps the 'IO' monad,
-- allowing @IO@ actions to be performed using @liftIO@.
type RulesT = Writer.WriterT ([Rule], [MonitorFileOrDir])

--------------------------------------------------------------------------------
-- API for ActionIds.

-- | A monad transformer for the registration of values, through the creation
-- of fresh identifiers for these values.
--
-- In practice, you should use 'ActionsM', which is specialised to the creation
-- of 'ActionId' identifiers.
newtype FreshT x x_id m a = FreshT {freshT :: State.StateT (Map x_id x) m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

runFreshT :: FreshT x x_id m a -> m (a, Map x_id x)
runFreshT = (`State.runStateT` Map.empty) . freshT

-- | Monad for defining actions. Does not support any 'IO'.
type ActionsM = FreshT Action ActionId Identity
