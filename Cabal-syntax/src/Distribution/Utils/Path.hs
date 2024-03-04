{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.Utils.Path
  ( FileOrDir (..)
  , AllowAbsolute (..)

    -- * Symbolic paths
  , RelativePath
  , SymbolicPath
  , SymbolicPathX -- NB: constructor not exposed, to retain type safety.
  , getSymbolicPath
  , sameDirectory
  , makeRelativePathEx
  , makeSymbolicPath
  , unsafeMakeSymbolicPath
  , interpretSymbolicPath
  , absoluteWorkingDir
  , tryMakeRelativeToWorkingDir
  , (</>)
  , (<.>)
  , coerceSymbolicPath
  , unsafeCoerceSymbolicPath
  , relativeSymbolicPath
  , symbolicPathRelative_maybe
  , takeDirectorySymbolicPath
  , dropExtensionsSymbolicPath
  , normaliseSymbolicPath
  , moduleNameSymbolicPath
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Coerce

import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
  ( toFilePath
  )
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (isAbsoluteOnAnyPlatform)

import qualified Distribution.Compat.CharParsing as P

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import GHC.Stack
  ( HasCallStack
  )
import GHC.TypeLits
  ( KnownSymbol
  , Symbol
  )

-------------------------------------------------------------------------------

-- * SymbolicPath

-------------------------------------------------------------------------------

{- Note [Symbolic paths]
~~~~~~~~~~~~~~~~~~~~~~~~
We want functions within the Cabal library to support getting the working
directory from their arguments, rather than retrieving it from the current
directory, which depends on the the state of the current process
(via getCurrentDirectory).

With such a constraint, to ensure correctness we need to know, for each relative
filepath, whether it is relative to the passed in working directory or to the
current working directory. We achieve this with the following API:

  - newtype SymbolicPath from to
  - getSymbolicPath :: SymbolicPath from to -> FilePath
  - interpretSymbolicPath
      :: Maybe (SymbolicPath "CWD" (Dir from)) -> SymbolicPath from to -> FilePath

Here, a symbolic path refers to an **uninterpreted** file path, i.e. any
passed in working directory **has not** been taken into account.
Whenever we see a symbolic path, it is a sign we must take into account this
working directory in some way.
Thus, whenever we interact with the file system, we do the following:

  - in a direct interaction (e.g. `doesFileExist`), we must **interpret** the
    path, e.g.

      doCheck :: Maybe (SymbolicPath "CWD" (Dir from))
              -> SymbolicPath from (File to)
              -> Bool
      doCheck mbWorkDir file = doesFileExist $ interpretSymbolicPath mbWorkDir file

  - when invoking a sub-process (such as GHC), we instead **do not interpret**
    symbolic paths, and instead set the working directory of the sub-process:

      callGhc :: Maybe (SymbolicPath "CWD" (Dir "Package"))
              -> SymbolicPath (Dir "Package") (File "Source")
              -> IO ()
      callGhc mbWorkDir inputFile =
        runProgramInvocation $
          programInvocationCwd mbWorkDir ghcProg [getSymbolicPath inputFile]

    Here, the input filepath is **uninterpreted** (we use getSymbolicPath
    rather than interpretSymbolicPath). The working directory is handled by
    using programInvocationCwd rather than programInvocation, which sets the
    working directory of the child process.

In practice, we often use:

  -- Interpret a symbolic path, to take into account the working directory.
  i :: SymbolicPath "Package" to -> FilePath
  i = interpretSymbolicPath mbWorkDir

  -- Do not interpret the symbolic path; the working directory
  -- is taken into account in some other way.
  u :: SymbolicPath "Package" to -> FilePath
  u = getSymbolicPath
-}

{- Note [Symbolic relative paths]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines:

  data kind AllowAbsolute = AllowAbsolute | OnlyRelative
  data kind FileOrDir = File Symbol | Dir Symbol

  type SymbolicPathX :: AllowAbsolute -> Symbol -> FileOrDir -> Type
  newtype SymbolicPathX allowAbsolute from to = SymbolicPath FilePath

  type RelativePath = SymbolicPathX 'OnlyRelative
  type SymbolicPath = SymbolicPathX 'AllowAbsolute

A 'SymbolicPath' is thus a symbolic path that is allowed to be absolute, whereas
a 'RelativePath' is a symbolic path that is additionally required to be relative.

This distinction allows us to keep track of which filepaths must be kept
relative.
-}

-- | A type-level symbolic name, to an abstract file or directory
-- (e.g. the Cabal package directory).
data FileOrDir
  = -- | The abstract name of a file or category of files,
    -- e.g. source files or data files.
    File Symbol
  | -- | The abstract name of a directory or category of directories,
    -- e.g. the package directory or source directories.
    Dir Symbol

-- | Is this symbolic path allowed to be absolute, or must it be relative?
data AllowAbsolute
  = -- | The path may be absolute, or it may be relative.
    AllowAbsolute
  | -- | The path must be relative.
    OnlyRelative

-- | A symbolic path, possibly relative to an abstract location specified
-- by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
newtype SymbolicPathX (allowAbsolute :: AllowAbsolute) (from :: Symbol) (to :: FileOrDir)
  = SymbolicPath FilePath
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

type role SymbolicPathX nominal nominal nominal

-- | A symbolic relative path, relative to an abstract location specified
-- by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type RelativePath = SymbolicPathX 'OnlyRelative

-- | A symbolic path which is allowed to be absolute.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type SymbolicPath = SymbolicPathX 'AllowAbsolute

instance Binary (SymbolicPathX allowAbsolute from to)
instance
  (Typeable allowAbsolute, KnownSymbol from, Typeable to)
  => Structured (SymbolicPathX allowAbsolute from to)
instance NFData (SymbolicPathX allowAbsolute from to) where rnf = genericRnf

-- | Extract the 'FilePath' underlying a 'SymbolicPath' or 'RelativePath',
-- without interpreting it.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
getSymbolicPath :: SymbolicPathX allowAbsolute from to -> FilePath
getSymbolicPath (SymbolicPath p) = p

-- | A symbolic path from a directory to itself.
sameDirectory :: SymbolicPathX allowAbsolute from (Dir to)
sameDirectory = SymbolicPath "."

-- | Make a 'RelativePath', ensuring the path is not absolute,
-- but performing no further checks.
makeRelativePathEx :: HasCallStack => FilePath -> RelativePath from to
makeRelativePathEx fp
  | isAbsoluteOnAnyPlatform fp =
      error $ "Cabal internal error: makeRelativePathEx: absolute path " ++ fp
  | otherwise =
      SymbolicPath fp

-- | Make a 'SymbolicPath', which may be relative or absolute.
makeSymbolicPath :: FilePath -> SymbolicPath from to
makeSymbolicPath fp = SymbolicPath fp

-- | Make a 'SymbolicPath' which may be relative or absolute,
-- without performing any checks.
--
-- Avoid using this function in new code.
unsafeMakeSymbolicPath :: FilePath -> SymbolicPathX allowAbs from to
unsafeMakeSymbolicPath fp = SymbolicPath fp

-- | Like 'System.FilePath.takeDirectory', for symbolic paths.
takeDirectorySymbolicPath :: SymbolicPathX allowAbsolute from (File to) -> SymbolicPathX allowAbsolute from (Dir to')
takeDirectorySymbolicPath (SymbolicPath fp) = SymbolicPath (FilePath.takeDirectory fp)

-- | Like 'System.FilePath.dropExtensions', for symbolic paths.
dropExtensionsSymbolicPath :: SymbolicPathX allowAbsolute from (File to) -> SymbolicPathX allowAbsolute from (File to')
dropExtensionsSymbolicPath (SymbolicPath fp) = SymbolicPath (FilePath.dropExtensions fp)

-- | Like 'System.FilePath.normalise', for symbolic paths.
normaliseSymbolicPath :: SymbolicPathX allowAbsolute from to -> SymbolicPathX allowAbsolute from to
normaliseSymbolicPath (SymbolicPath fp) = SymbolicPath (FilePath.normalise fp)

-- | Retrieve the relative symbolic path to a Haskell module.
moduleNameSymbolicPath :: ModuleName -> SymbolicPathX allowAbsolute "Source" (File "Source")
moduleNameSymbolicPath modNm = SymbolicPath $ ModuleName.toFilePath modNm

-- | Interpret a symbolic path with respect to the given directory.
--
-- Use this before directly interacting with the file system.
--
-- NB: when invoking external programs (such as @GHC@), it is preferable to set
-- the working directory of the process rather than calling this function, as
-- this function will turn relative paths into absolute paths if the working
-- directory is an absolute path. This can degrade error messages, or worse,
-- break the behaviour entirely (because the program might expect certain paths
-- to be relative).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
interpretSymbolicPath :: Maybe (SymbolicPath "CWD" (Dir from)) -> SymbolicPathX allowAbsolute from to -> FilePath
interpretSymbolicPath mbWorkDir (SymbolicPath p) =
  -- Note that this properly handles an absolute symbolic path,
  -- because if @p@ is absolute, then @blah </> p = p@.
  maybe p ((</> p) . getSymbolicPath) mbWorkDir

-- | Change what a symbolic path is pointing to.
coerceSymbolicPath :: SymbolicPathX allowAbsolute from to1 -> SymbolicPathX allowAbsolute from to2
coerceSymbolicPath = coerce

-- | Change both what a symbolic path is pointing from and pointing to.
--
-- Avoid using this in new code.
unsafeCoerceSymbolicPath :: SymbolicPathX allowAbsolute from1 to1 -> SymbolicPathX allowAbsolute from2 to2
unsafeCoerceSymbolicPath = coerce

-- | Weakening: convert a relative symbolic path to a symbolic path,
-- \"forgetting\" that it is relative.
relativeSymbolicPath :: RelativePath from to -> SymbolicPath from to
relativeSymbolicPath (SymbolicPath fp) = SymbolicPath fp

-- | Is this symbolic path a relative symbolic path?
symbolicPathRelative_maybe :: SymbolicPath from to -> Maybe (RelativePath from to)
symbolicPathRelative_maybe (SymbolicPath fp) =
  if isAbsoluteOnAnyPlatform fp
    then Nothing
    else Just $ SymbolicPath fp

-- | Absolute path to the current working directory.
absoluteWorkingDir :: Maybe (SymbolicPath "CWD" to) -> IO FilePath
absoluteWorkingDir Nothing = Directory.getCurrentDirectory
absoluteWorkingDir (Just wd) = Directory.makeAbsolute $ getSymbolicPath wd

-- | Try to make a path relative to the current working directory.
--
-- NB: this function may fail to make the path relative.
tryMakeRelativeToWorkingDir :: Maybe (SymbolicPath "CWD" (Dir dir)) -> SymbolicPath dir to -> IO (SymbolicPath dir to)
tryMakeRelativeToWorkingDir mbWorkDir (SymbolicPath fp) = do
  wd <- absoluteWorkingDir mbWorkDir
  return $ SymbolicPath (FilePath.makeRelative wd fp)

-------------------------------------------------------------------------------

-- ** Parsing and pretty printing

-------------------------------------------------------------------------------

instance Parsec (SymbolicPathX 'OnlyRelative from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else
        if isAbsoluteOnAnyPlatform token
          then P.unexpected "absolute FilePath"
          else return (SymbolicPath token)

instance Parsec (SymbolicPathX 'AllowAbsolute from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (SymbolicPath token)

instance Pretty (SymbolicPathX allowAbsolute from to) where
  pretty = showFilePath . getSymbolicPath

-------------------------------------------------------------------------------

-- * Composition

-------------------------------------------------------------------------------

infixr 7 <.>

-- | Types that support 'System.FilePath.<.>'.
class FileLike p where
  -- | Like 'System.FilePath.<.>', but also supporting symbolic paths.
  (<.>) :: p -> String -> p

instance FileLike FilePath where
  (<.>) = (FilePath.<.>)

instance p ~ File f => FileLike (SymbolicPathX allowAbsolute dir p) where
  SymbolicPath p <.> ext = SymbolicPath (p <.> ext)

infixr 5 </>

-- | Types that support 'System.FilePath.</>'.
class PathLike p q r | q r -> p, p r -> q, p q -> r where
  -- | Like 'System.FilePath.</>', but also supporting symbolic paths.
  (</>) :: p -> q -> r

instance PathLike FilePath FilePath FilePath where
  (</>) = (FilePath.</>)

-- | This instance ensures we don't accidentally discard a symbolic path
-- in a 'System.FilePath.</>' operation due to the second path being absolute.
--
-- (Recall that @a </> b = b@ whenever @b@ is absolute.)
instance
  (b1 ~ Dir b2, a3 ~ a1, c2 ~ c3, midAbsolute ~ OnlyRelative)
  => PathLike
      (SymbolicPathX allowAbsolute a1 b1)
      (SymbolicPathX midAbsolute b2 c2)
      (SymbolicPathX allowAbsolute a3 c3)
  where
  SymbolicPath p1 </> SymbolicPath p2 = SymbolicPath (p1 </> p2)
