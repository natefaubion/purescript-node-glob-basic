module Node.Glob.Basic
  ( expandGlobs
  , expandGlobsCwd
  ) where

import Prelude

import Control.Parallel (parTraverse, parallel, sequential)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process

-- | Expands globs relative to the current working directory.
expandGlobsCwd :: Array String -> Aff (Set FilePath)
expandGlobsCwd globs = do
  cwd <- liftEffect Process.cwd
  expandGlobs cwd globs

-- | Expands globs relative to the provided directory.
expandGlobs :: FilePath -> Array String -> Aff (Set FilePath)
expandGlobs pwd =
  map (fromPathWithSeparator Path.sep)
    >>> Set.fromFoldable
    >>> go pwd
  where
  go :: FilePath -> Set Glob' -> Aff (Set FilePath)
  go cwd globs = do
    let
      fullPath p =
        Path.concat [ cwd, p ]

      toListing path =
        fullPath path
          # FS.stat
          # map (Stats.isDirectory >>> { path, isDirectory: _ })

      prefix =
        globs # partitionFoldMap \glob -> case fixedPrefix glob of
          Nothing -> Left (Set.singleton glob)
          Just pr -> Right pr

    dirMatches <-
      guard (not (Set.isEmpty prefix.left)) $ flip catchError mempty do
        FS.readdir cwd
          >>= parTraverse toListing
          >>> map (List.fromFoldable >>> match prefix.left)
    let
      matches =
        (prefix.right <> dirMatches) # partitionFoldMapWithIndex \path -> case _ of
          Match -> Left (Set.singleton (fullPath path))
          MatchMore gs -> Right (parallel (go (fullPath path) gs))

    matches.right
      # map (append matches.left)
      # sequential

data Glob
  = GlobStar
  | GlobSegment (List String)

derive instance eqGlob :: Eq Glob
derive instance ordGlob :: Ord Glob

type Glob' = List Glob

fromString :: String -> Glob
fromString = case _ of
  "**" -> GlobStar
  str -> GlobSegment $ List.fromFoldable $ String.split (Pattern "*") str

fromFoldable :: forall f. Foldable f => f String -> Glob'
fromFoldable = foldr (Cons <<< fromString) Nil

fromPathWithSeparator :: String -> FilePath -> Glob'
fromPathWithSeparator sep = String.split (Pattern sep) >>> fromFoldable

fixedPrefix :: Glob' -> Maybe (SemigroupMap String Match)
fixedPrefix = case _ of
  GlobSegment (path : Nil) : glob | path /= "" ->
    Just $ SemigroupMap $ Map.singleton path $ MatchMore $ Set.singleton glob
  _ ->
    Nothing

type Listing =
  { path :: String
  , isDirectory :: Boolean
  }

match :: Set Glob' -> List Listing -> SemigroupMap String Match
match globs = foldMap \lst ->
  globs
    # foldMap (matchListing lst)
    # foldMap (SemigroupMap <<< Map.singleton lst.path)

data Match = Match | MatchMore (Set Glob')

instance semigroupMatch :: Semigroup Match where
  append a b =
    case a of
      MatchMore as ->
        case b of
          MatchMore bs -> MatchMore (as <> bs)
          _ -> a
      Match ->
        case b of
          MatchMore _ -> b
          _ -> Match

matchListing :: Listing -> Glob' -> Maybe Match
matchListing lst@{ path, isDirectory } = case _ of
  GlobStar : glob ->
    if isDirectory then
      Just $ MatchMore $ Set.fromFoldable [ glob, GlobStar : glob ]
    else
      matchListing lst glob
  GlobSegment segment : glob ->
    if matchSegment path segment then
      if List.null glob then
        Just Match
      else if isDirectory then
        Just $ MatchMore $ Set.singleton glob
      else
        Nothing
    else
      Nothing
  Nil ->
    Nothing

matchSegment :: String -> List String -> Boolean
matchSegment = go1
  where
  go1 str = case _ of
    Nil -> str == ""
    "" : ps -> go2 str ps
    p : ps ->
      case String.stripPrefix (Pattern p) str of
        Nothing -> false
        Just str' -> go2 str' ps

  go2 str = case _ of
    Nil -> str == ""
    "" : Nil -> true
    "" : ps -> go2 str ps
    p : ps ->
      case SCU.indexOf (Pattern p) str of
        Nothing -> false
        Just ix -> go2 (SCU.drop (ix + SCU.length p) str) ps

partitionFoldMap
  :: forall f a b c
   . Foldable f
  => Monoid b
  => Monoid c
  => (a -> Either b c)
  -> f a
  -> { left :: b, right :: c }
partitionFoldMap k = foldMap go
  where
  go value = case k value of
    Left left ->
      { left, right: mempty }
    Right right ->
      { left: mempty, right }

partitionFoldMapWithIndex
  :: forall ix f a b c
   . FoldableWithIndex ix f
  => Monoid b
  => Monoid c
  => (ix -> a -> Either b c)
  -> f a
  -> { left :: b, right :: c }
partitionFoldMapWithIndex k = foldMapWithIndex go
  where
  go ix value = case k ix value of
    Left left ->
      { left, right: mempty }
    Right right ->
      { left: mempty, right }
