module Node.Glob.Basic
  ( expandGlobs
  , expandGlobsCwd
  , expandGlobsWithStats
  , expandGlobsWithStatsCwd
  ) where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, attempt, catchError)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.FS.Aff as FS
import Node.FS.Stats (Stats)
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
expandGlobs pwd = map Map.keys <<< expandGlobsWithStats pwd

-- | Expands globs relative to the current working directory, returning Stats.
expandGlobsWithStatsCwd :: Array String -> Aff (Map FilePath Stats)
expandGlobsWithStatsCwd globs = do
  cwd <- liftEffect Process.cwd
  expandGlobsWithStats cwd globs

-- | Expands globs relative to the provided directory, returning Stats.
expandGlobsWithStats :: FilePath -> Array String -> Aff (Map FilePath Stats)
expandGlobsWithStats pwd initGlobs = do
  result <- liftEffect $ Ref.new Map.empty
  let
    insertMatch :: FilePath -> Stats -> Aff Unit
    insertMatch path stat = liftEffect $ Ref.modify_ (Map.insert path stat) result

    go :: FilePath -> Set Glob' -> Aff Unit
    go cwd globs = do
      let
        prefix = globs # foldMap \glob ->
          case fixedPrefix glob of
            Nothing -> { left: Set.singleton glob, right: mempty }
            Just pr -> { left: mempty, right: pr }

      dirMatches <-
        guard (not (Set.isEmpty prefix.left))
          $ flip catchError mempty
          $ match prefix.left <<< List.fromFoldable <$> FS.readdir cwd

      sequential $ forWithIndex_ (prefix.right <> dirMatches) \path m -> parallel do
        let fullPath = Path.concat [ cwd, path ]
        attempt (FS.stat fullPath) >>= case m, _ of
          MatchMore matchDir gs, Right stat | Stats.isDirectory stat -> do
            when matchDir $ insertMatch fullPath stat
            go fullPath gs
          MatchMore true _, Right stat ->
            insertMatch fullPath stat
          Match, Right stat ->
            insertMatch fullPath stat
          _, _ ->
            mempty

    globsWithDir =
      initGlobs
        # map (rootOrRelative pwd <<< fromPathWithSeparator Path.sep <<< fixupGlobPath)
        # Map.fromFoldableWith (<>)

  sequential $ forWithIndex_ globsWithDir \dir globs -> parallel do
    go dir globs

  liftEffect $ Ref.read result

fixupGlobPath :: String -> String
fixupGlobPath glob
  | Path.sep /= "/" = String.replaceAll (Pattern "/") (Replacement Path.sep) glob
  | otherwise = glob

rootOrRelative :: FilePath -> Glob' -> Tuple FilePath (Set Glob')
rootOrRelative pwd = case _ of
  GlobSegment ("" : Nil) : glob -> Tuple Path.sep (Set.singleton glob)
  glob -> Tuple pwd (Set.singleton glob)

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
    if List.null glob then
      Just $ SemigroupMap $ Map.singleton path Match
    else
      Just $ SemigroupMap $ Map.singleton path $ MatchMore false (Set.singleton glob)
  _ ->
    Nothing

match :: Set Glob' -> List FilePath -> SemigroupMap String Match
match globs = foldMap \path ->
  globs
    # foldMap (matchListing path)
    # foldMap (SemigroupMap <<< Map.singleton path)

data Match = Match | MatchMore Boolean (Set Glob')

isMatch :: Maybe Match -> Boolean
isMatch = case _ of
  Just Match -> true
  _ -> false

instance semigroupMatch :: Semigroup Match where
  append a b =
    case a of
      MatchMore am as ->
        case b of
          MatchMore bm bs -> MatchMore (am || bm) (as <> bs)
          _ -> a
      Match ->
        case b of
          MatchMore _ bs -> MatchMore true bs
          _ -> a

matchListing :: FilePath -> Glob' -> Maybe Match
matchListing path = case _ of
  GlobStar : GlobStar : glob ->
    matchListing path (GlobStar : glob)
  GlobStar : glob ->
    Just $ MatchMore (isMatch (matchListing path glob)) $ Set.fromFoldable [ glob, GlobStar : glob ]
  GlobSegment segment : glob ->
    if matchSegment path segment then
      if List.null glob then
        Just Match
      else
        Just $ MatchMore false $ Set.singleton glob
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
