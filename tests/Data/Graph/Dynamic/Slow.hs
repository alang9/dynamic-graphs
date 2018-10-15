-- | A very slow but simple and hence probably correct implementation against we
-- can check our proper implementations.
module Data.Graph.Dynamic.Slow
    ( Graph
    , empty
    , edgeless
    , insert
    , link
    , delete
    , cut
    , edge
    , connected
    , neighbours
    , vertices
    ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.List           as List
import           Data.Maybe          (fromMaybe)

newtype Graph v = Graph
    { unGraph :: HMS.HashMap v (HS.HashSet v)
    } deriving (Show)

empty :: Graph v
empty = Graph HMS.empty

edgeless :: (Eq v, Hashable v) => [v] -> Graph v
edgeless verts = Graph $
    HMS.fromList [(v, HS.empty) | v <- verts]

insert :: (Eq v, Hashable v) => v -> Graph v -> Graph v
insert v = Graph . HMS.insert v HS.empty . unGraph

link :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
link x y g = Graph $
    HMS.insertWith HS.union x (HS.singleton y) $
    HMS.insertWith HS.union y (HS.singleton x) $
    unGraph g

delete :: (Eq v, Hashable v) => v -> Graph v -> Graph v
delete x g | not (x `HMS.member` unGraph g) = g
delete x g0 =
    let nbs = neighbours x g0
        g1  = List.foldl' (\g n -> cut x n g) g0 nbs in
    Graph $ HMS.delete x (unGraph g1)


cut :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
cut x y g =
    let graph =
            HMS.adjust (HS.delete y) x $
            HMS.adjust (HS.delete x) y $
            unGraph g in
    g {unGraph = graph}

neighbours :: (Eq v, Hashable v) => v -> Graph v -> HS.HashSet v
neighbours x g = fromMaybe HS.empty $ HMS.lookup x (unGraph g)

edge :: (Eq v, Hashable v) => v -> v -> Graph v -> Bool
edge x y g = y `HS.member` neighbours x g

connected :: (Eq v, Hashable v) => v -> v -> Graph v -> Bool
connected x y g = go HS.empty (HS.singleton x)
  where
    go visited queue = case HS.toList queue of
        []                          -> False
        (q : _)
            | q `HS.member` visited -> go visited (HS.delete q queue)
            | q == y                -> True
            | otherwise             ->
                let new = neighbours q g `HS.difference` visited in
                go (HS.insert q visited) (new `HS.union` HS.delete q queue)

vertices :: (Eq v, Hashable v) => Graph v -> [v]
vertices = map fst . HMS.toList . unGraph
