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

    , isSpanningForest
    ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.List           as List
import           Data.Maybe          (fromMaybe)
import qualified Data.Tree           as T

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
connected x y g = y `elem` component x g

-- | Find all vertices connected to this component.  The list is build lazily so
-- we can reuse this code efficiently in 'connected'.
component :: (Eq v, Hashable v) => v -> Graph v -> [v]
component x g = go HS.empty (HS.singleton x)
  where
    go visited queue = case HS.toList queue of
        []                          -> []
        (q : _)
            | q `HS.member` visited -> go visited (HS.delete q queue)
            | otherwise             ->
                let new = neighbours q g `HS.difference` visited in
                q : go (HS.insert q visited) (new `HS.union` HS.delete q queue)

vertices :: (Eq v, Hashable v) => Graph v -> [v]
vertices = map fst . HMS.toList . unGraph

-- | Verifies that a forest is a right proper spanning forest of a graph.
isSpanningForest :: (Eq v, Hashable v) => T.Forest v -> Graph v -> Bool
isSpanningForest forest graph =
    -- All items in the forest are unique.
    unique forest &&
    -- The forest covers the entire graph.
    HS.fromList (concatMap T.flatten forest) == HS.fromList (vertices graph) &&
    -- The components in the forest pairwise have the same elements as the
    -- components in the graph.
    and
        [ HS.fromList (T.flatten tree) == HS.fromList (component root graph)
        | tree@(T.Node root _) <- forest
        ] &&
    -- The edges in the spanning forest actually exist in the graph.
    and
        [ edge x y graph
        | (x, y) <- edges forest
        ]
  where
    unique :: (Eq a, Hashable a) => T.Forest a -> Bool
    unique =
        go HS.empty . concatMap T.flatten
      where
        go _acc []                  = True
        go acc  (x : xs)
                | x `HS.member` acc = False
                | otherwise         = go (HS.insert x acc) xs

    edges :: (Eq a, Hashable a) => T.Forest a -> [(a, a)]
    edges = concatMap go
      where
        go (T.Node root children) =
            [(root, x) | T.Node x _ <- children] ++
            concatMap go children
