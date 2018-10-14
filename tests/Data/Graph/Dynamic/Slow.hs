-- | A very slow but simple and hence probably correct implementation against we
-- can check our proper implementations.
module Data.Graph.Dynamic.Slow
    ( Graph
    , empty
    , fromVertices
    , insertVertex
    , insertEdge
    , deleteVertex
    , deleteEdge
    , hasEdge
    , connected
    , neighbours
    , vertices
    , numVertices
    ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.List           as List
import           Data.Maybe          (fromMaybe)

data Graph v = Graph
    { gNumVertices :: !Int
    , gGraph       :: !(HMS.HashMap v (HS.HashSet v))
    } deriving (Show)

empty :: Graph v
empty = Graph 0 HMS.empty

fromVertices :: (Eq v, Hashable v) => [v] -> Graph v
fromVertices verts =
    let graph = HMS.fromList [(v, HS.empty) | v <- verts] in
    Graph (HMS.size graph) graph

insertVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
insertVertex v g = case HMS.lookup v (gGraph g) of
    Just _  -> g
    Nothing -> Graph
        { gGraph       = HMS.insert v HS.empty (gGraph g)
        , gNumVertices = gNumVertices g + 1
        }

insertEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
insertEdge x y g =
    let graph =
            HMS.insertWith HS.union x (HS.singleton y) $
            HMS.insertWith HS.union y (HS.singleton x) $
            gGraph g in
    g {gGraph = graph}

deleteVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
deleteVertex x g | not (x `HMS.member` gGraph g) = g
deleteVertex x g0 =
    let nbs = neighbours x g0
        g1  = List.foldl' (\g n -> deleteEdge x n g) g0 nbs in
    g0 {gNumVertices = gNumVertices g1 - 1, gGraph = HMS.delete x (gGraph g1)}


deleteEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Graph v
deleteEdge x y g =
    let graph =
            HMS.adjust (HS.delete y) x $
            HMS.adjust (HS.delete x) y $
            gGraph g in
    g {gGraph = graph}

neighbours :: (Eq v, Hashable v) => v -> Graph v -> HS.HashSet v
neighbours x g = fromMaybe HS.empty $ HMS.lookup x (gGraph g)

hasEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> Bool
hasEdge x y g = y `HS.member` neighbours x g

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
vertices = map fst . HMS.toList . gGraph

numVertices :: (Eq v, Hashable v) => Graph v -> Int
numVertices = gNumVertices
