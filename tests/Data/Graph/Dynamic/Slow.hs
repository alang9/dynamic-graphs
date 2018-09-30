-- | A very slow but simple and hence probably correct implementation against we
-- can check our proper implementations.
module Data.Graph.Dynamic.Slow
    ( Graph
    , empty
    , insertVertex
    , insertEdge
    , deleteVertex
    , deleteEdge
    , hasEdge
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

insertVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
insertVertex v = Graph . HMS.insertWith HS.union v HS.empty . unGraph

insertEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> (Bool, Graph v)
insertEdge x y g0 = case HMS.lookup x (unGraph g0) of
    Nothing  -> (False, g0)
    Just set
        | y `HS.member` set -> (False, g0)
        | otherwise         ->
            let g1 = Graph $
                    HMS.insertWith HS.union x (HS.singleton y) $
                    HMS.insertWith HS.union y (HS.singleton x) $
                    unGraph g0 in
            (True, g1)

deleteVertex :: (Eq v, Hashable v) => v -> Graph v -> Graph v
deleteVertex x g0 =
    let nbs = neighbours x g0
        g1 = List.foldl' (\g n -> snd (deleteEdge x n g)) g0 nbs in
    Graph $ HMS.delete x $ unGraph g1


deleteEdge :: (Eq v, Hashable v) => v -> v -> Graph v -> (Bool, Graph v)
deleteEdge x y g0 = case HMS.lookup x (unGraph g0) of
    Nothing  -> (False, g0)
    Just set
        | y `HS.member` set ->
            let g1 = Graph $
                    HMS.adjust (HS.delete y) x $
                    HMS.adjust (HS.delete x) y $
                    unGraph g0 in
            (True, g1)
        | otherwise         -> (False, g0)

neighbours :: (Eq v, Hashable v) => v -> Graph v -> HS.HashSet v
neighbours x g = fromMaybe HS.empty $ HMS.lookup x (unGraph g)

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
vertices = map fst . HMS.toList . unGraph
