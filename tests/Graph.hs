module Graph where

import Data.Set (Set)
import qualified Data.Set as Set

type Vertex = Int

data Graph = Graph
  { numNodes :: !Vertex
  , edges :: !(Set (Vertex, Vertex))
  }

discreteGraph :: Int -> Graph
discreteGraph n = Graph n $ Set.fromList [(v,v) | v <- [0..n-1]]

component :: Vertex -> Graph -> Set.Set Vertex
component v (Graph n edges) = go [v] Set.empty
  where
    go [] comp = comp
    go (x:xs) comp
      | Set.member x comp = go xs comp
      | otherwise = go newQueue $ Set.insert x comp
      where
        newQueue = [v' | v' <- [0..n - 1], Set.member (x, v') edges, Set.notMember v' comp] ++ xs
