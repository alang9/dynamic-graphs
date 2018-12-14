-- | This module implements full dynamic grah connectivity.
--
-- It is based on:
-- /Poly-logarithmic deterministic fully-dynamic algorithms for connectivity, minimum spanning tree, 2-edge, and biconnectivity/
-- by /Jacob Holm, Kristian de Lichtenberg and Mikkel Thorup/ (1998).
--
-- We use two naming conventions in this module:
--
-- * A prime suffix (@'@) indicates a simpler or less polymorphic version of a
-- function or datatype.  For example, see 'empty' and 'empty'', and
-- 'Graph' and 'Graph''.
--
-- * An underscore suffix (@_@) means that the return value is ignored.  For
-- example, see 'link' and 'link_'.
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dynamic.Levels
    ( -- * Type
      Graph
    , Graph'

      -- * Construction
    , empty
    , empty'
    , edgeless
    , edgeless'
    , complete
    , complete'

      -- * Queries
    , connected
    , edge
    , vertex
    , neighbours

      -- * Modifying
    , link
    , link_
    , cut
    , cut_
    , insert
    , insert_
    , delete
    , delete_

      -- * Advanced/internal
    , spanningForest
    ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Hashable                      (Hashable)
import qualified Data.HashMap.Strict                as HMS
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Primitive.MutVar
import qualified Data.Tree                          as DT
import qualified Data.Vector.Mutable                as VM

import qualified Data.Graph.Dynamic.EulerTour       as ET
import qualified Data.Graph.Dynamic.Internal.Random as Random
import           Data.Graph.Dynamic.Internal.Tree   (Tree)
import qualified Data.Graph.Dynamic.Internal.Tree   as Tree

type EdgeSet v = HMS.HashMap v (HS.HashSet v)

linkEdgeSet :: (Eq v, Hashable v) => v -> v -> EdgeSet v -> EdgeSet v
linkEdgeSet x y =
    HMS.insertWith HS.union x (HS.singleton y) .
    HMS.insertWith HS.union y (HS.singleton x)

cutEdgeSet :: (Eq v, Hashable v) => v -> v -> EdgeSet v -> EdgeSet v
cutEdgeSet x y = HMS.adjust (HS.delete x) y . HMS.adjust (HS.delete y) x

memberEdgeSet :: (Eq v, Hashable v) => v -> v -> EdgeSet v -> Bool
memberEdgeSet x y = maybe False (y `HS.member`) . HMS.lookup x

data L t s v = L
  { numVerts :: !Int
  , allEdges :: !(EdgeSet v)
  , unLevels :: !(VM.MVector s (ET.Forest t (Sum Int) s v, EdgeSet v))
  }

newtype Graph t s v = Graph (MutVar s (L t s v))

type Graph' s v = Graph Random.Tree s v

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

-- | /O(1)/
--
-- Create an empty graph.
empty :: (Eq v, Hashable v, Tree t, PrimMonad m) => m (Graph t (PrimState m) v)
empty = edgeless []

-- | Simple version of 'empty'.
empty' :: (Eq v, Hashable v, PrimMonad m) => m (Graph' (PrimState m) v)
empty' = empty

-- | Create a graph with the given vertices but no edges.
edgeless
  :: (Eq v, Hashable v, Tree t, PrimMonad m)
  => [v] -> m (Graph t (PrimState m) v)
edgeless xs = do
  unLevels <- VM.new 0
  let allEdges = HMS.empty
      numVerts = 0
  g <- Graph <$> newMutVar L {..}
  mapM_ (insert g) xs
  return g

-- | Simple version of 'edgeless'.
edgeless'
    :: (Eq v, Hashable v, PrimMonad m) => [v] -> m (Graph' (PrimState m) v)
edgeless' = edgeless

-- | Create the complete graph with the given vertices.
complete
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => [v] -> m (Graph t (PrimState m) v)
complete vertices = do
    g <- edgeless vertices
    forM_ (pairs vertices) $ \(x, y) -> link g x y
    return g
  where
    pairs :: [a] -> [(a, a)]
    pairs []       = []
    pairs (x : xs) =
        [(x, y) | y <- xs] ++ pairs xs

-- | Simple version of 'complete'
complete'
    :: (Eq v, Hashable v, PrimMonad m) => [v] -> m (Graph' (PrimState m) v)
complete' = complete

-- | Insert an edge in between two vertices.  If the vertices already have
-- an edge between them don't do anything.  Returns whether or not an edge was
-- actually inserted.
link
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> v -> m Bool
link (Graph levels) a b = do
  L {..} <- readMutVar levels
  let !newAllEdges = linkEdgeSet a b allEdges
  if memberEdgeSet a b allEdges || a == b || VM.null unLevels
    then return False
    else do
      (thisEtf, thisNonTreeEdges) <- VM.read unLevels 0
      isTreeEdge <- ET.link thisEtf a b
      let !thisNonTreeEdges'
            | isTreeEdge = thisNonTreeEdges
            | otherwise  = linkEdgeSet a b thisNonTreeEdges

      VM.write unLevels 0 (thisEtf, thisNonTreeEdges')
      writeMutVar levels $ L
          {allEdges = newAllEdges, unLevels = unLevels, numVerts = numVerts}
      return True

-- | Version of 'link' which ignores the result.
link_
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> v -> m ()
link_ g a b = void (link g a b)

-- | Check if a path exists in between two vertices.
connected
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> v -> m Bool
connected _ a b | a == b = return True
connected (Graph levels) a b = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return False
    else do
      (etf, _) <- VM.read unLevels 0
      ET.connected etf a b

-- | Check if this edge exists in the graph.
edge
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> v -> m Bool
edge (Graph levels) a b = do
    L {..} <- readMutVar levels
    return $ memberEdgeSet a b allEdges

-- | Check if this vertex exists in the graph.
vertex
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> m Bool
vertex (Graph levels) a = do
    L {..} <- readMutVar levels
    return $ a `HMS.member` allEdges

-- | Remove an edge in between two vertices.  If there is no edge in between
-- these vertices, do nothing.  Return whether or not an edge was actually
-- removed.
cut
    :: forall t m v. (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> v -> m Bool
cut (Graph levels) a b = do
  L {..} <- readMutVar levels
  let newAllEdges = cutEdgeSet a b allEdges
  if VM.length unLevels == 0 || a == b
    then return False
    else do
      cut' <- go unLevels (VM.length unLevels-1)
      writeMutVar levels L {allEdges = newAllEdges, ..}
      return cut'
  where
    go :: VM.MVector (PrimState m) (ET.Forest t (Sum Int) (PrimState m) v, EdgeSet v) -> Int -> m Bool
    go unLevels idx = do
      -- traceShowM ("go", idx)
      (etf, nonTreeEdges0) <- VM.read unLevels idx
      cutResult <- ET.cut etf a b
      case cutResult of
        False -> do
          let !nonTreeEdges1 = cutEdgeSet a b nonTreeEdges0
          VM.write unLevels idx (etf, nonTreeEdges1)
          if idx > 0 then go unLevels (idx - 1) else return False
        True -> do
          aSize <- ET.componentSize etf a
          bSize <- ET.componentSize etf b
          let (smaller, _bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot etf smaller

          -- These are all edges, and vertices within the smaller tree.
          sTreeEdges <- Tree.toList sRoot
          let !sVertices = HS.fromList $ map fst $
                    filter (uncurry (==)) sTreeEdges

          -- We need to consider all edges incident to the smaller tree.
          let sIncidentEdges =
                [ (x, y)
                | x <- HS.toList sVertices
                , y <- maybe [] HS.toList (HMS.lookup x nonTreeEdges0)
                ]

          -- Find a replacement and punish all edges we visit.
          let findRep punish [] = (punish, Nothing)
              findRep punish ((x, y) : candidates)
                | y `HS.member` sVertices =
                    findRep ((x, y) : punish) candidates
                | otherwise =
                    (punish, Just (x, y))

          -- Perform the search
          let (punished, replacementEdge) = findRep [] sIncidentEdges

          -- Increase the levels of the tree edges and the punished edges.
          nonTreeEdges1 <- if
              | idx + 1 >= VM.length unLevels -> return nonTreeEdges0
              | otherwise -> do
                    (incEtf, incNonTreeEdges0) <- VM.read unLevels (idx + 1)

                    let moveTreeEdge (x, y) =
                            ET.link_ incEtf x y

                    let moveNonTreeEdge !(ntes, !incNTes) (x, y) =
                            (cutEdgeSet x y ntes, linkEdgeSet x y incNTes)

                    mapM_ moveTreeEdge sTreeEdges
                    let !(!nonTreeEdges1, !incNonTreeEdges1) = L.foldl'
                            moveNonTreeEdge (nonTreeEdges0, incNonTreeEdges0) punished

                    VM.write unLevels (idx + 1) (incEtf, incNonTreeEdges1)
                    return nonTreeEdges1

          case replacementEdge of
            Nothing  -> do
              VM.write unLevels idx (etf, nonTreeEdges1)
              if idx > 0 then go unLevels (idx - 1) else return True
            Just rep@(c, d) -> do
              let !nonTreeEdges2 = cutEdgeSet c d nonTreeEdges1
              VM.write unLevels idx (etf, nonTreeEdges2)
              ET.link_ etf c d
              propagateReplacement unLevels (idx - 1) rep
              return True

    propagateReplacement unLevels idx (c, d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
      ET.cut_ etf a b
      ET.link_ etf c d
      -- TODO: mess with edges??
      propagateReplacement unLevels (idx - 1) (c, d)

-- | Version of 'cut' which ignores the result.
cut_
    :: forall t m v. (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> v -> m ()
cut_ g a b = void (cut g a b)

-- | Insert a new vertex.  Do nothing if it is already there.  Returns whether
-- or not a vertex was inserted in the graph.
insert
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> m Bool
insert (Graph g) x = do
  l@L {..} <- readMutVar g
  if HMS.member x allEdges then
      return False
  else do
    let newAllEdges = HMS.insert x HS.empty allEdges
    let numVertices = numVerts + 1
    unLevels' <- do
      let oldNumLevels = VM.length unLevels
      newUnLevels <- VM.take (logBase2 numVertices + 1) <$>
        VM.grow unLevels (max 0 $ logBase2 numVertices - oldNumLevels + 1)
      forM_ [oldNumLevels .. logBase2 numVertices] $ \levelIdx -> do
        df <- ET.edgeless (\v1 v2 -> if v1 == v2 then Sum 1 else Sum 0) $ map fst $ HMS.toList allEdges
        VM.write newUnLevels levelIdx (df, HMS.empty)
      return newUnLevels
    let updateLevel i
            | i >= VM.length unLevels' = return ()
            | otherwise               = do
                (forest, nt) <- VM.read unLevels' i
                ET.insert_ forest x
                VM.write unLevels' i (forest, nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar g $ l {allEdges = newAllEdges, unLevels = unLevels', numVerts = numVertices}
    return True

-- | Version of 'insert' which ignores the result.
insert_
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> m ()
insert_ g x = void (insert g x)

-- | Remove a vertex from the graph, if it exists.  If it is connected to any
-- other vertices, those edges are cut first.  Returns whether or not a vertex
-- was removed from the graph.
delete
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> m Bool
delete g@(Graph levels) x = do
  l0 <- readMutVar levels
  case HMS.lookup x (allEdges l0) of
    Nothing -> return False
    Just nbs -> do
      forM_ nbs $ \y -> cut g x y

      l1 <- readMutVar levels
      let newAllEdges = HMS.delete x (allEdges l1)
          updateLevel i
              | i >= VM.length (unLevels l1) = return ()
              | otherwise                    = do
                  (forest, nt) <- VM.read (unLevels l1) i
                  ET.delete_ forest x
                  VM.write (unLevels l1) i (forest, HMS.delete x nt)
                  updateLevel (i + 1)

      updateLevel 0
      writeMutVar levels $ l1 {allEdges = newAllEdges, numVerts = numVerts l0 - 1}
      return True

-- | Version of 'delete' which ignores the result.
delete_
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> m ()
delete_ g x = void (delete g x)

-- | Get all neighbours of the given vertex.
neighbours
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> v -> m (HS.HashSet v)
neighbours (Graph levels) x = do
    l0 <- readMutVar levels
    return $ fromMaybe HS.empty (HMS.lookup x (allEdges l0))

-- | Obtain the current spanning forest.
spanningForest
    :: (Eq v, Hashable v, Tree t, PrimMonad m)
    => Graph t (PrimState m) v -> m (DT.Forest v)
spanningForest (Graph levels) = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return []
    else do
      (etf, _) <- VM.read unLevels 0
      ET.spanningForest etf
