-- | This module implements full dynamic grah connectivity.
--
-- It is based on:
-- /Poly-logarithmic deterministic fully-dynamic algorithms for connectivity,
-- minimum spanning tree, 2-edge, and biconnectivity/ by /Jacob Holm, Kristian
-- de Lichtenberg and Mikkel Thorup/ (1998).
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
    , new
    , new'
    , fromVertices
    , fromVertices'

      -- * Queries
    , connected
    , hasEdge

      -- * Modifying
    , insertEdge
    , deleteEdge
    , insertVertex
    , deleteVertex
    ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.HashMap.Strict                as HMS
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L
import           Data.Maybe                         (fromMaybe)
import           Data.Primitive.MutVar
import qualified Data.Vector.Mutable                as VM

import qualified Data.Graph.Dynamic.EulerTour       as ET
import qualified Data.Graph.Dynamic.Internal.Random as Random
import           Data.Graph.Dynamic.Internal.Tree   (Tree, Edge (..), Vertex (..))
import qualified Data.Graph.Dynamic.Internal.Tree   as Tree

type EdgeSet = HMS.HashMap Int (HS.HashSet Int)

linkEdgeSet :: Vertex -> Vertex -> EdgeSet -> EdgeSet
linkEdgeSet (Vertex x) (Vertex y) =
    HMS.insertWith HS.union x (HS.singleton y) .
    HMS.insertWith HS.union y (HS.singleton x)

cutEdgeSet :: Vertex -> Vertex -> EdgeSet -> EdgeSet
cutEdgeSet (Vertex x) (Vertex y) =
    HMS.adjust (HS.delete x) y . HMS.adjust (HS.delete y) x

memberEdgeSet :: Vertex -> Vertex -> EdgeSet -> Bool
memberEdgeSet (Vertex x) (Vertex y) = maybe False (y `HS.member`) . HMS.lookup x

data L t s = L
  { numEdges :: !Int
  , allEdges :: !EdgeSet
  , unLevels :: !(VM.MVector s (ET.Forest t s, EdgeSet))
  }

newtype Graph t s = Graph (MutVar s (L t s))

type Graph' s = Graph Random.Tree s

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

new :: (Tree t, PrimMonad m, s ~ PrimState m) => m (Graph t s)
new = fromVertices []

new' :: (PrimMonad m, s ~ PrimState m) => m (Graph' s)
new' = new

fromVertices
    :: (Tree t, PrimMonad m, s ~ PrimState m) => [Vertex] -> m (Graph t s)
fromVertices xs = do
  unLevels <- VM.new 0
  let allEdges = HMS.fromList $ zip (map unVertex xs) $ repeat HS.empty
      numEdges = 0
  Graph <$> newMutVar L {..}

fromVertices'
    :: (PrimMonad m, s ~ PrimState m) => [Vertex] -> m (Graph' s)
fromVertices' = fromVertices

-- TODO (jaspervdj): Kill Ord constraints in this module
insertEdge :: (Tree t, PrimMonad m, s ~ PrimState m) => Graph t s -> Vertex -> Vertex -> m ()
insertEdge (Graph levels) a b = do --traceShow (numEdges, VM.length unLevels, HS.member (a, b) allEdges) $
  L {..} <- readMutVar levels
  let !newAllEdges = linkEdgeSet a b allEdges
      !newNumEdges = numEdges + 1

  if memberEdgeSet a b allEdges || a == b
    then return ()
    else do
      unLevels' <- do
        let oldNumLevels = VM.length unLevels
        newUnLevels <- VM.take (logBase2 newNumEdges + 1) <$>
          VM.grow unLevels (max 0 $ logBase2 newNumEdges - oldNumLevels + 1)
        forM_ [oldNumLevels .. logBase2 newNumEdges] $ \levelIdx -> do
          df <- ET.discreteForest $ map (Vertex . fst) $ HMS.toList allEdges
          VM.write newUnLevels levelIdx (df, HMS.empty)
        return newUnLevels
      -- traceShowM (VM.null levels')
      if VM.null unLevels'
        then return ()
        else do
          (thisEtf, thisNonTreeEdges) <- VM.read unLevels' 0
          isTreeEdge <- ET.insertEdge thisEtf a b
          -- traceShowM $ (newNumEdges, m'newEtf)
          -- traceShowM $ (newNumEdges, "test3")
          let !thisNonTreeEdges'
                | isTreeEdge = thisNonTreeEdges
                | otherwise  = linkEdgeSet a b thisNonTreeEdges

          VM.write unLevels' 0 (thisEtf, thisNonTreeEdges')
          writeMutVar levels $ L
              {allEdges = newAllEdges, unLevels = unLevels',numEdges = newNumEdges}

connected :: (Tree t, PrimMonad m, s ~ PrimState m) => Graph t s -> Vertex -> Vertex -> m (Maybe Bool)
connected _ a b | a == b = return (Just True)
connected (Graph levels) a b = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return (Just False)
    else do
      (etf, _) <- VM.read unLevels 0
      ET.connected etf a b

hasEdge :: (Tree t, PrimMonad m, s ~ PrimState m) => Graph t s -> Vertex -> Vertex -> m Bool
hasEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  return $ memberEdgeSet a b allEdges

deleteEdge :: forall t m s. (Tree t, PrimMonad m, s ~ PrimState m) => Graph t s -> Vertex -> Vertex -> m ()
deleteEdge (Graph levels) (Vertex a) (Vertex b) = do
  L {..} <- readMutVar levels
  let newAllEdges = cutEdgeSet (Vertex a) (Vertex b) allEdges
  -- | a == b = return Graph {..}
  if VM.length unLevels == 0 || a == b
    then return ()
    else do
      cut <- go unLevels (VM.length unLevels-1)
      let newNumEdges = if cut then numEdges - 1 else numEdges
      writeMutVar levels L {allEdges = newAllEdges, numEdges = newNumEdges, ..}
  where
    go :: VM.MVector s (ET.Forest t s, EdgeSet) -> Int -> m Bool
    go unLevels idx = do
      -- traceShowM ("go", idx)
      (etf, nonTreeEdges0) <- VM.read unLevels idx
      cutResult <- ET.deleteEdge etf (Vertex a) (Vertex b)
      case cutResult of
        False -> do
          let !nonTreeEdges1 = cutEdgeSet (Vertex a) (Vertex b) nonTreeEdges0
          VM.write unLevels idx (etf, nonTreeEdges1)
          if idx > 0 then go unLevels (idx - 1) else return False
        True -> do
          aSize <- ET.componentSize etf (Vertex a)
          bSize <- ET.componentSize etf (Vertex b)
          let (smaller, _bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot etf (Vertex smaller)

          -- These are all edges, and vertices within the smaller tree.
          sTreeEdges <- Tree.toList sRoot
          let !sVertices = HS.fromList [x | Edge x y <- sTreeEdges, x == y]

          -- We need to consider all edges incident to the smaller tree.
          let sIncidentEdges =
                [ Edge x y
                | x <- HS.toList sVertices
                , y <- maybe [] HS.toList (HMS.lookup x nonTreeEdges0)
                ]

          -- Find a replacement and punish all edges we visit.
          let findRep punish [] = (punish, Nothing)
              findRep punish (Edge x y : candidates)
                | y `HS.member` sVertices =
                    findRep (Edge x y : punish) candidates
                | otherwise =
                    (punish, Just (Edge x y))

          -- Perform the search
          let (punished, replacementEdge) = findRep [] sIncidentEdges

          -- Increase the levels of the tree edges and the punished edges.
          nonTreeEdges1 <- if
              | idx + 1 >= VM.length unLevels -> return nonTreeEdges0
              | otherwise -> do
                    (incEtf, incNonTreeEdges0) <- VM.read unLevels (idx + 1)

                    let moveTreeEdge (Edge x y) =
                            ET.insertEdge incEtf (Vertex x) (Vertex y)

                    let moveNonTreeEdge !(ntes, !incNTes) (Edge x y) =
                            ( cutEdgeSet (Vertex x) (Vertex y) ntes
                            , linkEdgeSet (Vertex x) (Vertex y) incNTes
                            )

                    mapM_ moveTreeEdge sTreeEdges
                    let !(!nonTreeEdges1, !incNonTreeEdges1) = L.foldl'
                            moveNonTreeEdge (nonTreeEdges0, incNonTreeEdges0) punished

                    VM.write unLevels (idx + 1) (incEtf, incNonTreeEdges1)
                    return nonTreeEdges1

          case replacementEdge of
            Nothing  -> do
              VM.write unLevels idx (etf, nonTreeEdges1)
              if idx > 0 then go unLevels (idx - 1) else return True
            Just rep@(Edge c d) -> do
              let !nonTreeEdges2 = cutEdgeSet (Vertex c) (Vertex d) nonTreeEdges1
              VM.write unLevels idx (etf, nonTreeEdges2)
              _ <- ET.insertEdge etf (Vertex c) (Vertex d)
              propagateReplacement unLevels (idx - 1) rep
              return True

    propagateReplacement unLevels idx (Edge c d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
      _ <- ET.deleteEdge etf (Vertex a) (Vertex b)
      _ <- ET.insertEdge etf (Vertex c) (Vertex d)
      -- TODO: mess with edges??
      propagateReplacement unLevels (idx - 1) (Edge c d)

insertVertex
    :: (Tree t, PrimMonad m) => Graph t (PrimState m) -> Vertex -> m ()
insertVertex (Graph g) (Vertex x) = do
    l@L {..} <- readMutVar g
    let newAllEdges   = HMS.insertWith HS.union x HS.empty allEdges
        updateLevel i
            | i >= VM.length unLevels = return ()
            | otherwise               = do
                (forest, nt) <- VM.read unLevels i
                ET.insertVertex forest (Vertex x)
                VM.write unLevels i (forest, nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar g $ l {allEdges = newAllEdges}

deleteVertex
    :: (Tree t, PrimMonad m) => Graph t (PrimState m) -> Vertex -> m ()
deleteVertex g@(Graph levels) (Vertex x) = do
    l0 <- readMutVar levels
    let neighbours = fromMaybe HS.empty (HMS.lookup x (allEdges l0))
    forM_ neighbours $ \y -> deleteEdge g (Vertex x) (Vertex y)

    l1 <- readMutVar levels
    let newAllEdges = HMS.delete x (allEdges l1)
        updateLevel i
            | i >= VM.length (unLevels l1) = return ()
            | otherwise                    = do
                (forest, nt) <- VM.read (unLevels l1) i
                ET.deleteVertex forest (Vertex x)
                VM.write (unLevels l1) i (forest, HMS.delete x nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar levels $ l1 {allEdges = newAllEdges}
