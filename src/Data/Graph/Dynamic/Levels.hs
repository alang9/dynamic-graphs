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
import           Data.Hashable                      (Hashable)
import qualified Data.HashMap.Strict                as HMS
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Primitive.MutVar
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
  { numEdges :: !Int
  , allEdges :: !(EdgeSet v)
  , unLevels :: !(VM.MVector s (ET.Forest t (Sum Int) s v, EdgeSet v))
  }

newtype Graph t s v = Graph (MutVar s (L t s v))

type Graph' s v = Graph Random.Tree s v

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

new :: (Eq v, Hashable v, Tree t, PrimMonad m, s ~ PrimState m) => m (Graph t s v)
new = fromVertices []

new' :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => m (Graph' s v)
new' = new

fromVertices
    :: (Eq v, Hashable v, Tree t, PrimMonad m, s ~ PrimState m) => [v] -> m (Graph t s v)
fromVertices xs = do
  unLevels <- VM.new 0
  let allEdges = HMS.fromList $ zip xs $ repeat HS.empty
      numEdges = 0
  Graph <$> newMutVar L {..}

fromVertices'
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => [v] -> m (Graph' s v)
fromVertices' = fromVertices

-- TODO (jaspervdj): Kill Ord constraints in this module
insertEdge :: (Eq v, Hashable v, Tree t, PrimMonad m, s ~ PrimState m) => Graph t s v -> v -> v -> m ()
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
          df <- ET.discreteForest (\v1 v2 -> if v1 == v2 then Sum 1 else Sum 0) $ map fst $ HMS.toList allEdges
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

connected :: (Eq v, Hashable v, Tree t, PrimMonad m, s ~ PrimState m) => Graph t s v -> v -> v -> m (Maybe Bool)
connected _ a b | a == b = return (Just True)
connected (Graph levels) a b = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return (Just False)
    else do
      (etf, _) <- VM.read unLevels 0
      ET.connected etf a b

hasEdge :: (Eq v, Hashable v, Tree t, PrimMonad m, s ~ PrimState m) => Graph t s v -> v -> v -> m Bool
hasEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  return $ memberEdgeSet a b allEdges

deleteEdge :: forall t m s v. (Eq v, Hashable v, Tree t, PrimMonad m, s ~ PrimState m) => Graph t s v -> v -> v -> m ()
deleteEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  let newAllEdges = cutEdgeSet a b allEdges
  -- | a == b = return Graph {..}
  if VM.length unLevels == 0 || a == b
    then return ()
    else do
      cut <- go unLevels (VM.length unLevels-1)
      let newNumEdges = if cut then numEdges - 1 else numEdges
      writeMutVar levels L {allEdges = newAllEdges, numEdges = newNumEdges, ..}
  where
    go :: VM.MVector s (ET.Forest t (Sum Int) s v, EdgeSet v) -> Int -> m Bool
    go unLevels idx = do
      -- traceShowM ("go", idx)
      (etf, nonTreeEdges0) <- VM.read unLevels idx
      cutResult <- ET.deleteEdge etf a b
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
                            ET.insertEdge incEtf x y

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
              _ <- ET.insertEdge etf c d
              propagateReplacement unLevels (idx - 1) rep
              return True

    propagateReplacement unLevels idx (c, d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
      _ <- ET.deleteEdge etf a b
      _ <- ET.insertEdge etf c d
      -- TODO: mess with edges??
      propagateReplacement unLevels (idx - 1) (c, d)

insertVertex
    :: (Eq v, Hashable v, Tree t, PrimMonad m) => Graph t (PrimState m) v -> v -> m ()
insertVertex (Graph g) x = do
    l@L {..} <- readMutVar g
    let newAllEdges   = HMS.insertWith HS.union x HS.empty allEdges
        updateLevel i
            | i >= VM.length unLevels = return ()
            | otherwise               = do
                (forest, nt) <- VM.read unLevels i
                ET.insertVertex forest x
                VM.write unLevels i (forest, nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar g $ l {allEdges = newAllEdges}

deleteVertex
    :: (Eq v, Hashable v, Tree t, PrimMonad m) => Graph t (PrimState m) v -> v -> m ()
deleteVertex g@(Graph levels) x = do
    l0 <- readMutVar levels
    let neighbours = fromMaybe HS.empty (HMS.lookup x (allEdges l0))
    forM_ neighbours $ \y -> deleteEdge g x y

    l1 <- readMutVar levels
    let newAllEdges = HMS.delete x (allEdges l1)
        updateLevel i
            | i >= VM.length (unLevels l1) = return ()
            | otherwise                    = do
                (forest, nt) <- VM.read (unLevels l1) i
                ET.deleteVertex forest x
                VM.write (unLevels l1) i (forest, HMS.delete x nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar levels $ l1 {allEdges = newAllEdges}
