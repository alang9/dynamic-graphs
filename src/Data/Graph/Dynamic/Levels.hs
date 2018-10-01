-- | This module implements full dynamic grah connectivity.
--
-- It is based on:
-- /Poly-logarithmic deterministic fully-dynamic algorithms for connectivity,
-- minimum spanning tree, 2-edge, and biconnectivity/ by /Jacob Holm, Kristian
-- de Lichtenberg and Mikkel Thorup/ (1998).
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dynamic.Levels
    ( -- * Type
      Graph

      -- * Construction
    , new
    , fromVertices

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
import           Data.Hashable                     (Hashable)
import qualified Data.HashMap.Strict               as HMS
import qualified Data.HashSet                      as HS
import           Data.Maybe                        (fromMaybe)
import           Data.Primitive.MutVar
import qualified Data.Vector.Mutable               as VM

import qualified Data.Graph.Dynamic.EulerTour      as ET
import qualified Data.Graph.Dynamic.Internal.Splay as Splay

data L s v = L
  { numEdges :: !Int
  , allEdges :: !(HMS.HashMap v (HS.HashSet v))
  , unLevels :: !(VM.MVector s (ET.Forest s v, HMS.HashMap v (HS.HashSet v)))
  }

newtype Graph s v = Graph (MutVar s (L s v))

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

new :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => m (Graph s v)
new = fromVertices []

fromVertices
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => [v] -> m (Graph s v)
fromVertices xs = do
  unLevels <- VM.new 0
  let allEdges = HMS.fromList $ zip xs $ repeat HS.empty
      numEdges = 0
  Graph <$> newMutVar L {..}

-- TODO (jaspervdj): Kill Ord constraints in this module
insertEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m ()
insertEdge (Graph levels) a b = do --traceShow (numEdges, VM.length unLevels, HS.member (a, b) allEdges) $
  L {..} <- readMutVar levels
  let newAllEdges =
            HMS.insertWith HS.union a (HS.singleton b) $
            HMS.insertWith HS.union b (HS.singleton a) $
            allEdges

      newNumEdges = numEdges + 1

  if maybe False (b `HS.member`) (HMS.lookup a allEdges) || a == b
    then return ()
    else do
      unLevels' <- do
        let oldNumLevels = VM.length unLevels
        newUnLevels <- VM.take (logBase2 newNumEdges + 1) <$>
          VM.grow unLevels (max 0 $ logBase2 newNumEdges - oldNumLevels + 1)
        forM_ [oldNumLevels .. logBase2 newNumEdges] $ \levelIdx -> do
          df <- ET.discreteForest $ map fst $ HMS.toList allEdges
          VM.write newUnLevels levelIdx (df, HMS.empty)
        return newUnLevels
      -- traceShowM (VM.null levels')
      if VM.null unLevels'
        then return ()
        else do
          (thisEtf, thisEdges) <- VM.read unLevels' 0
          _m'newEtf <- ET.insertEdge thisEtf a b
          -- traceShowM $ (newNumEdges, m'newEtf)
          -- traceShowM $ (newNumEdges, "test3")
          let newEdges = HMS.insertWith HS.union a (HS.singleton b) $
                HMS.insertWith HS.union b (HS.singleton a) thisEdges
          VM.write unLevels' 0 (thisEtf, newEdges)
          writeMutVar levels $ L
              {allEdges = newAllEdges, unLevels = unLevels',numEdges = newNumEdges}

connected :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m (Maybe Bool)
connected _ a b | a == b = return (Just True)
connected (Graph levels) a b = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return (Just False)
    else do
      (etf, _) <- VM.read unLevels 0
      ET.connected etf a b

hasEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m Bool
hasEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  return $ maybe False (b `HS.member`) (HMS.lookup a allEdges)

deleteEdge :: forall m s v. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m ()
deleteEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  let newAllEdges = HMS.adjust (HS.delete a) b $ HMS.adjust (HS.delete b) a allEdges
  -- | a == b = return Graph {..}
  if VM.length unLevels == 0 || a == b
    then return ()
    else do
      cut <- go unLevels (VM.length unLevels-1)
      let newNumEdges = if cut then numEdges - 1 else numEdges
      writeMutVar levels L {allEdges = newAllEdges, numEdges = newNumEdges, ..}
  where
    go :: VM.MVector s (ET.Forest s v, HMS.HashMap v (HS.HashSet v)) -> Int -> m Bool
    go unLevels idx = do
      -- traceShowM ("go", idx)
      (etf, edges) <- VM.read unLevels idx
      cutResult <- ET.deleteEdge etf a b
      let edges' = HMS.adjust (HS.delete b) a $ HMS.adjust (HS.delete a) b edges
      case cutResult of
        False -> do
          VM.write unLevels idx (etf, edges')
          if idx > 0 then go unLevels (idx - 1) else return False
        True -> do
          aSize <- ET.componentSize etf a
          bSize <- ET.componentSize etf b
          let (smaller, bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot etf smaller
          sEdges <- Splay.toList sRoot
          (edges'', mPrevEdges) <- do
            if not (idx + 1 < VM.length unLevels)
              then return (edges', Nothing)
              else do
                (prevEtf, prevEdges) <- VM.read unLevels (idx + 1)
                let go' (oldPrevEdges, oldEdges) (c, d) = do
                      _ <- ET.insertEdge prevEtf c d
                      return ( HMS.insertWith HS.union d (HS.singleton c) (HMS.insertWith HS.union c (HS.singleton d) oldPrevEdges)
                             , HMS.adjust (HS.delete c) d (HMS.adjust (HS.delete d) c oldEdges)
                             )
                (newPrevEdges, newEdges) <- foldM go' (prevEdges, edges') sEdges
                VM.write unLevels (idx + 1) (prevEtf, newPrevEdges)
                return (newEdges, Just newPrevEdges)
          let sVertices = map fst $ filter (uncurry (==)) sEdges
          (replacementEdge, newEdges, m'newPrevEdges) <- findReplacement etf edges'' mPrevEdges bigger sVertices
          -- traceShowM ("delete", idx, VM.length unLevels, edges, edges'', newEdges, bigger, sVertices)
          -- traceShowM ("delete", idx)
          VM.write unLevels idx (etf, newEdges)
          case m'newPrevEdges of
            Nothing -> return ()
            Just newPrevEdges -> VM.modify unLevels (\(prevEtf, _) -> (prevEtf, newPrevEdges)) (idx + 1)
          case replacementEdge of
            Nothing  -> if idx > 0 then go unLevels (idx - 1) else return True
            Just rep -> propagateReplacement unLevels (idx - 1) rep >> return True

    propagateReplacement unLevels idx (c, d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
      _ <- ET.deleteEdge etf a b
      _ <- ET.insertEdge etf c d
      propagateReplacement unLevels (idx - 1) (c, d)

    findReplacement ::
      ET.Forest s v -> HMS.HashMap v (HS.HashSet v) -> Maybe (HMS.HashMap v (HS.HashSet v)) ->
      v -> [v] ->
      m (Maybe (v, v), HMS.HashMap v (HS.HashSet v), Maybe (HMS.HashMap v (HS.HashSet v)))
    findReplacement _ remainingEdges m'prevEdges _ [] = return (Nothing, remainingEdges, m'prevEdges)
    findReplacement f remainingEdges m'prevEdges other (x:xs) = case HS.toList xEdges of
      -- HS.toList produces a lazy list so this is all okay.
      [] -> findReplacement f remainingEdges m'prevEdges other xs
      (c : _) -> do
        cConnected <- ET.connected f c other
        if maybe err id cConnected
          then do
            True <- ET.insertEdge f c x
            return (Just (c, x), remainingEdges, m'prevEdges)
          else
            findReplacement f
              (HMS.adjust (HS.delete c) x $ HMS.adjust (HS.delete x) c $ remainingEdges)
              (HMS.insertWith HS.union x (HS.singleton c) . HMS.insertWith HS.union c (HS.singleton x) <$> m'prevEdges)
              other (x:xs)
      where
        xEdges = maybe HS.empty id $ HMS.lookup x remainingEdges
        err = error "delete.findReplacement: invalid state"

insertVertex
    :: (Eq v, Hashable v, PrimMonad m) => Graph (PrimState m) v -> v -> m ()
insertVertex (Graph g) x = do
    l@L {..} <- readMutVar g
    let newAllEdges   = HMS.insertWith HS.union x HS.empty allEdges
        updateLevel i
            | i >= VM.length unLevels = return ()
            | otherwise               = do
                (forest, m) <- VM.read unLevels i
                ET.insertVertex forest x
                VM.write unLevels i (forest, HMS.insertWith HS.union x HS.empty m)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar g $ l {allEdges = newAllEdges}

deleteVertex
    :: (Eq v, Hashable v, PrimMonad m) => Graph (PrimState m) v -> v -> m ()
deleteVertex g@(Graph levels) x = do
    l0 <- readMutVar levels
    let neighbours = fromMaybe HS.empty (HMS.lookup x (allEdges l0))
    forM_ neighbours $ \y -> deleteEdge g x y

    l1 <- readMutVar levels
    let newAllEdges = HMS.delete x (allEdges l1)
        updateLevel i
            | i >= VM.length (unLevels l1) = return ()
            | otherwise                    = do
                (forest, m) <- VM.read (unLevels l1) i
                ET.deleteVertex forest x
                VM.write (unLevels l1) i (forest, HMS.delete x m)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar levels $ l1 {allEdges = newAllEdges}
