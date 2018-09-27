{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MGraph where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Primitive.MutVar
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector.Mutable as VM
import Data.Hashable (Hashable)

import qualified Data.MTree.Avl as Avl
import qualified Data.MTree.Splay as Splay
import qualified Data.MTree.FastAvl as FastAvl
import qualified Data.MTree.EulerTour as ET
import Data.MTree.EulerTour (EulerTourForest)

import Debug.Trace

data L s v = L
  { vertices :: Set v
  , allEdges :: !(Set (v, v))
  , unLevels :: !(VM.MVector s (EulerTourForest s v, Map v (Set v)))
  }

type Levels s v = MutVar s (L s v)

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

fromVertices :: (PrimMonad m, s ~ PrimState m, Ord v) => [v] -> m (Levels s v)
fromVertices xs = newMutVar =<< L (Set.fromList xs) Set.empty <$> VM.new 0

-- TODO (jaspervdj): Kill Ord constraints in this module
insert :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m ()
insert a b levels = do --traceShow (numEdges, VM.length unLevels, Set.member (a, b) allEdges) $
  L {..} <- readMutVar levels
  let newAllEdges = if a == b then allEdges else Set.insert (b, a) $ Set.insert (a, b) allEdges
      numEdges = Set.size newAllEdges `div` 2
  if Set.member (a, b) allEdges
    then return ()
    else do
      unLevels' <- do
        let oldNumLevels = VM.length unLevels
        newUnLevels <- VM.take (logBase2 numEdges + 1) <$>
          VM.grow unLevels (max 0 $ logBase2 numEdges - oldNumLevels + 1)
        forM_ [oldNumLevels .. logBase2 numEdges] $ \levelIdx -> do
          df <- ET.discreteForest $ Set.toList vertices
          VM.write newUnLevels levelIdx (df, Map.empty)
        return newUnLevels
      -- traceShowM (VM.null levels')
      if VM.null unLevels'
        then return ()
        else do
          (thisEtf, thisEdges) <- VM.read unLevels' 0
          m'newEtf <- ET.link a b thisEtf
          -- traceShowM $ (numEdges, m'newEtf)
          -- traceShowM $ (numEdges, "test3")
          let newEdges = Map.insertWith Set.union a (Set.singleton b) $
                Map.insertWith Set.union b (Set.singleton a) thisEdges
          VM.write unLevels' 0 (thisEtf, newEdges)
          writeMutVar levels $ L {allEdges = newAllEdges, unLevels = unLevels', ..}

connected :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m (Maybe Bool)
connected a b levels = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return $ Just $ a == b
    else do
      (etf, _) <- VM.read unLevels 0
      ET.connected a b etf

delete :: forall m s v. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m ()
delete a b levels = do
  L {..} <- readMutVar levels
  let newAllEdges = Set.delete (a, b) $ Set.delete (b, a) allEdges
  -- | a == b = return Levels {..}
  if VM.length unLevels == 0
    then return ()
    else do
      go unLevels (VM.length unLevels-1)
      writeMutVar levels L {allEdges = newAllEdges, ..}
  where
    go :: VM.MVector s (EulerTourForest s v, Map v (Set v)) -> Int -> m ()
    go unLevels idx = do
      -- traceShowM ("go", idx)
      (etf, edges) <- VM.read unLevels idx
      cutResult <- ET.cut a b etf
      let edges' = Map.adjust (Set.delete b) a $ Map.adjust (Set.delete a) b edges
      case cutResult of
        False -> do
          VM.write unLevels idx (etf, edges')
          when (idx > 0) $ go unLevels (idx - 1)
        True -> do
          aSize <- ET.componentSize a etf
          bSize <- ET.componentSize b etf
          let (smaller, bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot smaller etf
          sEdges <- FastAvl.toList sRoot
          (edges'', mPrevEdges) <- do
            if not (idx + 1 < VM.length unLevels)
              then return (edges', Nothing)
              else do
                (prevEtf, prevEdges) <- VM.read unLevels (idx + 1)
                let go' (oldPrevEdges, oldEdges) (c, d) = do
                      ET.link c d prevEtf
                      return ( Map.insertWith Set.union d (Set.singleton c) (Map.insertWith Set.union c (Set.singleton d) oldPrevEdges)
                             , Map.adjust (Set.delete c) d (Map.adjust (Set.delete d) c oldEdges)
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
            Nothing -> when (idx > 0) $ go unLevels (idx - 1)
            Just rep -> propagateReplacement unLevels (idx - 1) rep

    propagateReplacement unLevels idx (c, d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
      ET.cut a b etf
      ET.link c d etf
      propagateReplacement unLevels (idx - 1) (c, d)

    findReplacement ::
      EulerTourForest s v -> Map v (Set v) -> Maybe (Map v (Set v)) ->
      v -> [v] ->
      m (Maybe (v, v), Map v (Set v), Maybe (Map v (Set v)))
    findReplacement _ remainingEdges m'prevEdges _ [] = return (Nothing, remainingEdges, m'prevEdges)
    findReplacement f remainingEdges m'prevEdges other (x:xs) = case Set.minView xEdges of
      Nothing -> findReplacement f remainingEdges m'prevEdges other xs
      Just (c, _) -> do
        cConnected <- ET.connected c other f
        if maybe err id cConnected
          then do
            True <- ET.link c x f
            return (Just (c, x), remainingEdges, m'prevEdges)
          else
            findReplacement f
              (Map.adjust (Set.delete c) x $ Map.adjust (Set.delete x) c $ remainingEdges)
              (Map.insertWith Set.union x (Set.singleton c) . Map.insertWith Set.union c (Set.singleton x) <$> m'prevEdges)
              other (x:xs)
      where
        xEdges = maybe Set.empty id $ Map.lookup x remainingEdges
        err = error "delete.findReplacement: invalid state"

