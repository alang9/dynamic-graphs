{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MGraph where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector.Mutable as VM

import qualified Data.MTree.Avl as Avl
import qualified Data.MTree.EulerTour as ET
import Data.MTree.EulerTour (EulerTourForest)

import Debug.Trace

data Levels s v = Levels
  { vertices :: Set v
  , allEdges :: !(Set (v, v))
  , unLevels :: !(VM.MVector s (EulerTourForest s v, Map v (Set v)))
  }

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

fromVertices :: (PrimMonad m, s ~ PrimState m, Ord v) => [v] -> m (Levels s v)
fromVertices xs = Levels (Set.fromList xs) Set.empty <$> VM.new 0

insert :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m (Levels s v)
insert a b levels@Levels {..} = --traceShow (numEdges, VM.length unLevels, Set.member (a, b) allEdges) $
  if Set.member (a, b) allEdges
    then return Levels {..}
    else do
      levels' <- do
        let oldNumLevels = VM.length unLevels
        newUnLevels <- VM.take (logBase2 numEdges + 1) <$>
          VM.grow unLevels (max 0 $ logBase2 numEdges - oldNumLevels + 1)
        forM_ [oldNumLevels .. logBase2 numEdges] $ \levelIdx -> do
          df <- ET.discreteForest $ Set.toList vertices
          VM.write newUnLevels levelIdx (df, Map.empty)
        return newUnLevels
      -- traceShowM (VM.null levels')
      if VM.null levels'
        then return levels
        else do
          (thisEtf, thisEdges) <- VM.read levels' 0
          m'newEtf <- ET.link a b thisEtf
          -- traceShowM $ (numEdges, m'newEtf)
          -- traceShowM $ (numEdges, "test3")
          let newEdges = Map.insertWith Set.union a (Set.singleton b) $
                Map.insertWith Set.union b (Set.singleton a) thisEdges
          VM.write levels' 0 (thisEtf, newEdges)
          return Levels {allEdges = newAllEdges, unLevels = levels', ..}
  where
    numEdges = Set.size newAllEdges `div` 2
    newAllEdges = if a == b then allEdges else Set.insert (b, a) $ Set.insert (a, b) allEdges

connected :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m (Maybe Bool)
connected a b Levels {..} = if VM.null unLevels
  then return $ Just $ a == b
  else do
    (etf, _) <- VM.read unLevels 0
    ET.connected a b etf

delete :: forall m s v. (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m (Levels s v)
delete a b Levels {..}
  -- | a == b = return Levels {..}
  | VM.length unLevels == 0 = return Levels {..}
  | otherwise = do
      go (VM.length unLevels-1)
      return Levels {allEdges = newAllEdges, ..}
  where
    newAllEdges = Set.delete (a, b) $ Set.delete (b, a) allEdges
    go :: Int -> m ()
    go idx = do
      -- traceShowM ("go", idx)
      (etf, edges) <- VM.read unLevels idx
      cutResult <- ET.cut a b etf
      let edges' = Map.adjust (Set.delete b) a $ Map.adjust (Set.delete a) b edges
      case cutResult of
        False -> do
          VM.write unLevels idx (etf, edges')
          when (idx > 0) $ go (idx - 1)
        True -> do
          aSize <- ET.componentSize a etf
          bSize <- ET.componentSize b etf
          let (smaller, bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot smaller etf
          sEdges <- Avl.toList sRoot
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
            Nothing -> when (idx > 0) $ go (idx - 1)
            Just rep -> propagateReplacement (idx - 1) rep

    propagateReplacement idx (c, d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
      ET.cut a b etf
      ET.link c d etf
      propagateReplacement (idx - 1) (c, d)

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

