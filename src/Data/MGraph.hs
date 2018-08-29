{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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
insert a b levels@Levels {..} = traceShow (numEdges) $
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
      if VM.null levels'
        then return levels
        else do
          (thisEtf, thisEdges) <- VM.read levels' 0
          m'newEtf <- ET.link a b thisEtf
          traceShowM $ const () <$> m'newEtf
          let newEtf = maybe thisEtf id m'newEtf
              newEdges = Map.insertWith Set.union a (Set.singleton b) $
                Map.insertWith Set.union b (Set.singleton a) thisEdges
          VM.write levels' 0 (newEtf, newEdges)
          return Levels {allEdges = newAllEdges, unLevels = levels', ..}
  where
    numEdges = Set.size newAllEdges `div` 2
    newAllEdges = if a == b then allEdges else Set.insert (b, a) $ Set.insert (a, b) allEdges

connected :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m (Maybe Bool)
connected a b Levels {..} = if VM.null unLevels
  then return $ Just $ a == b
  else do
    (etf, _) <- VM.read unLevels 0
    sequenceA $ ET.connected a b etf

delete :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> Levels s v -> m (Levels s v)
delete a b Levels {..}
  -- | a == b = return Levels {..}
  | VM.length unLevels == 0 = return Levels {..}
  | otherwise = do
      go (VM.length unLevels-1)
      return Levels {allEdges = newAllEdges, ..}
  where
    newAllEdges = Set.delete (a, b) $ Set.delete (b, a) allEdges
    go idx = do
      (etf, edges) <- VM.read unLevels idx
      case ET.cut a b etf of
        Nothing -> VM.write unLevels idx (etf, edges)
        Just goEtf -> do
          etf' <- goEtf
          edges' <- do
            if not (idx + 1 < VM.length unLevels)
              then return edges
              else do
                Just aRoot <- sequenceA $ ET.findRoot a etf'
                Just bRoot <- sequenceA $ ET.findRoot b etf'
                aEdges <- Avl.toList aRoot
                bEdges <- Avl.toList bRoot
                (prevEtf, prevEdges) <- VM.read unLevels (idx + 1)
                let go' (oldPrevEtf, oldPrevEdges, oldEdges) (c, d) =
                      ET.link c d oldPrevEtf >>= \case
                        Nothing -> do
                          return ( oldPrevEtf
                                 , Map.adjust (Set.insert b) a (Map.adjust (Set.insert a) b oldPrevEdges)
                                 , Map.adjust (Set.delete b) a (Map.adjust (Set.delete a) b oldEdges)
                                 )
                        Just newEtf -> do
                          return ( newEtf
                                 , Map.adjust (Set.insert b) a (Map.adjust (Set.insert a) b oldPrevEdges)
                                 , Map.adjust (Set.delete b) a (Map.adjust (Set.delete a) b oldEdges)
                                 )
                (newPrevEtf, newPrevEdges, newEdges) <- foldM go' (prevEtf, prevEdges, edges) (aEdges ++ bEdges)
                VM.write unLevels (idx + 1) (newPrevEtf, newPrevEdges)
                return newEdges
          VM.write unLevels idx (etf', Map.adjust (Set.delete b) a $ Map.adjust (Set.delete a) b edges')
      when (idx > 0) $ go (idx - 1)
