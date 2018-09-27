{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.MTree.EulerTour where

import Control.Monad
import Control.Monad.Primitive
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.List.NonEmpty as NonEmpty
import Data.Primitive.MutVar
import qualified Data.Tree as Tree
import qualified Data.MTree.Splay as Splay
import qualified Data.MTree.FastAvl as FastAvl

type EulerTourForest s v = MutVar s (ETF s v)

newtype ETF s v = ETF {etf :: Map.Map (v, v) (FastAvl.Tree s (v, v) (Sum Int))}

empty :: (PrimMonad m, s ~ PrimState m) => m (EulerTourForest s v)
empty = newMutVar $ ETF Map.empty

-- values in nodes must be unique
fromTree :: (PrimMonad m, s ~ PrimState m, Ord v) => Tree.Tree v -> m (EulerTourForest s v)
fromTree tree = do
  m <- ETF . snd <$> go Map.empty tree
  newMutVar m
  where
    go m0 (Tree.Node l children) = do
      node0 <- FastAvl.singleton (l, l) (Sum 1)
      let m1 = Map.insert (l, l) node0 m0
      (node1, m2) <- foldM (go' l) (node0, m1) children
      return (node1, m2)

    go' parent (node0, m0) tr@(Tree.Node l _) = do
      (lnode, m1) <- go m0 tr
      parentToL   <- FastAvl.singleton (parent, l) (Sum 0)
      lToParent   <- FastAvl.singleton (l, parent) (Sum 0)

      node1 <- FastAvl.concat $ node0 NonEmpty.:| [parentToL, lnode, lToParent]
      let m2 = Map.insert (l, parent) lToParent $ Map.insert (parent, l) parentToL m1
      return (node1, m2)

findRoot
    :: (PrimMonad m, s ~ PrimState m, Ord v)
    => v -> EulerTourForest s v -> m (Maybe (FastAvl.Tree s (v, v) (Sum Int)))
findRoot v f = do
  ETF m <- readMutVar f
  sequenceA $ FastAvl.root <$> Map.lookup (v, v) m

cut :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> m Bool
cut a b f = do
  ETF m <- readMutVar f
  case (Map.lookup (a, b) m, Map.lookup (b, a) m) of
    _ | a == b -> return False -- Can't cut self-loops
    (Just ab, Just ba) -> do
      (part1, part2) <- FastAvl.split ab

      baIsInPart1 <- case part1 of
        Just p -> FastAvl.connected p ba
        _      -> return False

      (mbL, _, mbR) <- if baIsInPart1 then do
        (part3, part4) <- FastAvl.split ba
        return (part3, part4, part2)
      else do
        (part3, part4) <- FastAvl.split ba
        return (part1, part3, part4)

      _ <- sequenceA $ FastAvl.append <$> mbL <*> mbR
      writeMutVar f $ ETF $ Map.delete (a, b) $ Map.delete (b, a) m
      return True

    (Nothing, _) -> return False -- No edge to cut
    (_, Nothing) -> return False -- No edge to cut

-- | reroot the represented tree by shifting the euler tour.  Returns the new
-- root.
reroot
    :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v)
    => FastAvl.Tree s a v -> m (FastAvl.Tree s a v)
reroot t = do
    (mbPre, mbPost) <- FastAvl.split t
    FastAvl.concat $ t NonEmpty.:| catMaybes [mbPost, mbPre]

hasEdge :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> m Bool
hasEdge a b f = do
  ETF m <- readMutVar f
  return $ isJust $ Map.lookup (a, b) m

connected :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> m (Maybe Bool)
connected a b f = do
  ETF m <- readMutVar f
  case (Map.lookup (a, a) m, Map.lookup (b, b) m) of
    (Just aLoop, Just bLoop) -> Just <$> FastAvl.connected aLoop bLoop
    _ -> return Nothing

link :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> m Bool
link a b f = do
  ETF m <- readMutVar f
  case (Map.lookup (a, a) m, Map.lookup (b, b) m) of
    (Just aLoop, Just bLoop) -> FastAvl.connected aLoop bLoop >>= \case
        True -> return False
        False -> do

          bLoop1            <- reroot bLoop
          abNode            <- FastAvl.singleton (a, b) (Sum 0)
          baNode            <- FastAvl.singleton (b, a) (Sum 0)
          (mbPreA, mbPostA) <- FastAvl.split aLoop

          _ <- FastAvl.concat $
            aLoop NonEmpty.:| catMaybes
            [ Just abNode
            , Just bLoop1
            , Just baNode
            , mbPostA
            , mbPreA
            ]

          writeMutVar f $ ETF $
            Map.insert (a, b) abNode $
            Map.insert (b, a) baNode $
            m

          return True

    _ -> return False

{-

tree1 :: Tree.Tree Int
tree1 = Tree.unfoldTree buildNode 1
  where
    buildNode x = if 2*x + 1 > 10 then (x, []) else (x, [2*x, 2*x+1])

-}

showEtf :: Show a => EulerTourForest RealWorld a -> IO ()
showEtf f = do
  ETF m <- readMutVar f
  roots <- mapM FastAvl.root $ Map.elems m
  forM_ (nub roots) $ \root -> do
    FastAvl.print root
    putStrLn ""

discreteForest :: (PrimMonad m, s ~ PrimState m, Ord v) => [v] -> m (EulerTourForest s v)
discreteForest vs = do
  m <- ETF <$> foldM go Map.empty vs
  newMutVar m
  where
    go m v = do
      node <- FastAvl.singleton (v, v) (Sum 1)
      return $ Map.insert (v, v) node m

componentSize :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> EulerTourForest s v -> m Int
componentSize v f = do
  ETF m <- readMutVar f
  case Map.lookup (v, v) m of
    Nothing -> return 0
    Just tree -> do
      root <- FastAvl.root tree
      getSum <$> FastAvl.aggregate root
