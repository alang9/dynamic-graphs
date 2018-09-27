{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MTree.EulerTour where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Hashable                 (Hashable)
import           Data.List
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe
import           Data.Monoid
import qualified Data.MTree.FastAvl            as FastAvl
import qualified Data.MTree.Internal.HashTable as HT
import qualified Data.Tree                     as Tree

newtype EulerTourForest s v = ETF
    { unETF :: HT.HashTable s (v, v) (FastAvl.Tree s (v, v) (Sum Int))
    }

empty :: (PrimMonad m, s ~ PrimState m) => m (EulerTourForest s v)
empty = ETF <$> HT.new

-- values in nodes must be unique
fromTree
    :: forall v m s. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Tree.Tree v -> m (EulerTourForest s v)
fromTree tree = do
    etf@(ETF ht) <- empty
    _ <- go ht tree
    return etf
  where
    go ht (Tree.Node l children) = do
      node0 <- FastAvl.singleton (l, l) (Sum 1)
      HT.insert ht (l, l) node0
      foldM (go' ht l) node0 children

    go' ht parent node0 tr@(Tree.Node l _) = do
      lnode     <- go ht tr
      parentToL <- FastAvl.singleton (parent, l) (Sum 0)
      lToParent <- FastAvl.singleton (l, parent) (Sum 0)

      node1 <- FastAvl.concat $ node0 NonEmpty.:| [parentToL, lnode, lToParent]
      HT.insert ht (l, parent) lToParent
      HT.insert ht (parent, l) parentToL
      return node1

findRoot
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => v -> EulerTourForest s v -> m (Maybe (FastAvl.Tree s (v, v) (Sum Int)))
findRoot v (ETF ht) = do
    mbTree <- HT.lookup ht (v, v)
    case mbTree of
        Nothing -> return Nothing
        Just t  -> Just <$> FastAvl.root t

cut
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => v -> v -> EulerTourForest s v -> m Bool
cut a b (ETF ht) = do
  mbAb <- HT.lookup ht (a, b)
  mbBa <- HT.lookup ht (b, a)
  case (mbAb, mbBa) of
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
      HT.delete ht (a, b)
      HT.delete ht (b, a)
      return True

    (Nothing, _) -> return False -- No edge to cut
    (_, Nothing) -> return False -- No edge to cut

-- | reroot the represented tree by shifting the euler tour.  Returns the new
-- root.
reroot
    :: (PrimMonad m, s ~ PrimState m, Monoid v)
    => FastAvl.Tree s a v -> m (FastAvl.Tree s a v)
reroot t = do
    (mbPre, mbPost) <- FastAvl.split t
    FastAvl.concat $ t NonEmpty.:| catMaybes [mbPost, mbPre]

hasEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => v -> v -> EulerTourForest s v -> m Bool
hasEdge a b (ETF ht) = isJust <$> HT.lookup ht (a, b)

connected
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => v -> v -> EulerTourForest s v -> m (Maybe Bool)
connected a b (ETF ht) = do
  mbALoop <- HT.lookup ht (a, a)
  mbBLoop <- HT.lookup ht (b, b)
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Just <$> FastAvl.connected aLoop bLoop
    _                        -> return Nothing

link
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => v -> v -> EulerTourForest s v -> m Bool
link a b (ETF ht) = do
  mbALoop <- HT.lookup ht (a, a)
  mbBLoop <- HT.lookup ht (b, b)
  case (mbALoop, mbBLoop) of
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

          HT.insert ht (a, b) abNode
          HT.insert ht (b, a) baNode
          return True

    _ -> return False

showEtf :: Show a => EulerTourForest RealWorld a -> IO ()
showEtf (ETF ht) = do
  trees <- map snd <$> HT.toList ht
  roots <- mapM FastAvl.root trees
  forM_ (nub roots) $ \root -> do
    FastAvl.print root
    putStrLn ""

discreteForest
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => [v] -> m (EulerTourForest s v)
discreteForest vs = do
    etf@(ETF ht) <- empty
    forM_ vs $ \v -> do
        node <- FastAvl.singleton (v, v) (Sum 1)
        HT.insert ht (v, v) node
    return etf

componentSize
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => v -> EulerTourForest s v -> m Int
componentSize v (ETF ht) = do
  mbTree <- HT.lookup ht (v, v)
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- FastAvl.root tree
      getSum <$> FastAvl.aggregate root
