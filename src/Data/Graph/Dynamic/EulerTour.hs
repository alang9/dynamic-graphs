-- | This module provides dynamic connectivity for an acyclic graph (i.e. a
-- forest).
--
-- It is based on:
-- /Finding biconnected components and computing tree functions in logarithmic
-- parallel time/ by /Robert E. Tarjan and Uzi Vishki/ (1984).
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dynamic.EulerTour
    ( -- * Type
      Forest

      -- * Construction
    , empty
    , fromTree
    , discreteForest

      -- * Queries
    , connected
    , hasEdge

      -- * Modifying
    , insertEdge
    , deleteEdge
    , insertVertex
    , deleteVertex

      -- * Advanced/internal operations
    , findRoot
    , componentSize

      -- * Debugging
    , print
    ) where

import           Control.Monad                         (foldM, forM_)
import           Control.Monad.Primitive
import qualified Data.Graph.Dynamic.Internal.HashTable as HT
import qualified Data.Graph.Dynamic.Internal.Splay     as Splay
import           Data.Hashable                         (Hashable)
import qualified Data.HashMap.Strict                   as HMS
import           Data.List
import qualified Data.List.NonEmpty                    as NonEmpty
import           Data.Maybe
import           Data.Monoid
import qualified Data.Tree                             as Tree
import           Prelude                               hiding (print)

newtype Forest s v = ETF
    { _unETF :: HT.HashTable s v (HMS.HashMap v (Splay.Tree s (v, v) (Sum Int)))
    }

insertTree
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> Splay.Tree s (v, v) (Sum Int) -> m ()
insertTree (ETF ht) x y t = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> HT.insert ht x $ HMS.singleton y t
        Just m  -> HT.insert ht x $ HMS.insert y t m

lookupTree
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> m (Maybe (Splay.Tree s (v, v) (Sum Int)))
lookupTree (ETF ht) x y = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return Nothing
        Just m  -> return $ HMS.lookup y m

deleteTree
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> m ()
deleteTree (ETF ht) x y = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return ()
        Just m0 ->
            let m1 = HMS.delete y m0 in
            if HMS.null m1 then HT.delete ht x else HT.insert ht x m1

empty :: (PrimMonad m, s ~ PrimState m) => m (Forest s v)
empty = ETF <$> HT.new

-- values in nodes must be unique
fromTree
    :: forall v m s. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Tree.Tree v -> m (Forest s v)
fromTree tree = do
    etf <- empty
    _ <- go etf tree
    return etf
  where
    go etf (Tree.Node l children) = do
      node0 <- Splay.singleton (l, l) (Sum 1)
      insertTree etf l l node0
      foldM (go' etf l) node0 children

    go' etf parent node0 tr@(Tree.Node l _) = do
      lnode     <- go etf tr
      parentToL <- Splay.singleton (parent, l) (Sum 0)
      lToParent <- Splay.singleton (l, parent) (Sum 0)

      node1 <- Splay.concat $ node0 NonEmpty.:| [parentToL, lnode, lToParent]
      insertTree etf l parent lToParent
      insertTree etf parent l parentToL
      return node1

discreteForest
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => [v] -> m (Forest s v)
discreteForest vs = do
    etf <- empty
    forM_ vs $ \v -> do
        node <- Splay.singleton (v, v) (Sum 1)
        insertTree etf v v node
    return etf

findRoot
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> m (Maybe (Splay.Tree s (v, v) (Sum Int)))
findRoot etf v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Nothing -> return Nothing
        Just t  -> Just <$> Splay.root t

deleteEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> m Bool
deleteEdge etf a b = do
  mbAb <- lookupTree etf a b
  mbBa <- lookupTree etf b a
  case (mbAb, mbBa) of
    _ | a == b -> return False -- Can't cut self-loops
    (Just ab, Just ba) -> do
      (part1, part2) <- Splay.split ab

      baIsInPart1 <- case part1 of
        Just p -> Splay.connected p ba
        _      -> return False

      (mbL, _, mbR) <- if baIsInPart1 then do
        (part3, part4) <- Splay.split ba
        return (part3, part4, part2)
      else do
        (part3, part4) <- Splay.split ba
        return (part1, part3, part4)

      _ <- sequenceA $ Splay.append <$> mbL <*> mbR
      deleteTree etf a b
      deleteTree etf b a
      return True

    (Nothing, _) -> return False -- No edge to cut
    (_, Nothing) -> return False -- No edge to cut

-- | reroot the represented tree by shifting the euler tour.  Returns the new
-- root.
reroot
    :: (PrimMonad m, s ~ PrimState m, Monoid v)
    => Splay.Tree s a v -> m (Splay.Tree s a v)
reroot t = do
    (mbPre, mbPost) <- Splay.split t
    Splay.concat $ t NonEmpty.:| catMaybes [mbPost, mbPre]

hasEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> m Bool
hasEdge etf a b = isJust <$> lookupTree etf a b

connected
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> m (Maybe Bool)
connected etf a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Just <$> Splay.connected aLoop bLoop
    _                        -> return Nothing

insertEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> v -> m Bool
insertEdge etf a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Splay.connected aLoop bLoop >>= \case
        True -> return False
        False -> do

          bLoop1            <- reroot bLoop
          abNode            <- Splay.singleton (a, b) (Sum 0)
          baNode            <- Splay.singleton (b, a) (Sum 0)
          (mbPreA, mbPostA) <- Splay.split aLoop

          _ <- Splay.concat $
            aLoop NonEmpty.:| catMaybes
            [ Just abNode
            , Just bLoop1
            , Just baNode
            , mbPostA
            , mbPreA
            ]

          insertTree etf a b abNode
          insertTree etf b a baNode
          return True

    _ -> return False

insertVertex
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> m ()
insertVertex etf v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Just  _ -> return ()  -- It's already there
        Nothing -> do
            node <- Splay.singleton (v, v) (Sum 1)
            insertTree etf v v node

neighbours
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> m [v]
neighbours (ETF ht) x = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return []
        Just m  -> return $ filter (/= x) $ map fst $ HMS.toList m

deleteVertex
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> m ()
deleteVertex etf x = do
    nbs <- neighbours etf x
    forM_ nbs $ \y -> deleteEdge etf x y
    deleteTree etf x x

print :: Show a => Forest RealWorld a -> IO ()
print (ETF ht) = do
  maps <- map snd <$> HT.toList ht
  let trees = concatMap (map snd . HMS.toList) maps
  roots <- mapM Splay.root trees
  forM_ (nub roots) $ \root -> do
    Splay.print root
    putStrLn ""

componentSize
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest s v -> v -> m Int
componentSize etf v = do
  mbTree <- lookupTree etf v v
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- Splay.root tree
      getSum <$> Splay.aggregate root
