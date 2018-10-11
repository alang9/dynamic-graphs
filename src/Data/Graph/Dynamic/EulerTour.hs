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
    , Graph
    , Graph'

      -- * Construction
    , new
    , fromTree
    , discreteForest
    , discreteForest'

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

import           Control.Monad                         (filterM, foldM, forM_)
import           Control.Monad.Primitive
import qualified Data.Graph.Dynamic.Internal.HashTable as HT
import qualified Data.Graph.Dynamic.Internal.Tree      as Tree
import qualified Data.Graph.Dynamic.Internal.Random      as Random
import           Data.Hashable                         (Hashable)
import qualified Data.HashMap.Strict                   as HMS
import           Data.List
import qualified Data.List.NonEmpty                    as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy                            (Proxy (..))
import qualified Data.Tree                             as DT
import           Prelude                               hiding (print)

data Forest t a s v = ETF
    { edges :: {-# UNPACK#-} !(HT.HashTable s v (HMS.HashMap v (t s (v, v) a)))
    , toMonoid :: v -> v -> a
    , treeGen :: (Tree.TreeGen t s)
    }

type Graph t = Forest t ()

type Graph' = Graph Random.Tree

insertTree
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t a s v -> v -> v -> t s (v, v) a -> m ()
insertTree (ETF ht _ _) x y t = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> HT.insert ht x $ HMS.singleton y t
        Just m  -> HT.insert ht x $ HMS.insert y t m

lookupTree
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t a s v -> v -> v -> m (Maybe (t s (v, v) (a)))
lookupTree (ETF ht _ _) x y = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return Nothing
        Just m  -> return $ HMS.lookup y m

deleteTree
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t a s v -> v -> v -> m ()
deleteTree (ETF ht _ _) x y = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return ()
        Just m0 ->
            let m1 = HMS.delete y m0 in
            if HMS.null m1 then HT.delete ht x else HT.insert ht x m1

new :: forall t m s v a. (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => (v -> v -> a) -> m (Forest t a s v)
new f = do
  ht <- HT.new
  tg <- Tree.newTreeGen (Proxy :: Proxy t)
  return $ ETF ht f tg

-- values in nodes must be unique
fromTree
    :: forall v m t s a. (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => (v -> v -> a) -> DT.Tree v -> m (Forest t a s v)
fromTree toMonoid tree = do
    etf <- new toMonoid
    _ <- go etf tree
    return etf
  where
    go etf (DT.Node l children) = do
      node0 <- Tree.singleton (treeGen etf) (l, l) (toMonoid l l)
      insertTree etf l l node0
      foldM (go' etf l) node0 children

    go' etf parent node0 tr@(DT.Node l _) = do
      lnode     <- go etf tr
      parentToL <- Tree.singleton (treeGen etf) (parent, l) (toMonoid parent l)
      lToParent <- Tree.singleton (treeGen etf) (l, parent) (toMonoid l parent)

      node1 <- Tree.concat $ node0 NonEmpty.:| [parentToL, lnode, lToParent]
      insertTree etf l parent lToParent
      insertTree etf parent l parentToL
      return node1

discreteForest
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => (v -> v -> a) -> [v] -> m (Forest t a s v)
discreteForest toMonoid vs = do
    etf <- new toMonoid
    forM_ vs $ \v -> do
        node <- Tree.singleton (treeGen etf) (v, v) (toMonoid v v)
        insertTree etf v v node
    return etf

discreteForest'
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => (v -> v -> a) -> [v] -> m (Forest Random.Tree a s v)
discreteForest' = discreteForest

findRoot
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> m (Maybe (t s (v, v) a))
findRoot etf v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Nothing -> return Nothing
        Just t  -> Just <$> Tree.root t

deleteEdge
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> v -> m Bool
deleteEdge etf a b = do
  mbAb <- lookupTree etf a b
  mbBa <- lookupTree etf b a
  case (mbAb, mbBa) of
    _ | a == b -> return False -- Can't cut self-loops
    (Just ab, Just ba) -> do
      (part1, part2) <- Tree.split ab

      baIsInPart1 <- case part1 of
        Just p -> Tree.connected p ba
        _      -> return False

      (mbL, _, mbR) <- if baIsInPart1 then do
        (part3, part4) <- Tree.split ba
        return (part3, part4, part2)
      else do
        (part3, part4) <- Tree.split ba
        return (part1, part3, part4)

      _ <- sequenceA $ Tree.append <$> mbL <*> mbR
      deleteTree etf a b
      deleteTree etf b a
      return True

    (Nothing, _) -> return False -- No edge to cut
    (_, Nothing) -> return False -- No edge to cut

-- | reroot the represented tree by shifting the euler tour.  Returns the new
-- root.
reroot
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid v)
    => t s a v -> m (t s a v)
reroot t = do
    (mbPre, mbPost) <- Tree.split t
    t1 <- maybe (return t) (t `Tree.cons`) mbPost
    maybe (return t1) (t1 `Tree.append`) mbPre

hasEdge
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t a s v -> v -> v -> m Bool
hasEdge etf a b = isJust <$> lookupTree etf a b

connected
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> v -> m (Maybe Bool)
connected etf a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Just <$> Tree.connected aLoop bLoop
    _                        -> return Nothing

insertEdge
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> v -> m Bool
insertEdge etf@ETF{..} a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Tree.connected aLoop bLoop >>= \case
        True -> return False
        False -> do

          bLoop1            <- reroot bLoop
          abNode            <- Tree.singleton treeGen (a, b) (toMonoid a b)
          baNode            <- Tree.singleton treeGen (b, a) (toMonoid b a)
          bLoop2            <- abNode `Tree.cons` bLoop1
          bLoop3            <- bLoop2 `Tree.snoc` baNode
          (mbPreA, mbPostA) <- Tree.split aLoop

          _ <- Tree.concat $
            aLoop NonEmpty.:| catMaybes
            [ Just bLoop3
            , mbPostA
            , mbPreA
            ]

          insertTree etf a b abNode
          insertTree etf b a baNode
          return True

    _ -> return False

insertVertex
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> m ()
insertVertex etf@ETF{..} v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Just  _ -> return ()  -- It's already there
        Nothing -> do
            node <- Tree.singleton treeGen (v, v) (toMonoid v v)
            insertTree etf v v node

neighbours
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> m [v]
neighbours (ETF ht _ _) x = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return []
        Just m  -> return $ filter (/= x) $ map fst $ HMS.toList m

deleteVertex
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> m ()
deleteVertex etf x = do
    nbs <- neighbours etf x
    forM_ nbs $ \y -> deleteEdge etf x y
    deleteTree etf x x

print :: (Show a, Monoid b, Tree.TestTree t) => Forest t b RealWorld a -> IO ()
print (ETF ht _ _) = do
    maps <- map snd <$> HT.toList ht
    let trees = concatMap (map snd . HMS.toList) maps
    comps <- components trees
    forM_ comps $ \comp -> do
        root <- Tree.root comp
        Tree.print root
        putStrLn ""
  where
    components [] = return []
    components (t : ts) = do
        ts' <- filterM (fmap not . Tree.connected t) ts
        (t :) <$> components ts'

componentSize
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t (Sum Int) s v -> v -> m Int
componentSize etf v = do
  mbTree <- lookupTree etf v v
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- Tree.root tree
      getSum <$> Tree.aggregate root
