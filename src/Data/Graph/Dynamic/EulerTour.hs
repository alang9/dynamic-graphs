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

      -- * Construction
    , new
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

data Forest a s v = ETF
    { edges :: {-# UNPACK#-} !(HT.HashTable s v (HMS.HashMap v (Splay.Tree s (v, v) a)))
    , toMonoid :: v -> v -> a
    }

type Graph = Forest ()

insertTree
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest a s v -> v -> v -> Splay.Tree s (v, v) a -> m ()
insertTree (ETF ht _) x y t = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> HT.insert ht x $ HMS.singleton y t
        Just m  -> HT.insert ht x $ HMS.insert y t m

lookupTree
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest a s v -> v -> v -> m (Maybe (Splay.Tree s (v, v) (a)))
lookupTree (ETF ht _) x y = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return Nothing
        Just m  -> return $ HMS.lookup y m

deleteTree
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest a s v -> v -> v -> m ()
deleteTree (ETF ht _) x y = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return ()
        Just m0 ->
            let m1 = HMS.delete y m0 in
            if HMS.null m1 then HT.delete ht x else HT.insert ht x m1

new :: (PrimMonad m, s ~ PrimState m) => (v -> v -> a) -> m (Forest a s v)
new f = do
  ht <- HT.new
  return $ ETF ht f

-- values in nodes must be unique
fromTree
    :: forall v m s a. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => (v -> v -> a) -> Tree.Tree v -> m (Forest a s v)
fromTree toMonoid tree = do
    etf <- new toMonoid
    _ <- go etf tree
    return etf
  where
    go etf (Tree.Node l children) = do
      node0 <- Splay.singleton (l, l) (toMonoid l l)
      insertTree etf l l node0
      foldM (go' etf l) node0 children

    go' etf parent node0 tr@(Tree.Node l _) = do
      lnode     <- go etf tr
      parentToL <- Splay.singleton (parent, l) (toMonoid parent l)
      lToParent <- Splay.singleton (l, parent) (toMonoid l parent)

      node1 <- Splay.concat $ node0 NonEmpty.:| [parentToL, lnode, lToParent]
      insertTree etf l parent lToParent
      insertTree etf parent l parentToL
      return node1

discreteForest
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => (v -> v -> a) -> [v] -> m (Forest a s v)
discreteForest toMonoid vs = do
    etf <- new toMonoid
    forM_ vs $ \v -> do
        node <- Splay.singleton (v, v) (toMonoid v v)
        insertTree etf v v node
    return etf

findRoot
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest a s v -> v -> m (Maybe (Splay.Tree s (v, v) a))
findRoot etf v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Nothing -> return Nothing
        Just t  -> Just <$> Splay.root t

deleteEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest a s v -> v -> v -> m Bool
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
    t1 <- maybe (return t) (t `Splay.cons`) mbPost
    maybe (return t1) (t1 `Splay.append`) mbPre

hasEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest a s v -> v -> v -> m Bool
hasEdge etf a b = isJust <$> lookupTree etf a b

connected
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest a s v -> v -> v -> m (Maybe Bool)
connected etf a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Just <$> Splay.connected aLoop bLoop
    _                        -> return Nothing

insertEdge
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest a s v -> v -> v -> m Bool
insertEdge etf@ETF{..} a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Splay.connected aLoop bLoop >>= \case
        True -> return False
        False -> do

          bLoop1            <- reroot bLoop
          abNode            <- Splay.singleton (a, b) (toMonoid a b)
          baNode            <- Splay.singleton (b, a) (toMonoid b a)
          bLoop2            <- abNode `Splay.cons` bLoop1
          bLoop3            <- bLoop2 `Splay.snoc` baNode
          (mbPreA, mbPostA) <- Splay.split aLoop

          _ <- Splay.concat $
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
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest a s v -> v -> m ()
insertVertex etf@ETF{..} v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Just  _ -> return ()  -- It's already there
        Nothing -> do
            node <- Splay.singleton (v, v) (toMonoid v v)
            insertTree etf v v node

neighbours
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest a s v -> v -> m [v]
neighbours (ETF ht _) x = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return []
        Just m  -> return $ filter (/= x) $ map fst $ HMS.toList m

deleteVertex
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest a s v -> v -> m ()
deleteVertex etf x = do
    nbs <- neighbours etf x
    forM_ nbs $ \y -> deleteEdge etf x y
    deleteTree etf x x

print :: (Show a, Monoid b) => Forest b RealWorld a -> IO ()
print (ETF ht _) = do
  maps <- map snd <$> HT.toList ht
  let trees = concatMap (map snd . HMS.toList) maps
  roots <- mapM Splay.root trees
  forM_ (nub roots) $ \root -> do
    Splay.print root
    putStrLn ""

componentSize
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => Forest (Sum Int) s v -> v -> m Int
componentSize etf v = do
  mbTree <- lookupTree etf v v
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- Splay.root tree
      getSum <$> Splay.aggregate root
