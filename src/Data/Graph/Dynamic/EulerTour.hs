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
import qualified Data.Graph.Dynamic.Internal.Random    as Random
import           Data.Graph.Dynamic.Internal.Tree      (Edge (..), Vertex (..))
import qualified Data.Graph.Dynamic.Internal.Tree      as Tree
import qualified Data.HashMap.Strict                   as HMS
import           Data.List
import qualified Data.List.NonEmpty                    as NonEmpty
import           Data.Maybe
import           Data.Proxy                            (Proxy (..))
import qualified Data.Tree                             as DT
import           Prelude                               hiding (print)

data Forest t s = ETF
    { edges   :: {-# UNPACK#-} !(HT.HashTable s Int (HMS.HashMap Int (t s)))
    , treeGen :: (Tree.TreeGen t s)
    }

type Graph = Forest

type Graph' = Graph Random.Tree

insertTree
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Edge -> t s -> m ()
insertTree (ETF ht _) (Edge x y) t = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> HT.insert ht x $ HMS.singleton y t
        Just m  -> HT.insert ht x $ HMS.insert y t m

lookupTree
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Edge -> m (Maybe (t s))
lookupTree (ETF ht _) (Edge x y) = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return Nothing
        Just m  -> return $ HMS.lookup y m

deleteTree
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Edge -> m ()
deleteTree (ETF ht _) (Edge x y) = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return ()
        Just m0 ->
            let m1 = HMS.delete y m0 in
            if HMS.null m1 then HT.delete ht x else HT.insert ht x m1

new :: forall t m s. (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => m (Forest t s)
new = do
  ht <- HT.new
  tg <- Tree.newTreeGen (Proxy :: Proxy t)
  return $ ETF ht tg

-- values in nodes must be unique
fromTree
    :: forall m t s. (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => DT.Tree Vertex -> m (Forest t s)
fromTree tree = do
    etf <- new
    _ <- go etf tree
    return etf
  where
    go etf (DT.Node (Vertex l) children) = do
      node0 <- Tree.singleton (treeGen etf) (Edge l l)
      insertTree etf (Edge l l) node0
      foldM (go' etf l) node0 children

    go' etf parent node0 tr@(DT.Node (Vertex l) _) = do
      lnode     <- go etf tr
      parentToL <- Tree.singleton (treeGen etf) (Edge parent l)
      lToParent <- Tree.singleton (treeGen etf) (Edge l parent)

      node1 <- Tree.concat $ node0 NonEmpty.:| [parentToL, lnode, lToParent]
      insertTree etf (Edge l parent) lToParent
      insertTree etf (Edge parent l) parentToL
      return node1

discreteForest
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => [Vertex] -> m (Forest t s)
discreteForest vs = do
    etf <- new
    forM_ vs $ \(Vertex v) -> do
        node <- Tree.singleton (treeGen etf) (Edge v v)
        insertTree etf (Edge v v) node
    return etf

discreteForest'
    :: (PrimMonad m, s ~ PrimState m)
    => [Vertex] -> m (Forest Random.Tree s)
discreteForest' = discreteForest

findRoot
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> m (Maybe (t s))
findRoot etf (Vertex v) = do
    mbTree <- lookupTree etf (Edge v v)
    case mbTree of
        Nothing -> return Nothing
        Just t  -> Just <$> Tree.root t

deleteEdge
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> Vertex -> m Bool
deleteEdge etf (Vertex a) (Vertex b) = do
  mbAb <- lookupTree etf (Edge a b)
  mbBa <- lookupTree etf (Edge b a)
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
      deleteTree etf (Edge a b)
      deleteTree etf (Edge b a)
      return True

    (Nothing, _) -> return False -- No edge to cut
    (_, Nothing) -> return False -- No edge to cut

-- | reroot the represented tree by shifting the euler tour.  Returns the new
-- root.
reroot
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => t s -> m (t s)
reroot t = do
    (mbPre, mbPost) <- Tree.split t
    t1 <- maybe (return t) (t `Tree.cons`) mbPost
    maybe (return t1) (t1 `Tree.append`) mbPre

hasEdge
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> Vertex -> m Bool
hasEdge etf (Vertex a) (Vertex b) = isJust <$> lookupTree etf (Edge a b)

connected
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> Vertex -> m (Maybe Bool)
connected etf (Vertex a) (Vertex b) = do
  mbALoop <- lookupTree etf (Edge a a)
  mbBLoop <- lookupTree etf (Edge b b)
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Just <$> Tree.connected aLoop bLoop
    _                        -> return Nothing

insertEdge
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> Vertex -> m Bool
insertEdge etf@ETF{..} (Vertex a) (Vertex b) = do
  mbALoop <- lookupTree etf (Edge a a)
  mbBLoop <- lookupTree etf (Edge b b)
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Tree.connected aLoop bLoop >>= \case
        True -> return False
        False -> do

          bLoop1            <- reroot bLoop
          abNode            <- Tree.singleton treeGen (Edge a b)
          baNode            <- Tree.singleton treeGen (Edge b a)
          bLoop2            <- abNode `Tree.cons` bLoop1
          bLoop3            <- bLoop2 `Tree.snoc` baNode
          (mbPreA, mbPostA) <- Tree.split aLoop

          _ <- Tree.concat $
            aLoop NonEmpty.:| catMaybes
            [ Just bLoop3
            , mbPostA
            , mbPreA
            ]

          insertTree etf (Edge a b) abNode
          insertTree etf (Edge b a) baNode
          return True

    _ -> return False

insertVertex
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> m ()
insertVertex etf@ETF{..} (Vertex v) = do
    mbTree <- lookupTree etf (Edge v v)
    case mbTree of
        Just  _ -> return ()  -- It's already there
        Nothing -> do
            node <- Tree.singleton treeGen (Edge v v)
            insertTree etf (Edge v v) node

neighbours
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> m [Vertex]
neighbours (ETF ht _) (Vertex x) = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return []
        Just m  -> return $ map Vertex $ filter (/= x) $ map fst $ HMS.toList m

deleteVertex
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> m ()
deleteVertex etf (Vertex x) = do
    nbs <- neighbours etf (Vertex x)
    forM_ nbs $ \y -> deleteEdge etf (Vertex x) y
    deleteTree etf (Edge x x)

print :: (Tree.TestTree t) => Forest t RealWorld -> IO ()
print (ETF ht _) = do
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
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m)
    => Forest t s -> Vertex -> m Int
componentSize etf (Vertex v) = do
  mbTree <- lookupTree etf (Edge v v)
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- Tree.root tree
      Tree.aggregate root
