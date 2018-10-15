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
    , empty
    , empty'
    , edgeless
    , edgeless'
    , fromTree
    , fromTree'

      -- * Queries
    , connected
    , edge
    , vertex
    , neighbours

      -- * Modifying
    , link
    , link_
    , cut
    , cut_
    , insert
    , insert_
    , delete
    , delete_

      -- * Advanced/internal operations
    , findRoot
    , componentSize

      -- * Debugging
    , print
    ) where

import           Control.Monad                         (filterM, foldM, forM_,
                                                        void)
import           Control.Monad.Primitive
import qualified Data.Graph.Dynamic.Internal.HashTable as HT
import qualified Data.Graph.Dynamic.Internal.Random    as Random
import qualified Data.Graph.Dynamic.Internal.Tree      as Tree
import           Data.Hashable                         (Hashable)
import qualified Data.HashMap.Strict                   as HMS
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

-- | Graph type polymorphic in the tree used to represent sequences.
type Graph t s v = Forest t () s v

-- | Simple graph type.
type Graph' s v = Graph Random.Tree s v

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

-- | Create the empty tree.
empty
    :: forall t m v a. (Tree.Tree t, PrimMonad m)
    => (v -> v -> a) -> m (Forest t a (PrimState m) v)
empty f = do
  ht <- HT.new
  tg <- Tree.newTreeGen (Proxy :: Proxy t)
  return $ ETF ht f tg

-- | Simple version of 'empty'.
empty'
    :: PrimMonad m => m (Graph' (PrimState m) v)
empty' = empty (\_ _ -> ())

-- | Create a graph with the given vertices but no edges.
edgeless
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => (v -> v -> a) -> [v] -> m (Forest t a (PrimState m) v)
edgeless toMonoid vs = do
    etf <- empty toMonoid
    forM_ vs $ \v -> do
        node <- Tree.singleton (treeGen etf) (v, v) (toMonoid v v)
        insertTree etf v v node
    return etf

-- | Simple version of 'edgeless'.
edgeless'
    :: (Eq v, Hashable v, PrimMonad m)
    => [v] -> m (Graph' (PrimState m) v)
edgeless' = edgeless (\_ _ -> ())

-- | Create a graph from a 'DT.Tree'.  Note that the values in nodes must be
-- unique.
fromTree
    :: forall v m t a. (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => (v -> v -> a) -> DT.Tree v -> m (Forest t a (PrimState m) v)
fromTree toMonoid tree = do
    etf <- empty toMonoid
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

-- | Simple version of 'fromTree'.
fromTree'
    :: (Eq v, Hashable v, PrimMonad m)
    => DT.Tree v -> m (Graph' (PrimState m) v)
fromTree' = fromTree (\_ _ -> ())

findRoot
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid a)
    => Forest t a s v -> v -> m (Maybe (t s (v, v) a))
findRoot etf v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Nothing -> return Nothing
        Just t  -> Just <$> Tree.root t

-- | Remove an edge in between two vertices.  If there is no edge in between
-- these vertices, do nothing.  Return whether or not an edge was actually
-- removed.
cut
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> v -> m Bool
cut etf a b = do
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

-- | Version of 'cut' which ignores the result.
cut_
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> v -> m ()
cut_ etf a b = void (cut etf a b)

-- | reroot the represented tree by shifting the euler tour.  Returns the new
-- root.
reroot
    :: (Tree.Tree t, PrimMonad m, s ~ PrimState m, Monoid v)
    => t s a v -> m (t s a v)
reroot t = do
    (mbPre, mbPost) <- Tree.split t
    t1 <- maybe (return t) (t `Tree.cons`) mbPost
    maybe (return t1) (t1 `Tree.append`) mbPre

-- | Check if this edge exists in the graph.
edge
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m)
    => Forest t a (PrimState m) v -> v -> v -> m Bool
edge etf a b = isJust <$> lookupTree etf a b

-- | Check if this vertex exists in the graph.
vertex
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m)
    => Forest t a (PrimState m) v -> v -> m Bool
vertex etf a = isJust <$> lookupTree etf a a

-- | Check if a path exists in between two vertices.
connected
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> v -> m (Maybe Bool)
connected etf a b = do
  mbALoop <- lookupTree etf a a
  mbBLoop <- lookupTree etf b b
  case (mbALoop, mbBLoop) of
    (Just aLoop, Just bLoop) -> Just <$> Tree.connected aLoop bLoop
    _                        -> return Nothing

-- | Insert an edge in between two vertices.  If the vertices are already
-- connected, we don't do anything, since this is an acyclic graph.  Returns
-- whether or not an edge was actually inserted.
link
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> v -> m Bool
link etf@ETF{..} a b = do
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

-- | Version of 'link' which ignores the result.
link_
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> v -> m ()
link_ etf a b = void (link etf a b)

-- | Insert a new vertex.  Do nothing if it is already there.  Returns whether
-- or not a vertex was inserted in the graph.
insert
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> m Bool
insert etf@ETF{..} v = do
    mbTree <- lookupTree etf v v
    case mbTree of
        Just  _ -> return False
        Nothing -> do
            node <- Tree.singleton treeGen (v, v) (toMonoid v v)
            insertTree etf v v node
            return True

-- | Version of 'insert' which ignores the result.
insert_
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> m ()
insert_ etf v = void (insert etf v)

-- | Get all neighbours of the given vertex.
neighbours
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> m [v]
neighbours etf x = fromMaybe [] <$> maybeNeighbours etf x

maybeNeighbours
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> m (Maybe [v])
maybeNeighbours (ETF ht _ _) x = do
    mbMap <- HT.lookup ht x
    case mbMap of
        Nothing -> return Nothing
        Just m  -> return $ Just $ filter (/= x) $ map fst $ HMS.toList m

-- | Remove a vertex from the graph, if it exists.  If it is connected to any
-- other vertices, those edges are cut first.  Returns whether or not a vertex
-- was removed from the graph.
delete
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> m Bool
delete etf x = do
    mbNbs <- maybeNeighbours etf x
    case mbNbs of
        Nothing  -> return False
        Just nbs -> do
            forM_ nbs $ \y -> cut etf x y
            deleteTree etf x x
            return True

-- | Version of 'delete' which ignores the result.
delete_
    :: (Eq v, Hashable v, Tree.Tree t, PrimMonad m, Monoid a)
    => Forest t a (PrimState m) v -> v -> m ()
delete_ etf x = void (delete etf x)

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
