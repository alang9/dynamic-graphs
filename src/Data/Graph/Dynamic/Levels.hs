-- | This module implements full dynamic grah connectivity.
--
-- It is based on:
-- /Poly-logarithmic deterministic fully-dynamic algorithms for connectivity,
-- minimum spanning tree, 2-edge, and biconnectivity/ by /Jacob Holm, Kristian
-- de Lichtenberg and Mikkel Thorup/ (1998).
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dynamic.Levels
    ( -- * Type
      Graph

      -- * Construction
    , new
    , fromVertices

      -- * Queries
    , connected
    , hasEdge

      -- * Modifying
    , insertEdge
    , deleteEdge
    , insertVertex
    , deleteVertex
    ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Hashable                     (Hashable)
import qualified Data.HashMap.Strict               as HMS
import qualified Data.HashSet                      as HS
import qualified Data.List                         as L
import Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Primitive.MutVar
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Mutable               as VM

import qualified Data.Graph.Dynamic.EulerTour      as ET
import qualified Data.Graph.Dynamic.Internal.Splay as Splay

type EdgeSet v = HMS.HashMap v (HS.HashSet v)

linkEdgeSet :: (Eq v, Hashable v) => v -> v -> EdgeSet v -> EdgeSet v
linkEdgeSet x y =
    HMS.insertWith HS.union x (HS.singleton y) .
    HMS.insertWith HS.union y (HS.singleton x)

cutEdgeSet :: (Eq v, Hashable v) => v -> v -> EdgeSet v -> EdgeSet v
cutEdgeSet x y = HMS.adjust (HS.delete x) y . HMS.adjust (HS.delete y) x

memberEdgeSet :: (Eq v, Hashable v) => v -> v -> EdgeSet v -> Bool
memberEdgeSet x y = maybe False (y `HS.member`) . HMS.lookup x

nullEdgeSet :: (Eq v, Hashable v) => v -> EdgeSet v -> Bool
nullEdgeSet v = maybe True HS.null . HMS.lookup v

data L s v = L
  { numEdges :: !Int
  , allEdges :: !(EdgeSet v)
  , unLevels :: !(VM.MVector s (ET.Forest (Aggregate s v) s v))
  }

newtype Graph s v = Graph (MutVar s (L s v))

data Aggregate s v
  = EdgeAggregate
    { eSize :: {-# UNPACK #-} !Int
    , hasNonTreeEdges :: {-# UNPACK #-} !Bool
    }
  | VertexAggregate
    { neighbours :: Map v (Splay.Tree s (v, v) (Aggregate s v))
    } deriving (Eq)

instance (Show v) => Show (Aggregate s v) where
  show (EdgeAggregate s b) = unwords ["EdgeAggregate", show s, show b]
  show (VertexAggregate n) = unwords ["VertexAggregate", show (Map.keys n)]

hasNeighbours :: Aggregate s v -> Bool
hasNeighbours (EdgeAggregate _ x) = x
hasNeighbours (VertexAggregate xs) = not (Map.null xs)

size :: Aggregate s v -> Int
size (EdgeAggregate s _) = s
size (VertexAggregate _) = 1

instance Semigroup (Aggregate s v) where
  EdgeAggregate s1 h1 <> EdgeAggregate s2 h2 = EdgeAggregate (s1 + s2) (h1 || h2)
  VertexAggregate neighbours <> EdgeAggregate s2 h2 = EdgeAggregate (1 + s2) (not (Map.null neighbours) || h2)
  EdgeAggregate s1 h1 <> VertexAggregate neighbours = EdgeAggregate (s1 + 1) (h1 || not (Map.null neighbours))
  VertexAggregate neighbours1 <> VertexAggregate neighbours2 = EdgeAggregate 2 (not (Map.null neighbours1) || not (Map.null neighbours2))

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

new :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => m (Graph s v)
new = fromVertices []

fromVertices
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => [v] -> m (Graph s v)
fromVertices xs = do
  unLevels <- VM.new 0
  let allEdges = HMS.fromList $ zip xs $ repeat HS.empty
      numEdges = 0
  Graph <$> newMutVar L {..}

-- TODO (jaspervdj): Kill Ord constraints in this module
insertEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => Graph s v -> v -> v -> m Bool
insertEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  let !newAllEdges = linkEdgeSet a b allEdges
      !newNumEdges = numEdges + 1

  if memberEdgeSet a b allEdges || a == b
    then return False
    else do
      unLevels' <- do
        let oldNumLevels = VM.length unLevels
        newUnLevels <- VM.take (logBase2 newNumEdges + 2) <$>
          VM.grow unLevels (max 0 $ logBase2 newNumEdges - oldNumLevels + 2)
        forM_ [oldNumLevels .. logBase2 newNumEdges + 1] $ \levelIdx -> do
          df <- ET.discreteForest (\v1 v2 -> if v1 == v2 then VertexAggregate Map.empty else VertexAggregate Map.empty) $ map fst $ HMS.toList allEdges
          VM.write newUnLevels levelIdx df
        return newUnLevels
      if VM.null unLevels'
        then return $ error "insertEdge: should never happen"
        else do
          thisEtf <- VM.read unLevels' 0
          m'insertResult <- ET.insertEdge' thisEtf a b
          case m'insertResult of
            Nothing -> error "insertEdge: should never happen"
            Just (newlyConnected, aLoop, bLoop) -> do
              when (not newlyConnected) $ do
                Splay.modifyValue aLoop $ \(VertexAggregate n) -> VertexAggregate (Map.insert b bLoop n)
                Splay.modifyValue bLoop $ \(VertexAggregate n) -> VertexAggregate (Map.insert a aLoop n)
              VM.write unLevels' 0 thisEtf
              writeMutVar levels $ L
                  {allEdges = newAllEdges, unLevels = unLevels', numEdges = newNumEdges}
              return True

connected :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m (Maybe Bool)
connected _ a b | a == b = return (Just True)
connected (Graph levels) a b = do
  L {..} <- readMutVar levels
  if VM.null unLevels
    then return (Just False)
    else do
      etf <- VM.read unLevels 0
      ET.connected etf a b

hasEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m Bool
hasEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  return $ memberEdgeSet a b allEdges

componentSize
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => ET.Forest (Aggregate s v) s v -> v -> m Int
componentSize etf v = do
  mbTree <- ET.lookupTree etf v v
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- Splay.root tree
      size <$> Splay.aggregate root

deleteEdge :: forall m s v. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => Graph s v -> v -> v -> m ()
deleteEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  let newAllEdges = cutEdgeSet a b allEdges
  -- | a == b = return Graph {..}
  if VM.length unLevels == 0 || a == b
    then return ()
    else do
      cut <- go unLevels (VM.length unLevels-1)
      let newNumEdges = if cut then numEdges - 1 else numEdges
      writeMutVar levels L {allEdges = newAllEdges, numEdges = newNumEdges, ..}
  where
    go :: VM.MVector s (ET.Forest (Aggregate s v) s v) -> Int -> m Bool
    go unLevels idx = do
      etf <- VM.read unLevels idx
      cutResult <- ET.deleteEdge etf a b
      case cutResult of
        False -> do
          Just aLoop <- ET.lookupTree etf a a
          Splay.modifyValue aLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.delete b xs
          Just bLoop <- ET.lookupTree etf b b
          Splay.modifyValue bLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.delete a xs
          if idx > 0 then go unLevels (idx - 1) else return False
        True -> do
          aSize <- componentSize etf a
          bSize <- componentSize etf b
          let (smaller, _bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot etf smaller

          let findRep' :: [(v, v)] -> [((v, Splay.Tree s (v, v) (Aggregate s v)), (v, Splay.Tree s (v, v) (Aggregate s v)))] -> m ([(v, v)], Maybe (v, v))
              findRep' punish [] = return (punish, Nothing)
              findRep' punish (((x, xLoop), (y, yLoop)):xs) = ET.connected etf x y >>= \case
                Nothing -> error "should never happen"
                Just True -> do
                  Splay.modifyValue xLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.delete y xs
                  Splay.modifyValue yLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.delete x xs
                  findRep' ((x, y):punish) xs
                Just False -> do
                  Splay.modifyValue xLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.delete y xs
                  Splay.modifyValue yLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.delete x xs
                  return (punish, Just (x, y))

          let findRep :: [(v, v)] -> Splay.Tree s (v, v) (Aggregate s v) -> m ([(v, v)], Maybe (v, v))
              findRep punish node = if Splay.isNil node
                then do
                  return (punish, Nothing)
                else do
                  node' <- readMutVar node
                  case hasNeighbours (Splay.tAgg node') of
                    False -> return (punish, Nothing)
                    True -> case Splay.tValue node' of
                      VertexAggregate neighbours | Map.size neighbours > 0 -> do
                        let (x, _y) = Splay.tLabel node'
                        (punish', res) <- findRep' punish $ map ((,) (x, node)) $ Map.toList neighbours
                        case res of
                          Nothing -> do
                            Splay.splay node
                            findRep punish' node
                          Just res -> return (punish', Just res)
                      _ -> findRep punish (Splay.tLeft node') >>= \case
                        r@(_, Just _) -> return r
                        (punish', Nothing) -> findRep punish' (Splay.tRight node')

          -- These are all edges, and vertices within the smaller tree.
          sTreeEdges <- Splay.toList sRoot

          -- Perform the search
          (punished, replacementEdge) <- findRep [] sRoot
          -- Increase the levels of the tree edges and the punished edges.
          if
              | idx + 1 >= VM.length unLevels -> return ()
              | otherwise -> do
                    incEtf <- VM.read unLevels (idx + 1)

                    let moveTreeEdge (x, y) =
                            ET.insertEdge incEtf x y

                    let moveNonTreeEdge (x, y) = do
                          Just xLoop <- ET.lookupTree incEtf x x
                          Just yLoop <- ET.lookupTree incEtf y y
                          Splay.modifyValue xLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.insert y yLoop xs
                          Splay.modifyValue yLoop $ \(VertexAggregate xs) -> VertexAggregate $ Map.insert x xLoop xs

                    mapM_ moveTreeEdge sTreeEdges
                    !incNonTreeEdges1 <- mapM_
                            moveNonTreeEdge punished

                    VM.write unLevels (idx + 1) incEtf
                    return ()

          case replacementEdge of
            Nothing  -> do
              if idx > 0 then go unLevels (idx - 1) else return True
            Just rep@(c, d) -> do
              ET.insertEdge etf c d
              propagateReplacement unLevels (idx - 1) rep
              return True

    propagateReplacement unLevels idx (c, d) = when (idx >= 0) $ do
      etf <- VM.read unLevels idx
      _ <- ET.deleteEdge etf a b
      _ <- ET.insertEdge etf c d
      -- TODO: mess with edges??
      propagateReplacement unLevels (idx - 1) (c, d)

insertVertex
    :: (Eq v, Hashable v, PrimMonad m) => Graph (PrimState m) v -> v -> m ()
insertVertex (Graph g) x = do
    l@L {..} <- readMutVar g
    let newAllEdges   = HMS.insertWith HS.union x HS.empty allEdges
        updateLevel i
            | i >= VM.length unLevels = return ()
            | otherwise               = do
                forest <- VM.read unLevels i
                ET.insertVertex forest x
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar g $ l {allEdges = newAllEdges}

deleteVertex
    :: (Eq v, Hashable v, PrimMonad m, Ord v) => Graph (PrimState m) v -> v -> m ()
deleteVertex g@(Graph levels) x = do
    l0 <- readMutVar levels
    let neighbours = fromMaybe HS.empty (HMS.lookup x (allEdges l0))
    forM_ neighbours $ \y -> deleteEdge g x y

    l1 <- readMutVar levels
    let newAllEdges = HMS.delete x (allEdges l1)
        updateLevel i
            | i >= VM.length (unLevels l1) = return ()
            | otherwise                    = do
                forest <- VM.read (unLevels l1) i
                ET.deleteVertex forest x
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar levels $ l1 {allEdges = newAllEdges}
