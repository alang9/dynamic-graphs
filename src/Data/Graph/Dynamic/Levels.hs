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
  , unLevels :: !(VM.MVector s (ET.Forest Aggregate s v, EdgeSet v))
  }

newtype Graph s v = Graph (MutVar s (L s v))

data Aggregate
  = Aggregate
    { size :: {-# UNPACK #-} !Int
    , hasNonTreeEdges :: {-# UNPACK #-} !Bool
    } deriving (Show)

instance Semigroup Aggregate where
  Aggregate s1 h1 <> Aggregate s2 h2 = Aggregate (s1 + s2) (h1 || h2)

instance Monoid Aggregate where
  mempty = Aggregate 0 False

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
insertEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m Bool
insertEdge (Graph levels) a b = do --traceShow (numEdges, VM.length unLevels, HS.member (a, b) allEdges) $
  L {..} <- readMutVar levels
  let !newAllEdges = linkEdgeSet a b allEdges
      !newNumEdges = numEdges + 1

  if memberEdgeSet a b allEdges || a == b
    then return False
    else do
      unLevels' <- do
        let oldNumLevels = VM.length unLevels
        newUnLevels <- VM.take (logBase2 newNumEdges + 1) <$>
          VM.grow unLevels (max 0 $ logBase2 newNumEdges - oldNumLevels + 1)
        forM_ [oldNumLevels .. logBase2 newNumEdges] $ \levelIdx -> do
          df <- ET.discreteForest (\v1 v2 -> if v1 == v2 then Aggregate 1 False else Aggregate 0 False) $ map fst $ HMS.toList allEdges
          VM.write newUnLevels levelIdx (df, HMS.empty)
        return newUnLevels
      if VM.null unLevels'
        then return $ error "insertEdge: should never happen"
        else do
          (thisEtf, thisNonTreeEdges) <- VM.read unLevels' 0
          m'insertResult <- ET.insertEdge' thisEtf a b
          case m'insertResult of
            Nothing -> error "insertEdge: should never happen"
            Just (newlyConnected, aLoop, bLoop) -> do
              when (not newlyConnected) $ do
                modifyMutVar aLoop $ \aLoop' -> aLoop' {Splay.tValue = Aggregate 1 True}
                Splay.propagate aLoop
                modifyMutVar bLoop $ \bLoop' -> bLoop' {Splay.tValue = Aggregate 1 True}
                Splay.propagate bLoop
              let !thisNonTreeEdges'
                    | newlyConnected = thisNonTreeEdges
                    | otherwise  = linkEdgeSet a b thisNonTreeEdges
              VM.write unLevels' 0 (thisEtf, thisNonTreeEdges')
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
      (etf, _) <- VM.read unLevels 0
      ET.connected etf a b

hasEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m Bool
hasEdge (Graph levels) a b = do
  L {..} <- readMutVar levels
  return $ memberEdgeSet a b allEdges

componentSize
    :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m)
    => ET.Forest Aggregate s v -> v -> m Int
componentSize etf v = do
  mbTree <- ET.lookupTree etf v v
  case mbTree of
    Nothing -> return 0
    Just tree -> do
      root <- Splay.root tree
      size <$> Splay.aggregate root

deleteEdge :: forall m s v. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m) => Graph s v -> v -> v -> m ()
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
    go :: VM.MVector s (ET.Forest Aggregate s v, EdgeSet v) -> Int -> m Bool
    go unLevels idx = do
      (etf, nonTreeEdges0) <- VM.read unLevels idx
      cutResult <- ET.deleteEdge etf a b
      case cutResult of
        False -> do
          let !nonTreeEdges1 = cutEdgeSet a b nonTreeEdges0
          when (nullEdgeSet a nonTreeEdges1) $ do
            Just aLoop <- ET.lookupTree etf a a
            modifyMutVar' aLoop $ \aLoop' -> aLoop' {Splay.tValue = Aggregate 1 False}
            Splay.propagate aLoop
          when (nullEdgeSet b nonTreeEdges1) $ do
            Just bLoop <- ET.lookupTree etf b b
            modifyMutVar' bLoop $ \bLoop' -> bLoop' {Splay.tValue = Aggregate 1 False}
            Splay.propagate bLoop
          VM.write unLevels idx (etf, nonTreeEdges1)
          if idx > 0 then go unLevels (idx - 1) else return False
        True -> do
          aSize <- componentSize etf a
          bSize <- componentSize etf b
          let (smaller, _bigger) = if aSize <= bSize then (a, b) else (b, a)
          Just sRoot <- ET.findRoot etf smaller

          let findRep' :: [(v, v)] -> EdgeSet v -> [(v, v)] -> m ([(v, v)], EdgeSet v, Maybe (v, v))
              findRep' punish nonT [] = return (punish, nonT, Nothing)
              findRep' punish nonT ((x, y):xs) = ET.connected etf x y >>= \case
                Nothing -> error "should never happen"
                Just True -> do
                  let nonT' = cutEdgeSet x y nonT
                  when (nullEdgeSet x nonT') $ do
                    Just xLoop <- ET.lookupTree etf x x
                    modifyMutVar' xLoop $ \xLoop' -> xLoop' {Splay.tValue = Aggregate 1 False}
                    Splay.propagate xLoop
                  when (nullEdgeSet y nonT') $ do
                    Just yLoop <- ET.lookupTree etf y y
                    modifyMutVar' yLoop $ \yLoop' -> yLoop' {Splay.tValue = Aggregate 1 False}
                    Splay.propagate yLoop
                  findRep' ((x, y):punish) nonT' xs
                Just False -> do
                  let nonT' = cutEdgeSet x y nonT
                  when (nullEdgeSet x nonT') $ do
                    Just xLoop <- ET.lookupTree etf x x
                    modifyMutVar' xLoop $ \xLoop' -> xLoop' {Splay.tValue = Aggregate 1 False}
                    Splay.propagate xLoop
                  when (nullEdgeSet y nonT') $ do
                    Just yLoop <- ET.lookupTree etf y y
                    modifyMutVar' yLoop $ \yLoop' -> yLoop' {Splay.tValue = Aggregate 1 False}
                    Splay.propagate yLoop
                  return (punish, nonT', Just (x, y))

          let findRep :: [(v, v)] -> EdgeSet v -> Splay.Tree s (v, v) Aggregate -> m ([(v, v)], EdgeSet v, Maybe (v, v))
              findRep punish nonT node = if Splay.isNil node
                then do
                  return (punish, nonT, Nothing)
                else do
                  node' <- readMutVar node
                  case Splay.tAgg node' of
                    Aggregate _ False -> return (punish, nonT, Nothing)
                    Aggregate _ True -> case Splay.tValue node' of
                      Aggregate _ True -> do
                        let (x, _y) = Splay.tLabel node'
                        let neighbours = maybe HS.empty id $ HMS.lookup x nonT
                        (punish', nonT', res) <- findRep' punish nonT (map ((,) x) $ HS.toList neighbours)
                        case res of
                          Nothing -> do
                            Splay.splay node
                            findRep punish' nonT' node
                          Just res -> return (punish', nonT', Just res)
                      Aggregate _ False -> findRep punish nonT (Splay.tLeft node') >>= \case
                        r@(_, _, Just _) -> return r
                        (punish', nonT', Nothing) -> findRep punish' nonT' (Splay.tRight node')


          -- These are all edges, and vertices within the smaller tree.
          sTreeEdges <- Splay.toList sRoot

          -- Perform the search
          (punished, nonTreeEdges1, replacementEdge) <- findRep [] nonTreeEdges0 sRoot
          -- Increase the levels of the tree edges and the punished edges.
          if
              | idx + 1 >= VM.length unLevels -> return ()
              | otherwise -> do
                    (incEtf, incNonTreeEdges0) <- VM.read unLevels (idx + 1)

                    let moveTreeEdge (x, y) =
                            ET.insertEdge incEtf x y

                    let moveNonTreeEdge !incNTes (x, y) = do
                          let incNTes' = linkEdgeSet x y incNTes
                          when (nullEdgeSet x incNTes) $ do
                            Just xLoop <- ET.lookupTree incEtf x x
                            modifyMutVar' xLoop $ \xLoop' -> xLoop' {Splay.tValue = Aggregate 1 True}
                            Splay.propagate xLoop
                          when (nullEdgeSet y incNTes) $ do
                            Just yLoop <- ET.lookupTree incEtf y y
                            modifyMutVar' yLoop $ \yLoop' -> yLoop' {Splay.tValue = Aggregate 1 True}
                            Splay.propagate yLoop
                          return incNTes'

                    mapM_ moveTreeEdge sTreeEdges
                    !incNonTreeEdges1 <- foldM
                            moveNonTreeEdge incNonTreeEdges0 punished

                    VM.write unLevels (idx + 1) (incEtf, incNonTreeEdges1)
                    return ()

          case replacementEdge of
            Nothing  -> do
              VM.write unLevels idx (etf, nonTreeEdges1)
              if idx > 0 then go unLevels (idx - 1) else return True
            Just rep@(c, d) -> do
              let !nonTreeEdges2 = cutEdgeSet c d nonTreeEdges1
              VM.write unLevels idx (etf, nonTreeEdges2)
              ET.insertEdge etf c d
              propagateReplacement unLevels (idx - 1) rep
              return True

    propagateReplacement unLevels idx (c, d) = when (idx >= 0) $ do
      (etf, _) <- VM.read unLevels idx
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
                (forest, nt) <- VM.read unLevels i
                ET.insertVertex forest x
                VM.write unLevels i (forest, nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar g $ l {allEdges = newAllEdges}

deleteVertex
    :: (Eq v, Hashable v, PrimMonad m) => Graph (PrimState m) v -> v -> m ()
deleteVertex g@(Graph levels) x = do
    l0 <- readMutVar levels
    let neighbours = fromMaybe HS.empty (HMS.lookup x (allEdges l0))
    forM_ neighbours $ \y -> deleteEdge g x y

    l1 <- readMutVar levels
    let newAllEdges = HMS.delete x (allEdges l1)
        updateLevel i
            | i >= VM.length (unLevels l1) = return ()
            | otherwise                    = do
                (forest, nt) <- VM.read (unLevels l1) i
                ET.deleteVertex forest x
                VM.write (unLevels l1) i (forest, HMS.delete x nt)
                updateLevel (i + 1)

    updateLevel 0
    writeMutVar levels $ l1 {allEdges = newAllEdges}
