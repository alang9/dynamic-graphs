{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Graph.Dynamic.Thorup2000 where

import Control.Applicative
import Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.HashMap.Strict               as HMS
import Data.Hashable (Hashable)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Primitive.MutVar
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality
import Unsafe.Coerce

import Debug.Trace

data Graph s v = Graph
  { numEdges :: !(MutVar s Int)
  , allEdges :: !(MutVar s (HMS.HashMap (v, v) (EdgeType, Level)))
  , allVertices :: !(MutVar s (Map v (StructuralTree 'True 'False 'False 'False 'False s v)))
  }

type Level = Int

type Rank = Int

data ST (nt0 :: Bool) (nt1 :: Bool) (nt2 :: Bool) (nt3 :: Bool) (nt4 :: Bool) s v where
  SLeaf :: forall s v a4.
    { lParent :: {-# UNPACK #-} !(StructuralTree 'False 'False 'False 'False a4 s v)
    , vertex :: !v
    , groups :: !(Map (EdgeType, Level) (Set v))
    , lTreeEdges :: {-# UNPACK #-} !Int
    , lNonTreeEdges :: {-# UNPACK #-} !Int
    }
    -> ST 'True 'False 'False 'False 'False s v
  SNode :: forall s v a1 a4.
    { nParent :: {-# UNPACK #-} !(StructuralTree 'False a1 'False 'False a4 s v)
    , children :: {-# UNPACK #-} !(StructuralTree 'False 'False 'True 'False 'False s v)
    , level :: {-# UNPACK #-} !Int
    , nTreeEdges :: {-# UNPACK #-} !Int
    , nNonTreeEdges :: {-# UNPACK #-} !Int
    , nCount :: {-# UNPACK #-} !Int
    }
    -> ST 'False 'True 'False 'False 'False s v
  LSNode :: forall s v nt nt' a3 a4.
    { lsPrev :: {-# UNPACK #-} !(StructuralTree 'False nt nt' 'False 'False s v)
    , lsLocalRankTree :: {-# UNPACK #-} !(StructuralTree 'False 'False 'False a3 a4 s v)
    , lsNext :: {-# UNPACK #-} !(StructuralTree 'False 'False 'True 'False 'False s v)
    , lsTreeEdges :: {-# UNPACK #-} !Int
    , lsNonTreeEdges :: {-# UNPACK #-} !Int
    , lsCount :: {-# UNPACK #-} !Int
    }
    -> ST 'False 'False 'True 'False 'False s v
  LRNode :: forall s v nt0 nt1 nt4 a4 nt7 a4'.
    { lrnParent :: {-# UNPACK #-} !(StructuralTree 'False 'False nt0 nt1 'False s v)
    , lrnLeft :: {-# UNPACK #-} !(StructuralTree 'False 'False 'False nt4 a4 s v)
    , lrnRight :: {-# UNPACK #-} !(StructuralTree 'False 'False 'False nt7 a4' s v)
    , lrnTreeEdges :: {-# UNPACK #-} !Int
    , lrnNonTreeEdges :: {-# UNPACK #-} !Int
    , lrnCount :: {-# UNPACK #-} !Int
    }
    -> ST 'False 'False 'False 'True 'False s v
  LRLeaf :: forall s v a2 a3 a0 a1.
    { lrlParent :: {-# UNPACK #-} !(StructuralTree 'False 'False a2 a3 'False s v)
    , lrlChild :: {-# UNPACK #-} !(StructuralTree a0 a1 'False 'False 'False s v)
    , lrlTreeEdges :: {-# UNPACK #-} !Int
    , lrlNonTreeEdges :: {-# UNPACK #-} !Int
    , lrlCount :: {-# UNPACK #-} !Int
    }
    -> ST 'False 'False 'False 'False 'True s v

type StructuralTree b0 b1 b2 b3 b4 s v = MutVar s (ST b0 b1 b2 b3 b4 s v)

data EdgeType = TreeEdge | NonTreeEdge deriving (Show, Eq, Ord)

new :: (PrimMonad m, s ~ PrimState m) => m (Graph s v)
new = do
  numEdges <- newMutVar 0
  allEdges <- newMutVar HMS.empty
  allVertices <- newMutVar Map.empty
  return $ Graph numEdges allEdges allVertices

eq :: MutVar s a -> MutVar s b -> Maybe (a :~: b)
eq a b = if a == unsafeCoerce b then Just (unsafeCoerce Refl) else Nothing

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

singletonST :: (PrimMonad m, s ~ PrimState m) => v -> m (StructuralTree 'True 'False 'False 'False 'False s v)
singletonST v = do
  sl <- newMutVar undefined
  lrl <- newMutVar undefined
  ls <- newMutVar undefined
  sn <- newMutVar undefined
  writeMutVar sn $ SNode sn ls 0 0 0 1
  -- checkNeq "sing" ls sn
  writeMutVar ls $ LSNode sn lrl ls 0 0 1
  writeMutVar lrl $ LRLeaf ls sl 0 0 1
  writeMutVar sl $ SLeaf lrl v Map.empty 0 0
  return sl

insert ::
  (Eq v, PrimMonad m, Ord v) => Graph (PrimState m) v -> v -> m ()
insert Graph {..} v = do
  vs <- readMutVar allVertices
  case Map.lookup v vs of
    Nothing -> do
      newSTree <- singletonST v
      writeMutVar allVertices $ Map.insert v newSTree vs
      modifyMutVar' numEdges succ
    Just _ -> return ()

fromVertices :: (PrimMonad m, Eq v, Ord v) => [v] -> m (Graph (PrimState m) v)
fromVertices xs = do
  n <- new
  mapM_ (insert n) xs
  return n

hasEdge :: (PrimMonad m, Eq v, Hashable v) => Graph (PrimState m) v -> v -> v -> m Bool
hasEdge Graph{..} x y = do
  ae <- readMutVar allEdges
  return $ HMS.member (x, y) ae

connected ::
  (Eq v, PrimMonad m, Ord v) => Graph (PrimState m) v -> v -> v -> m Bool
connected g a b = maybe False id <$> connected' g a b

connected' ::
  (Eq v, PrimMonad m, Ord v) => Graph (PrimState m) v -> v -> v -> m (Maybe Bool)
connected' Graph {..} a b = do
  allVertices' <- readMutVar allVertices
  case Map.lookup a allVertices' of
    Nothing -> return Nothing
    Just as -> case Map.lookup b allVertices' of
      Nothing -> return Nothing
      Just bs -> do
        aRoot <- getRoot as
        bRoot <- getRoot bs
        return $ Just $ aRoot == bRoot

getBitvectors :: (PrimState m ~ s, PrimMonad m) => StructuralTree b0 b1 b2 b3 b4 s v -> m (Int, Int)
getBitvectors st = do
  st' <- readMutVar st
  case st' of
    SLeaf {..} -> return (lTreeEdges, lNonTreeEdges)
    SNode {..} -> return (nTreeEdges, nNonTreeEdges)
    LSNode {..} -> return (lsTreeEdges, lsNonTreeEdges)
    LRNode {..} -> return (lrnTreeEdges, lrnNonTreeEdges)
    LRLeaf {..} -> return (lrlTreeEdges, lrlNonTreeEdges)

count :: (PrimState m ~ s, PrimMonad m) => StructuralTree b0 b1 b2 b3 b4 s v -> m Int
count st = do
  st' <- readMutVar st
  case st' of
    SLeaf {..} -> return 1
    SNode {..} -> return nCount
    LSNode {..} -> return lsCount
    LRNode {..} -> return lrnCount
    LRLeaf {..} -> return lrlCount

rank :: (PrimState m ~ s, PrimMonad m) => StructuralTree b0 b1 b2 b3 b4 s v -> m Int
rank a = logBase2 <$> count a

checkNeq :: (PrimMonad m, s ~ PrimState m) => String -> StructuralTree a0 a1 a2 a3 a4 s v -> StructuralTree b0 b1 b2 b3 b4 s v -> m ()
checkNeq str n1 n2 = do
  case eq n1 n2 of
    Just _ -> error str
    Nothing -> return ()

propagateLeaf ::
  (PrimState m ~ s, PrimMonad m) => StructuralTree 'True 'False 'False 'False 'False s v -> Int -> m ()
propagateLeaf st i = readMutVar st >>= \case
  st'@SLeaf {..} -> do
      let newT = case Map.lookup (TreeEdge, i) groups of
            Just xs | not (Set.null xs) -> setBit lTreeEdges i
            _ -> clearBit lTreeEdges i
      let newNT = case Map.lookup (NonTreeEdge, i) groups of
            Just xs | not (Set.null xs) -> setBit lNonTreeEdges i
            _ -> clearBit lNonTreeEdges i
      when (newT /= lTreeEdges || newNT /= lNonTreeEdges) $ do
        writeMutVar st $! st' {lTreeEdges = newT, lNonTreeEdges = newNT}
        when ((lParent `eq` st) == Nothing) $ propagate lParent

propagate :: (PrimState m ~ s, PrimMonad m) => StructuralTree b0 b1 b2 b3 b4 s v -> m ()
propagate st = do
  st' <- readMutVar st
  case st' of
    SLeaf {..} -> do
      let (newT, newNT) = Map.foldlWithKey' go (0, 0) groups
      when (newT /= lTreeEdges || newNT /= lNonTreeEdges) $ do
        writeMutVar st $! st' {lTreeEdges = newT, lNonTreeEdges = newNT}
        when ((lParent `eq` st) == Nothing) $ propagate lParent
      where
        go (t, nt) (TreeEdge, i) vs = if Set.null vs then (t, nt) else (setBit t i, nt)
        go (t, nt) (NonTreeEdge, i) vs = if Set.null vs then (t, nt) else (t, setBit nt i)
    SNode {..} -> do
      (newT, newNT) <- getBitvectors children
      childrenCount <- count children
      writeMutVar st $ st' {nTreeEdges = newT, nNonTreeEdges = newNT, nCount = childrenCount}
      when ((nParent `eq` st) == Nothing) $ propagate nParent
    LSNode {..} -> do
      (lrT, lrNT) <- getBitvectors lsLocalRankTree
      lrCnt <- count lsLocalRankTree
      case eq lsNext st of
        Just Refl -> when (lrT /= lsTreeEdges || lrNT /= lsNonTreeEdges || lrCnt /= lsCount) $ do
          writeMutVar st $ st' {lsTreeEdges = lrT, lsNonTreeEdges = lrNT, lsCount = lrCnt}
          propagate lsPrev
        Nothing -> do
          (nT, nNT) <- getBitvectors lsNext
          nCnt <- count lsNext
          let newT = nT .|. lrT
              newNT = nNT .|. lrNT
              newCnt = lrCnt + nCnt
          when (newT /= lsTreeEdges || newNT /= lsNonTreeEdges || newCnt /= lsCount) $ do
            writeMutVar st $ st' {lsTreeEdges = newT, lsNonTreeEdges = newNT, lsCount = newCnt}
            propagate lsPrev
    LRNode {..} -> do
      (lT, lNT) <- getBitvectors lrnLeft
      lCnt <- count lrnLeft
      (rT, rNT) <- getBitvectors lrnRight
      rCnt <- count lrnRight
      let newT = lT .|. rT
          newNT = lNT .|. rNT
          newCnt = lCnt + rCnt
      when (newT /= lrnTreeEdges || newNT /= lrnNonTreeEdges || newCnt /= lrnCount) $ do
        writeMutVar st $ st' {lrnTreeEdges = newT, lrnNonTreeEdges = newNT, lrnCount = newCnt}
        propagate lrnParent
    LRLeaf {..} -> do
      (cT, cNT) <- getBitvectors lrlChild
      cCnt <- count lrlChild
      when (cT /= lrlTreeEdges || cNT /= lrlNonTreeEdges || cCnt /= lrlCount) $ do
        writeMutVar st $ st' {lrlTreeEdges = cT, lrlNonTreeEdges = cNT, lrlCount = cCnt}
        propagate lrlParent

update :: (PrimState m ~ s, PrimMonad m) => StructuralTree b0 b1 b2 b3 b4 s v -> m ()
update st = do
  st' <- readMutVar st
  case st' of
    SLeaf {..} -> do
      let (newT, newNT) = Map.foldlWithKey' go (0, 0) groups
      when (newT /= lTreeEdges || newNT /= lNonTreeEdges) $ do
        writeMutVar st $! st' {lTreeEdges = newT, lNonTreeEdges = newNT}
      where
        go (t, nt) (TreeEdge, i) vs = if Set.null vs then (t, nt) else (setBit t i, nt)
        go (t, nt) (NonTreeEdge, i) vs = if Set.null vs then (t, nt) else (t, setBit nt i)
    SNode {..} -> do
      (newT, newNT) <- getBitvectors children
      childrenCount <- count children
      writeMutVar st $ st' {nTreeEdges = newT, nNonTreeEdges = newNT, nCount = childrenCount}
    LSNode {..} -> do
      (lrT, lrNT) <- getBitvectors lsLocalRankTree
      lrCnt <- count lsLocalRankTree
      case eq lsNext st of
        Just Refl -> when (lrT /= lsTreeEdges || lrNT /= lsNonTreeEdges || lrCnt /= lsCount) $ do
          writeMutVar st $ st' {lsTreeEdges = lrT, lsNonTreeEdges = lrNT, lsCount = lrCnt}
        Nothing -> do
          (nT, nNT) <- getBitvectors lsNext
          nCnt <- count lsNext
          let newT = nT .|. lrT
              newNT = nNT .|. lrNT
              newCnt = lrCnt + nCnt
          when (newT /= lsTreeEdges || newNT /= lsNonTreeEdges || newCnt /= lsCount) $ do
            writeMutVar st $ st' {lsTreeEdges = newT, lsNonTreeEdges = newNT, lsCount = newCnt}
    LRNode {..} -> do
      (lT, lNT) <- getBitvectors lrnLeft
      lCnt <- count lrnLeft
      (rT, rNT) <- getBitvectors lrnRight
      rCnt <- count lrnRight
      let newT = lT .|. rT
          newNT = lNT .|. rNT
          newCnt = lCnt + rCnt
      when (newT /= lrnTreeEdges || newNT /= lrnNonTreeEdges || newCnt /= lrnCount) $ do
        writeMutVar st $ st' {lrnTreeEdges = newT, lrnNonTreeEdges = newNT, lrnCount = newCnt}
    LRLeaf {..} -> do
      (cT, cNT) <- getBitvectors lrlChild
      cCnt <- count lrlChild
      when (cT /= lrlTreeEdges || cNT /= lrlNonTreeEdges || cCnt /= lrlCount) $ do
        writeMutVar st $ st' {lrlTreeEdges = cT, lrlNonTreeEdges = cNT, lrlCount = cCnt}

getRoot :: (PrimMonad m, s ~ PrimState m) => StructuralTree b0 b1 b2 b3 b4 s v -> m (StructuralTree 'False 'True 'False 'False 'False s v)
getRoot st = do
  st' <- readMutVar st
  case st' of
    SLeaf {..} -> getRoot lParent
    SNode {..} -> case eq nParent st of
      Just Refl -> return st
      Nothing -> getRoot nParent
    LSNode {..} -> getRoot lsPrev
    LRNode {..} -> getRoot lrnParent
    LRLeaf {..} -> getRoot lrlParent

getRootLS :: (PrimMonad m, s ~ PrimState m) => StructuralTree 'False 'False 'True 'False 'False s v -> m (StructuralTree 'False 'False 'True 'False 'False s v)
getRootLS st = readMutVar st >>= \LSNode {..} -> do
  readMutVar lsPrev >>= \case
    LSNode {} -> getRootLS lsPrev
    SNode {} -> return st

getParentSNode :: (PrimMonad m, s ~ PrimState m) => StructuralTree b0 b1 b2 b3 b4 s v -> m (StructuralTree 'False 'True 'False 'False 'False s v)
getParentSNode st = do
  st' <- readMutVar st
  case st' of
    SLeaf {..} -> getParentSNode lParent
    SNode {..} -> return st
    LSNode {..} -> getParentSNode lsPrev
    LRNode {..} -> getParentSNode lrnParent
    LRLeaf {..} -> getParentSNode lrlParent

isRoot :: (PrimMonad m, s ~ PrimState m) => StructuralTree b0 b1 b2 b3 b4 s v -> m Bool
isRoot st = do
  st' <- readMutVar st
  case st' of
    SNode {..} -> case eq nParent st of
      Just _ -> return True
      Nothing -> return False
    _ -> return False

mergeST ::
  (PrimState m ~ s, PrimMonad m) => Graph s v ->
  StructuralTree 'False 'True 'False 'False 'False s v -> StructuralTree 'False 'True 'False 'False 'False s v -> m ()
mergeST graph ast bst = do
  ast' <- readMutVar ast
  bst' <- readMutVar bst
  bstRoot <- getRoot bst
  astRoot <- getRoot ast
  -- checkGraph graph
  -- checkValidFromRoot bstRoot
  -- checkGraph graph
  -- checkValidFromRoot astRoot
  newLS <- mergeLS (children ast') (children bst')
  modifyMutVar' ast $ \ast' -> ast' {children = newLS}
  -- checkNeq "mergeST" newLS ast
  modifyMutVar' newLS $ \LSNode {..} -> LSNode {lsPrev = ast, ..}
  propagate ast
  deleteNode graph bst
  -- checkGraph graph
  -- checkValidFromRoot astRoot
  -- checkGraph graph
  -- checkValidFromLeaf newLS
  -- checkGraph graph
  -- checkValidFromRoot ast
  writeMutVar bst $ error "mergeST"

deleteNode ::
  (PrimState m ~ s, PrimMonad m) => Graph s v -> StructuralTree 'False 'True 'False 'False 'False s v -> m ()
deleteNode graph node = do
  readMutVar node >>= \SNode {..} -> case eq nParent node of
      Just Refl -> return ()
      Nothing -> do
        readMutVar nParent >>= \LRLeaf {lrlCount} -> do
          (m'ls1, oldLs) <- deleteLR Nothing nParent
          -- checkGraph graph
          (m'ls2, parentS) <- deleteLS oldLs
          case (m'ls1, m'ls2) of
            (Nothing, Nothing) -> do
              deleteNode graph parentS
              -- checkGraph graph
            (Just ls1, Nothing) -> do
              -- checkNeq "del" ls1 parentS
              modifyMutVar' ls1 $ \LSNode {..} -> LSNode {lsPrev = parentS, ..}
              modifyMutVar' parentS $ \SNode {..} -> SNode {children = ls1, ..}
              propagate parentS
              -- checkValidFromLeaf ls1
            (Nothing, Just ls2) -> do
              -- checkNeq "del2" ls2 parentS
              modifyMutVar' ls2 $ \LSNode {..} -> LSNode {lsPrev = parentS, ..}
              modifyMutVar' parentS $ \SNode {..} -> SNode {children = ls2, ..}
              propagate parentS
              -- checkValidFromLeaf ls2
            (Just ls1, Just ls2) -> do
              ls3 <- mergeLS ls1 ls2
              -- checkNeq "del3" ls3 parentS
              modifyMutVar' ls3 $ \LSNode {..} -> LSNode {lsPrev = parentS, ..}
              modifyMutVar' parentS $ \SNode {..} -> SNode {children = ls3, ..}
              propagate parentS
              -- checkValidFromLeaf ls3
          writeMutVar node $ SNode {nParent = node, ..}

deleteLR ::
  forall m s v a3 a4.
  (PrimState m ~ s, PrimMonad m) =>
  Maybe (StructuralTree 'False 'False 'True 'False 'False s v) -> StructuralTree 'False 'False 'False a3 a4 s v ->
  m (Maybe (StructuralTree 'False 'False 'True 'False 'False s v), StructuralTree 'False 'False 'True 'False 'False s v)
deleteLR acc node = do
  readMutVar node >>= \case
    LRLeaf {lrlParent} -> readMutVar lrlParent >>= \case
      LSNode {} -> do
        return (acc, lrlParent)
      LRNode {lrnLeft,lrnRight} -> case eq lrnLeft node of
        Just Refl -> do
            lsNode <- newMutVar undefined
            (t1, nt1) <- maybe (return (0, 0)) getBitvectors acc
            c1 <- maybe (return 0) count acc
            (t2, nt2) <- getBitvectors lrnRight
            c2 <- count lrnRight
            case acc of
              Nothing ->
                writeMutVar lsNode $ LSNode lsNode lrnRight lsNode (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
              Just lsNext -> do
                writeMutVar lsNode $ LSNode lsNode lrnRight lsNext (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
                modifyMutVar' lsNext $ \LSNode {..} -> LSNode {lsPrev = lsNode, ..}
            modifyMutVar' lrnRight $ \case
              LRLeaf{..} -> LRLeaf {lrlParent=lsNode, ..}
              LRNode{..} -> LRNode {lrnParent=lsNode, ..}
            deleteLR (Just lsNode) lrlParent
        Nothing -> case eq lrnRight node of
          Just Refl -> do
            lsNode <- newMutVar undefined
            (t1, nt1) <- maybe (return (0, 0)) getBitvectors acc
            c1 <- maybe (return 0) count acc
            (t2, nt2) <- getBitvectors lrnLeft
            c2 <- count lrnLeft
            case acc of
              Nothing ->
                writeMutVar lsNode $ LSNode lsNode lrnLeft lsNode (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
              Just lsNext -> do
                writeMutVar lsNode $ LSNode lsNode lrnLeft lsNext (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
                modifyMutVar' lsNext $ \LSNode {..} -> LSNode {lsPrev = lsNode, ..}
            modifyMutVar' lrnLeft $ \case
              LRLeaf{..} -> LRLeaf {lrlParent=lsNode, ..}
              LRNode{..} -> LRNode {lrnParent=lsNode, ..}
            deleteLR (Just lsNode) lrlParent
          Nothing -> error "impossible"
    LRNode {lrnParent} -> readMutVar lrnParent >>= \case
      LSNode {} -> return (acc, lrnParent)
      LRNode {lrnLeft, lrnRight} -> case eq lrnLeft node of
        Just Refl -> do
            lsNode <- newMutVar undefined
            (t1, nt1) <- maybe (return (0, 0)) getBitvectors acc
            c1 <- maybe (return 0) count acc
            (t2, nt2) <- getBitvectors lrnRight
            c2 <- count lrnRight
            case acc of
              Nothing ->
                writeMutVar lsNode $ LSNode lsNode lrnRight lsNode (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
              Just lsNext -> do
                writeMutVar lsNode $ LSNode lsNode lrnRight lsNext (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
                modifyMutVar' lsNext $ \LSNode {..} -> LSNode {lsPrev = lsNode, ..}
            modifyMutVar' lrnRight $ \case
              LRLeaf{..} -> LRLeaf {lrlParent=lsNode, ..}
              LRNode{..} -> LRNode {lrnParent=lsNode, ..}
            deleteLR (Just lsNode) lrnParent
        Nothing -> case eq lrnRight node of
          Just Refl -> do
            lsNode <- newMutVar undefined
            (t1, nt1) <- maybe (return (0, 0)) getBitvectors acc
            c1 <- maybe (return 0) count acc
            (t2, nt2) <- getBitvectors lrnLeft
            c2 <- count lrnLeft
            case acc of
              Nothing ->
                writeMutVar lsNode $ LSNode lsNode lrnLeft lsNode (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
              Just lsNext -> do
                writeMutVar lsNode $ LSNode lsNode lrnLeft lsNext (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
                modifyMutVar' lsNext $ \LSNode {..} -> LSNode {lsPrev = lsNode, ..}
            modifyMutVar' lrnLeft $ \case
              LRLeaf{..} -> LRLeaf {lrlParent=lsNode, ..}
              LRNode{..} -> LRNode {lrnParent=lsNode, ..}
            deleteLR (Just lsNode) lrnParent
          Nothing -> error "impossible"

deleteLS ::
  (PrimState m ~ s, PrimMonad m) =>
  StructuralTree 'False 'False 'True 'False 'False s v -> m (Maybe (StructuralTree 'False 'False 'True 'False 'False s v), StructuralTree 'False 'True 'False 'False 'False s v)
deleteLS node = readMutVar node >>= \LSNode {lsPrev = lsPrev',..} ->
  if lsNext == node
    then readMutVar lsPrev' >>= \case
      prev'@LSNode {} -> do
        writeMutVar lsPrev' prev' {lsNext = lsPrev'}
        lsRoot <- getRootLS lsPrev'
        (,) (Just lsRoot) <$> getParentSNode lsRoot
      SNode {} -> do
        -- writeMutVar node $ error "deleteLS"
        return (Nothing, lsPrev')
    else do
      modifyMutVar' lsNext $ \LSNode {..} -> LSNode {lsPrev = lsPrev', ..}
      readMutVar lsPrev' >>= \case
        prev'@LSNode {} -> do
          writeMutVar lsPrev' prev' {lsNext = lsNext}
          lsRoot <- getRootLS lsPrev'
          (,) (Just lsRoot) <$> getParentSNode lsRoot
        SNode {} -> return (Just lsNext, lsPrev')

mergeLS ::
  (PrimState m ~ s, PrimMonad m) =>
  StructuralTree 'False 'False 'True 'False 'False s v ->
  StructuralTree 'False 'False 'True 'False 'False s v -> m (StructuralTree 'False 'False 'True 'False 'False s v)
mergeLS als bls = do
  als' <- readMutVar als
  bls' <- readMutVar bls
  aRank <- case als' of
    LSNode {..} -> rank lsLocalRankTree
  bRank <- case bls' of
    LSNode {..} -> rank lsLocalRankTree
  case compare aRank bRank of
    LT -> do
      if lsNext bls' == bls
        then do
          writeMutVar als $ case als' of
            LSNode {..} -> LSNode {lsPrev = bls, ..}
          writeMutVar bls $ case bls' of LSNode {..} -> LSNode { lsNext = als, lsPrev = bls, .. }
          update bls
          -- checkValidFromRoot bls
          return bls
        else do
          cls <- mergeLS als (lsNext bls')
          cls' <- readMutVar cls
          clrRank <- case cls' of
            LSNode {..} -> rank lsLocalRankTree
          blrRank <- case bls' of
            LSNode {..} -> rank lsLocalRankTree
          if clrRank == blrRank
            then do
              newLR <- case cls' of
                LSNode {lsLocalRankTree = clrt} -> case bls' of
                  LSNode {lsLocalRankTree = blrt} -> mergeLR clrt blrt
              modifyMutVar' newLR $ \case newLR'@LRNode{..} -> LRNode {lrnParent = cls, ..}
              writeMutVar cls $ case cls' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, lsPrev = cls, ..}
              update cls
              writeMutVar bls $ error "mergLS: bls"
              -- checkValidFromRoot cls
              return cls
            else do
              modifyMutVar' cls $ \case LSNode {..} -> LSNode {lsPrev = bls, ..}
              writeMutVar bls $ case bls' of LSNode {..} -> LSNode {lsNext = cls, lsPrev = bls, ..}
              update bls
              -- checkValidFromRoot bls
              return bls
    EQ -> do
      newLR <- case als' of
        LSNode {lsLocalRankTree = alrt} -> case bls' of
          LSNode {lsLocalRankTree = blrt} -> mergeLR alrt blrt
      case (lsNext als' == als, lsNext bls' == bls) of
        (_, True) -> do
          writeMutVar als $ case als' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, lsPrev = als, ..}
          modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = als, ..}
          update als
          writeMutVar bls $ error "mergeLS: EQ"
          -- checkValidFromRoot als
          return als
        (True, False) -> do
          writeMutVar bls $ case bls' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, lsPrev = bls, ..}
          update bls
          modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = bls, ..}
          writeMutVar als $ error "mergeLS: EQ als"
          -- checkValidFromRoot bls
          return bls
        (False, False) -> do
          cls <- mergeLS (lsNext als') (lsNext bls')
          writeMutVar als $ case als' of LSNode {..} -> LSNode {lsNext = cls, lsLocalRankTree = newLR, lsPrev = als, ..}
          modifyMutVar' cls $ \case LSNode {..} -> LSNode {lsPrev = als, ..}
          modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = als, ..}
          update als
          writeMutVar bls $ error "mergeLS: EQ False"
          -- checkValidFromRoot als
          return als
    GT -> do
      if lsNext als' == als
        then do
          writeMutVar bls $ case bls' of LSNode {..} -> LSNode {lsPrev = als, ..}
          writeMutVar als $ case als' of LSNode {..} -> LSNode {lsNext = bls, lsPrev = als, ..}
          update als
          -- checkValidFromRoot als
          return als
        else do
          cls <- mergeLS (lsNext als') bls
          cls' <- readMutVar cls
          clrRank <- case cls' of
            LSNode {..} -> rank lsLocalRankTree
          alrRank <- case als' of
            LSNode {..} -> rank lsLocalRankTree
          if clrRank == alrRank
            then do
              newLR <- case cls' of
                LSNode {lsLocalRankTree = clrt} -> case als' of
                  LSNode {lsLocalRankTree = alrt} -> mergeLR clrt alrt
              modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = cls, ..}
              writeMutVar cls $ case cls' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, lsPrev = cls, ..}
              update cls
              writeMutVar als $ error "mergLS: GT: als"
              -- checkValidFromRoot cls
              return cls
            else do
              modifyMutVar' cls $ \case LSNode {..} -> LSNode {lsPrev = als, ..}
              writeMutVar als $ case als' of LSNode{..} -> LSNode {lsNext = cls, lsPrev = als, ..}
              update als
              -- checkValidFromRoot als
              return als

mergeLR ::
  (PrimMonad m, s ~ PrimState m) =>
  StructuralTree 'False 'False 'False a3 a4 s v -> StructuralTree 'False 'False 'False b3 b4 s v -> m (StructuralTree 'False 'False 'False 'True 'False s v)
mergeLR alr blr = do
  newLR <- newMutVar undefined
  (at, ant) <- getBitvectors alr
  (bt, bnt) <- getBitvectors blr
  aCount <- count alr
  bCount <- count blr
  writeMutVar newLR $ LRNode newLR alr blr (at .|. bt) (ant .|. bnt) (aCount + bCount)
  modifyMutVar' alr $ \case
    LRNode {..} -> LRNode {lrnParent = newLR, ..}
    LRLeaf {..} -> LRLeaf {lrlParent = newLR, ..}
  modifyMutVar' blr $ \case
    LRNode {..} -> LRNode {lrnParent = newLR, ..}
    LRLeaf {..} -> LRLeaf {lrlParent = newLR, ..}
  return newLR

link :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => Graph s v -> v -> v -> m ()
link g@Graph {..} a b = do
  -- checkGraph g
  ae <- readMutVar allEdges
  case HMS.lookup (a, b) ae of
    Just _ -> return ()
    Nothing -> do
      av <- readMutVar allVertices
      conn <- connected' g a b
      when (a == b && conn /= Just True) $ error $ show conn
      case conn of
        Nothing -> error "link: TODO"
        Just True -> do
          let Just ast = Map.lookup a av
          let Just bst = Map.lookup b av
          -- checkValidFromLeaf ast
          -- checkValidFromLeaf bst
          ast' <- readMutVar ast
          bst' <- readMutVar bst
          writeMutVar ast $! ast' {groups = Map.insertWith Set.union (NonTreeEdge, 0) (Set.singleton b) (groups ast'), lNonTreeEdges = setBit (lNonTreeEdges ast') 0}
          writeMutVar bst $! bst' {groups = Map.insertWith Set.union (NonTreeEdge, 0) (Set.singleton a) (groups bst'), lNonTreeEdges = setBit (lNonTreeEdges bst') 0}
          case ast' of SLeaf {lParent} -> propagate lParent
          case bst' of SLeaf {lParent} -> propagate lParent
          writeMutVar allEdges $ HMS.insert (a, b) (NonTreeEdge, 0) $ HMS.insert (b, a) (NonTreeEdge, 0) ae
          modifyMutVar' numEdges succ
          -- checkValidFromLeaf ast
          -- checkValidFromLeaf bst
          -- checkGraph g
        Just False -> do
          let Just ast = Map.lookup a av
          let Just bst = Map.lookup b av
          -- checkValidFromLeaf ast
          -- checkValidFromLeaf bst
          ast' <- readMutVar ast
          bst' <- readMutVar bst
          writeMutVar ast $! ast' {groups = Map.insertWith Set.union (TreeEdge, 0) (Set.singleton b) (groups ast'), lTreeEdges = setBit (lTreeEdges ast') 0}
          writeMutVar bst $! bst' {groups = Map.insertWith Set.union (TreeEdge, 0) (Set.singleton a) (groups bst'), lTreeEdges = setBit (lTreeEdges bst') 0}
          aRoot <- getRoot ast
          bRoot <- getRoot bst
          mergeST g aRoot bRoot
          readMutVar ast >>= \case SLeaf {lParent} -> propagate lParent
          readMutVar bst >>= \case SLeaf {lParent} -> propagate lParent
          writeMutVar allEdges $ HMS.insert (a, b) (TreeEdge, 0) $ HMS.insert (b, a) (TreeEdge, 0) ae
          modifyMutVar' numEdges succ
          -- checkValidFromLeaf ast
          -- checkValidFromLeaf bst
          -- checkGraph g

-- Go to the highest ancestor with level greater or equal to the given level. Return Nothing if it would be a leaf node or if the input node's level is too low
goToLevelGEQ ::
  (PrimState m ~ s, PrimMonad m) =>
  Level -> StructuralTree a0 a1 False False False s v -> m (Maybe (StructuralTree False True False False False s v))
goToLevelGEQ l a = do
  a' <- readMutVar a
  case a' of
    SLeaf {lParent} -> getParentSNode lParent >>= goToLevelGEQ l
    SNode {nParent, level} ->
      case compare l level of
        GT -> return Nothing
        EQ -> return $ Just a
        LT -> do
          case eq a nParent of
            Just Refl -> error $ show ("goToLevelGEQ", l, level)
            Nothing -> return ()
          getParentSNode nParent >>= goToLevelGEQ l >>= \case
            Nothing -> return $ Just a
            Just b -> return $ Just b

getLevel ::
  (PrimState m ~ s, PrimMonad m) =>
  Level -> StructuralTree a0 a1 False False False s v -> m (StructuralTree False True False False False s v)
getLevel lvl n = do
  parent <- readMutVar n >>= \case
    SLeaf {..} -> getParentSNode lParent
    SNode {..} -> getParentSNode nParent
  readMutVar parent >>= \parent' -> do
    case compare lvl (level parent') of
      LT -> getLevel lvl parent
      EQ -> return parent
      GT -> do
        n' <- readMutVar n
        case n' of
          SNode {..} -> do
            let t = nTreeEdges
                nt = nNonTreeEdges
                cnt = nCount
            lsNode <- newMutVar undefined
            newSNode <- newMutVar undefined
            lrLeaf <- newMutVar $ LRLeaf lsNode n t nt cnt
            writeMutVar lsNode $ LSNode newSNode lrLeaf lsNode t nt cnt
            -- when (lvl /= 0) $ checkNeq "get" nParent newSNode
            writeMutVar newSNode $ SNode nParent lsNode lvl t nt cnt
            modifyMutVar' nParent $ \LRLeaf {..} -> LRLeaf {lrlChild = newSNode, ..}
            -- when (level /= 0) $ checkNeq "get2" lrLeaf n
            writeMutVar n $ SNode {nParent = lrLeaf, ..}
            return newSNode
          SLeaf {..} -> do
            let t = lTreeEdges
                nt = lNonTreeEdges
                cnt = 1
            lsNode <- newMutVar undefined
            newSNode <- newMutVar undefined
            lrLeaf <- newMutVar $ LRLeaf lsNode n t nt cnt
            writeMutVar lsNode $ LSNode newSNode lrLeaf lsNode t nt cnt
            -- when (lvl /= 0) $ checkNeq "get" lParent newSNode
            writeMutVar newSNode $ SNode lParent lsNode lvl t nt cnt
            modifyMutVar' lParent $ \LRLeaf {..} -> LRLeaf {lrlChild = newSNode, ..}
            writeMutVar n $ SLeaf {lParent = lrLeaf, ..}
            return newSNode

findSmaller ::
  (PrimMonad m, s ~ PrimState m, Ord v) =>
  Graph s v -> Level -> v -> v -> m (Either (Int, Set (v,v)) (Int, Set (v, v)))
findSmaller g lvl v1 v2 = go (SizeWithoutState 0 Set.empty [(v2, v1)]) (SizeWithoutState 0 Set.empty [(v1, v2)])
  where
    go sws1 sws2
      | sizeSoFar sws1 <= sizeSoFar sws2 =
          case processQueue sws1 of
            [] -> return $ Left (sizeSoFar sws1, edgesSoFar sws1)
            ((a, b):xs) -> do
              (cnt, newNeighbours) <- sizeWithoutStep g lvl a b
              go (SizeWithoutState (sizeSoFar sws1 + cnt) (Set.union (edgesSoFar sws1) newNeighbours)
                  (Set.toList newNeighbours ++ xs)) sws2
      | otherwise =
          case processQueue sws2 of
            [] -> return $ Right (sizeSoFar sws2, edgesSoFar sws2)
            ((a, b):xs) -> do
              (cnt, newNeighbours) <- sizeWithoutStep g lvl a b
              go sws1 (SizeWithoutState (sizeSoFar sws2 + cnt) (Set.union (edgesSoFar sws2) newNeighbours)
                       (Set.toList newNeighbours ++ xs))

data SizeWithoutState v = SizeWithoutState
  { sizeSoFar :: {-# UNPACK #-} !Int
  , edgesSoFar :: !(Set (v, v))
  , processQueue :: !([(v, v)])
  }

sizeWithoutStep ::
  forall m s v. (PrimMonad m, s ~ PrimState m, Ord v) =>
  Graph s v -> Level -> v -> v -> m (Int, Set (v, v))
sizeWithoutStep graph@Graph{..} level a b = do
  when (a == b) $ error "sizeWithout: matching vertices"
  av <- readMutVar allVertices
  let Just bl = Map.lookup b av
  bSucc <- goToLevelGEQ (level + 1) bl
  bEdges <- case bSucc of
    Nothing -> foldSize bl
    Just bAncestor -> do
      foldSize bAncestor
  bSize <-  maybe (return 1) count bSucc
  return (bSize, bEdges)
  where
    foldSize :: StructuralTree a0 a1 a2 a3 a4 s v -> m (Set (v, v))
    foldSize node = do
      readMutVar node >>= \case
        SLeaf {lTreeEdges, groups, vertex}
          | testBit lTreeEdges level -> case Map.lookup (TreeEdge, level) groups of
              Nothing -> return mempty
              Just s -> do
                let neighbours = Set.filter (\v -> (vertex, v) /= (a, b) && (vertex, v) /= (b, a)) s
                when (Set.filter (== vertex) neighbours /= Set.empty) $ error "sizeWithout: matching neighbour"
                return $ Set.map (vertex,) neighbours -- Only one-sided, not the reverse pair
          | otherwise -> return mempty
        SNode {nTreeEdges, children}
          | testBit nTreeEdges level -> foldSize children
          | otherwise -> return mempty
        LSNode {lsTreeEdges, lsNext, lsLocalRankTree}
          | testBit lsTreeEdges level -> case eq lsNext node of
              Just _ -> foldSize lsLocalRankTree
              Nothing -> mappend <$> foldSize lsLocalRankTree <*> foldSize lsNext
          | otherwise -> return mempty
        LRNode {lrnTreeEdges, lrnLeft, lrnRight}
          | testBit lrnTreeEdges level -> mappend <$> foldSize lrnLeft <*> foldSize lrnRight
          | otherwise -> return mempty
        LRLeaf {lrlTreeEdges, lrlChild}
          | testBit lrlTreeEdges level -> foldSize lrlChild
          | otherwise -> return mempty

cut :: forall v s m. (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => Graph s v -> v -> v -> m ()
cut graph@Graph {..} a b = do
  -- checkGraph graph
  ae <- readMutVar allEdges
  av <- readMutVar allVertices
  writeMutVar allEdges $ HMS.delete (a, b) $ HMS.delete (b, a) ae
  let Just al = Map.lookup a av
  let Just bl = Map.lookup b av
  -- checkValidFromLeaf al
  -- checkValidFromLeaf bl
  case HMS.lookup (a, b) ae of
    Nothing -> return ()
    Just (NonTreeEdge, level) -> do
      modifyMutVar' al $ \al' -> al' {groups = Map.adjust (Set.delete b) (NonTreeEdge, level) $ groups al'}
      modifyMutVar' bl $ \bl' -> bl' {groups = Map.adjust (Set.delete a) (NonTreeEdge, level) $ groups bl'}
      propagateLeaf al level
      propagateLeaf bl level
      -- checkValidFromLeaf al
      -- checkValidFromLeaf bl
      -- checkGraph graph
    Just (TreeEdge, level) -> do
      modifyMutVar' al $ \SLeaf {..} ->
        let group = Set.delete b $ Map.findWithDefault Set.empty (TreeEdge, level) groups
            newGroups = if Set.null group then Map.delete (TreeEdge, level) groups else Map.insert (TreeEdge, level) group groups in
        SLeaf {groups = newGroups, ..}
      modifyMutVar' bl $ \SLeaf {..} ->
        let group = Set.delete a $ Map.findWithDefault Set.empty (TreeEdge, level) groups
            newGroups = if Set.null group then Map.delete (TreeEdge, level) groups else Map.insert (TreeEdge, level) group groups in
        SLeaf {groups = newGroups, ..}
      propagateLeaf al level
      propagateLeaf bl level
      when (a == b) $ error "cut: self-loop is tree edge"
      -- checkGraph graph
      handleTreeEdge level
      -- checkValidFromLeaf al
      -- checkValidFromLeaf bl
      -- checkGraph graph
  where
    handleTreeEdge lvl = do
      av <- readMutVar allVertices
      smaller <- findSmaller graph lvl a b
      case smaller of
        Left (aSize, aEdges) -> do
          let Just al = Map.lookup a av
          handleSmaller a lvl al aEdges
        Right (bSize, bEdges) -> do
          let Just bl = Map.lookup b av
          handleSmaller b lvl bl bEdges

    handleSmaller vertex lvl al aEdges = do
          alVs <- subtreeVertices al
          aNext <- getLevel (lvl + 1) al
          aNextVs <- subtreeVertices aNext
          -- checkGraph graph
          mapM_ (increaseLevel lvl) $ Set.toList aEdges
          -- checkGraph graph
          Just aAncestor <- goToLevelGEQ lvl al -- Should be exactly lvl
          !_ <- readMutVar aAncestor >>= \SNode {..} -> if level == lvl then return () else error $ "bad level " ++ show (level, lvl)
          aNextParent <- readMutVar aNext >>= \SNode {..} -> getParentSNode nParent
          -- checkGraph graph
          m'rep <- findReplacement lvl aNext aNext
          case m'rep of
            Just (rep1, rep2) -> return ()
              -- checkGraph graph
            Nothing -> do
              -- checkGraph graph
              deleteNode graph aNext
              -- checkValidFromLeaf aNext
              -- checkGraph graph
              (t, nt) <- getBitvectors aNext
              cnt <- count aNext
              lr <- newMutVar undefined
              ls <- newMutVar undefined
              if lvl == 0
                then do
                  sn <- newMutVar undefined
                  writeMutVar lr $ LRLeaf ls aNext t nt cnt
                  writeMutVar ls $ LSNode sn lr ls t nt cnt
                  writeMutVar sn $ SNode sn ls lvl t nt cnt
                  modifyMutVar' aNext $ \SNode {..} -> SNode {nParent = lr, ..}
                  -- checkValidFromLeaf al
                  -- checkGraph graph
                else do
                  writeMutVar lr $ LRLeaf ls aNext t nt cnt
                  modifyMutVar' aNext $ \SNode {..} -> SNode {nParent = lr, ..}
                  aGrand <- getLevel (lvl - 1) aAncestor
                  writeMutVar ls $ LSNode aGrand lr ls t nt cnt
                  readMutVar aGrand >>= \SNode {..} -> do
                    newLS <- mergeLS children ls
                    modifyMutVar' newLS $ \LSNode {..} -> LSNode {lsPrev = aGrand, ..}
                    modifyMutVar' aGrand $ \SNode {..} -> SNode {children = newLS, ..}
                    -- checkValidFromRoot aGrand
                    -- checkValidFromLeaf newLS
                    -- checkGraph graph
                    handleTreeEdge (lvl - 1)
    increaseLevel lvl (v1, v2) = do
      when (v1 == v2) $ error $ show ("increaseLevel: matching vertex")
      av <- readMutVar allVertices
      let Just v1' = Map.lookup v1 av
      let Just v2' = Map.lookup v2 av
      modifyMutVar' v1' $ \v1'' ->
        let Just s = Map.lookup (TreeEdge, lvl) (groups v1'') in
        let s' = Set.delete v2 s in
        if Set.null s'
          then v1'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v2) $
                      Map.delete (TreeEdge, lvl) (groups v1'')}
          else v1'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v2) $
                      Map.insert (TreeEdge, lvl) s' (groups v1'')}
      when (v1 /= v2) $ modifyMutVar' v2' $ \v2'' ->
        let Just s = Map.lookup (TreeEdge, lvl) (groups v2'') in
        let s' = Set.delete v1 s in
        if Set.null s'
          then v2'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v1) $
                      Map.delete (TreeEdge, lvl) (groups v2'')}
          else v2'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v1) $
                      Map.insert (TreeEdge, lvl) s' (groups v2'')}
      propagateLeaf v1' (lvl + 1)
      propagateLeaf v2' (lvl + 1)
      v1Ancestor <- getLevel (lvl + 1) v1'
      v2Ancestor <- getLevel (lvl + 1) v2'
      -- checkGraph graph
      mergeST graph v1Ancestor v2Ancestor
      -- checkGraph graph
      modifyMutVar' allEdges $ HMS.insert (v1, v2) (TreeEdge, lvl + 1) . HMS.insert (v2, v1) (TreeEdge, lvl + 1)
    findReplacement :: Int -> StructuralTree a0 a1 a2 a3 a4 s v -> StructuralTree 'False 'True 'False 'False 'False s v -> m (Maybe (v, v))
    findReplacement lvl node myNext = readMutVar node >>= \case
      SNode {..} -> case testBit nNonTreeEdges lvl of
        False -> return Nothing
        True -> findReplacement lvl children myNext
      SLeaf {..} -> case testBit lNonTreeEdges lvl of
        False -> return Nothing
        True -> do
          av <- readMutVar allVertices
          let neighbours = Map.findWithDefault Set.empty (NonTreeEdge, lvl) groups
          let go res@(Just _) _ = return res
              go Nothing neighbour = do
                let Just nNode = Map.lookup neighbour av
                nNext <- goToLevelGEQ (lvl + 1) nNode
                if nNext /= Just myNext
                  then do
                    let newNodeGroup = Set.delete neighbour $ Map.findWithDefault Set.empty (NonTreeEdge, lvl) groups
                        newNodeGroups = Map.insertWith Set.union (TreeEdge, lvl) (Set.singleton neighbour) $
                          if Set.null newNodeGroup
                            then Map.delete (NonTreeEdge, lvl) groups
                            else Map.insert (NonTreeEdge, lvl) newNodeGroup groups
                    when (neighbour == vertex) $ error "findReplacement: matching"
                    writeMutVar node $ SLeaf {groups = newNodeGroups, ..}
                    modifyMutVar' nNode $ \sl@SLeaf {groups} ->
                      let newNeighNodeGroup = Set.delete vertex $ Map.findWithDefault Set.empty (NonTreeEdge, lvl) groups
                          newNeighNodeGroups = Map.insertWith Set.union (TreeEdge, lvl) (Set.singleton vertex) $
                            if Set.null newNeighNodeGroup
                              then Map.delete (NonTreeEdge, lvl) groups
                              else Map.insert (NonTreeEdge, lvl) newNeighNodeGroup groups in
                        sl {groups = newNeighNodeGroups}
                    propagateLeaf node lvl
                    propagateLeaf nNode lvl
                    modifyMutVar' allEdges $ HMS.insert (vertex, neighbour) (TreeEdge, lvl) . HMS.insert (neighbour, vertex) (TreeEdge, lvl)
                    return $ Just (vertex, neighbour)
                  else return Nothing -- TODO increase level
          foldM go Nothing (Set.toList neighbours)
      LSNode {..} -> case testBit lsNonTreeEdges lvl of
        False -> return Nothing
        True -> do
          lr <- findReplacement lvl lsLocalRankTree myNext
          case lr of
            Nothing -> if lsNext == node then return empty else findReplacement lvl lsNext myNext
            Just _ -> return lr
      LRNode {..} -> case testBit lrnNonTreeEdges lvl of
        False -> return Nothing
        True -> do
          l <- findReplacement lvl lrnLeft myNext
          case l of
            Nothing -> findReplacement lvl lrnRight myNext
            Just _ -> return l
      LRLeaf {..} -> case testBit lrlNonTreeEdges lvl of
        False -> return Nothing
        True -> do
          findReplacement lvl lrlChild myNext

subtreeVertices :: (PrimMonad m, s ~ PrimState m) => StructuralTree a0 a1 a2 a3 a4 s v -> m [v]
subtreeVertices node = readMutVar node >>= \case
  SNode {..} -> subtreeVertices children
  SLeaf {..} -> return [vertex]
  LSNode {..} -> do
      lr <- subtreeVertices lsLocalRankTree
      next <- if lsNext == node then return [] else subtreeVertices lsNext
      return $ lr ++ next
  LRNode {..} -> do
      l <- subtreeVertices lrnLeft
      r <- subtreeVertices lrnRight
      return $ l ++ r
  LRLeaf {..} -> subtreeVertices lrlChild

checkValidFromLeaf :: (PrimState m ~ s, PrimMonad m) => StructuralTree a0 a1 a2 a3 a4 s v -> m ()
checkValidFromLeaf node = readMutVar node >>= \case
  SNode {nParent} -> readMutVar nParent >>= \case
    SNode {level}
      | isJust (eq nParent node) -> return ()
      | otherwise -> error $ show ("bad 1", level)
    LRLeaf {lrlChild} -> if isJust (eq lrlChild node) then checkValidFromLeaf nParent else error "bad 2"
  SLeaf {lParent} -> readMutVar lParent >>= \LRLeaf {..} ->
    if isJust (eq lrlChild node) then checkValidFromLeaf lParent else error "bad 3"
  LSNode {lsPrev}
    | isJust (eq lsPrev node) -> return () -- error "bad 10"
    | otherwise -> readMutVar lsPrev >>= \case
        LSNode {lsNext}
          | lsNext /= node -> error "bad 4"
          | otherwise -> checkValidFromLeaf lsPrev
        SNode {children} -> if children == node then checkValidFromLeaf lsPrev else error "bad 5"
  LRNode {lrnParent} -> readMutVar lrnParent >>= \case
    LRNode {lrnLeft, lrnRight} | isJust (eq lrnLeft node) || isJust (eq lrnRight node) -> checkValidFromLeaf lrnParent
                               | otherwise -> error "bad 6"
    LSNode {lsLocalRankTree} | isJust (eq lsLocalRankTree node) -> checkValidFromLeaf lrnParent
                             | otherwise -> error "bad 7"
  LRLeaf {lrlParent} -> readMutVar lrlParent >>= \case
    LRNode {lrnLeft, lrnRight} | isJust (eq lrnLeft node) || isJust (eq lrnRight node) -> checkValidFromLeaf lrlParent
                               | otherwise -> error "bad 8"
    LSNode {lsLocalRankTree} | isJust (eq lsLocalRankTree node) -> checkValidFromLeaf lrlParent
                             | otherwise -> error "bad 9"

checkValidFromRoot :: (PrimState m ~ s, PrimMonad m) => StructuralTree a0 a1 a2 a3 a4 s v -> m ()
checkValidFromRoot node = readMutVar node >>= \case
  SNode {children, level} -> readMutVar children >>= \case
    LSNode {lsPrev}
      | isJust (eq lsPrev node) -> checkValidFromRoot children
      | otherwise -> error $ show ("rad 1", level)
  SLeaf {} -> return ()
  LSNode {lsNext, lsLocalRankTree} -> do
    when (lsNext /= node) $ do
      readMutVar lsNext >>= \LSNode {lsPrev} ->
        if not (isJust (eq lsPrev node)) then error $ show ("rad 2") else checkValidFromRoot lsNext
    readMutVar lsLocalRankTree >>= \case
      LRNode {lrnParent} -> if isJust (eq lrnParent node) then checkValidFromRoot lsLocalRankTree else error "rad 3"
      LRLeaf {lrlParent} -> if isJust (eq lrlParent node) then checkValidFromRoot lsLocalRankTree else error "rad 4"
  LRNode {lrnLeft, lrnRight} -> do
    () <- readMutVar lrnLeft >>= \case
      LRNode {lrnParent} -> case eq lrnParent node of
        Just Refl -> checkValidFromRoot lrnLeft
        Nothing -> error "rad 5"
      LRLeaf {lrlParent} -> case eq lrlParent node of
        Just Refl -> checkValidFromRoot lrnLeft
        Nothing -> error "rad 5"
    readMutVar lrnRight >>= \case
      LRNode {lrnParent} -> case eq lrnParent node of
        Just Refl -> checkValidFromRoot lrnRight
        Nothing -> error "rad 5"
      LRLeaf {lrlParent} -> case eq lrlParent node of
        Just Refl -> checkValidFromRoot lrnRight
        Nothing -> error "rad 5"
  LRLeaf {lrlChild} -> readMutVar lrlChild >>= \case
    SNode {nParent} | isJust (eq nParent node) -> checkValidFromRoot lrlChild
                    | otherwise -> error "rad 8"
    SLeaf {lParent} | isJust (eq lParent node) -> checkValidFromRoot lrlChild
                    | otherwise -> error "rad 9"

checkGraph :: (PrimState m ~ s, PrimMonad m) => Graph s v -> m ()
checkGraph Graph {..} = do
  av <- readMutVar allVertices
  traceShowM ("checkGraph", Map.size av)
  mapM_ checkValidFromLeaf (Map.elems av)
