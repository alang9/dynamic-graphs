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

import Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.HashMap.Strict               as HMS
import Data.Hashable (Hashable)
import Data.Map as Map
import Data.Monoid
import Data.Primitive.MutVar
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality
import Unsafe.Coerce

import qualified Data.Graph.Dynamic.EulerTour      as ET
import qualified Data.Graph.Dynamic.Internal.Splay as Splay


data Graph s v = Graph
  { numEdges :: !(MutVar s Int)
  , allEdges :: !(MutVar s (HMS.HashMap (v, v) (EdgeType, Level)))
  , allVertices :: !(MutVar s (Map v (StructuralTree 'True 'False 'False 'False 'False s v)))
  }

type Level = Int

type Rank = Int

data ST (nt0 :: Bool) (nt1 :: Bool) (nt2 :: Bool) (nt3 :: Bool) (nt4 :: Bool) s v where
  SLeaf ::
    { lParent :: StructuralTree 'False 'False 'False 'False a4 s v
    , vertex :: v
    , groups :: Map (EdgeType, Level) (Set v)
    , lTreeEdges :: Int
    , lNonTreeEdges :: Int
    }
    -> ST 'True b1 b2 b3 b4 s v
  SNode ::
    { nParent :: StructuralTree 'False a1 'False 'False a4 s v
    , level :: !Int
    , nTreeEdges :: !Int
    , nNonTreeEdges :: !Int
    , children :: StructuralTree 'False 'False 'True 'False 'False s v
    , sCount :: !Int
    }
    -> ST 'False 'True 'False 'False 'False s v
  LSNode ::
    { lsPrev :: StructuralTree 'False nt nt' 'False 'False s v
    , lsLocalRankTree :: StructuralTree 'False 'False 'False a3 a4 s v
    , lsNext :: StructuralTree 'False 'False 'True 'False 'False s v
    , lsTreeEdges :: Int
    , lsNonTreeEdges :: Int
    , lsCount :: !Int
    }
    -> ST 'False 'False 'True 'False 'False s v
  LRNode ::
    { lrnParent :: StructuralTree 'False 'False nt0 nt1 'False s v
    , lrnLeft :: StructuralTree 'False 'False 'False nt4 a4 s v
    , lrnRight :: StructuralTree 'False 'False 'False nt7 a4' s v
    , lrnTreeEdges :: Int
    , lrnNonTreeEdges :: Int
    , lrnCount :: !Int
    }
    -> ST nt' nt'' nt''' 'True b4 s v
  LRLeaf ::
    { lrlParent :: StructuralTree 'False 'False a2 a3 'False s v
    , lrlChild :: StructuralTree a0 a1 'False 'False 'False s v
    , lrlTreeEdges :: !Int
    , lrlNonTreeEdges :: !Int
    , lrlCount :: !Int
    }
    -> ST 'False 'False b2 b3 'True s v

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
  writeMutVar sn $ SNode sn 0 0 0 ls 1
  writeMutVar ls $ LSNode sn lrl ls 0 0 1
  writeMutVar lrl $ LRLeaf ls sl 0 0 1
  writeMutVar sl $ SLeaf lrl v Map.empty 0 0
  return sl

insertVertex ::
  (Eq v, PrimMonad m, Ord v) => Graph (PrimState m) v -> v -> m ()
insertVertex Graph {..} v = do
  vs <- readMutVar allVertices
  case Map.lookup v vs of
    Nothing -> do
      newSTree <- singletonST v
      writeMutVar allVertices $ Map.insert v newSTree vs
      modifyMutVar' numEdges succ
    Just _ -> return ()

connected ::
  (Eq v, PrimMonad m, Ord v) => Graph (PrimState m) v -> v -> v -> m (Maybe Bool)
connected Graph {..} a b = do
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
    SNode {..} -> return sCount
    LSNode {..} -> return lsCount
    LRNode {..} -> return lrnCount
    LRLeaf {..} -> return lrlCount

rank :: (PrimState m ~ s, PrimMonad m) => StructuralTree b0 b1 b2 b3 b4 s v -> m Int
rank a = logBase2 <$> count a

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
      writeMutVar st $ st' {nTreeEdges = newT, nNonTreeEdges = newNT, sCount = childrenCount}
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
  (PrimState m ~ s, PrimMonad m) =>
  StructuralTree 'False 'True 'False 'False 'False s v -> StructuralTree 'False 'True 'False 'False 'False s v -> m ()
mergeST ast bst = do
  ast' <- readMutVar ast
  bst' <- readMutVar bst
  newLS <- mergeLS (children ast') (children bst')
  modifyMutVar' ast $ \ast' -> ast' {children = newLS}
  modifyMutVar' newLS $ \LSNode {..} -> LSNode {lsPrev = ast, ..}
  deleteNode bst

deleteNode ::
  (PrimState m ~ s, PrimMonad m) => StructuralTree 'False 'True 'False 'False 'False s v -> m ()
deleteNode node = do
  readMutVar node >>= \SNode {..} -> case eq nParent node of
      Just Refl -> return ()
      Nothing ->
        readMutVar nParent >>= \LRLeaf {} -> do
          (m'ls1, oldLs) <- deleteLR Nothing nParent
          (m'ls2, parentS) <- deleteLS oldLs
          undefined

deleteLR ::
  (PrimState m ~ s, PrimMonad m) =>
  Maybe (StructuralTree 'False 'False 'True 'False 'False s v) -> StructuralTree 'False 'False a2 a3 a4 s v ->
  m (Maybe (StructuralTree 'False 'False 'True 'False 'False s v), StructuralTree 'False 'False 'True 'False 'False s v)
deleteLR acc node = do
  readMutVar node >>= \case
    LRLeaf {lrlParent} -> readMutVar lrlParent >>= \case
      LSNode {} -> return (acc, lrlParent)
      LRNode {..} -> case eq lrnLeft node of
        Just Refl -> do
            lsNode <- newMutVar undefined
            (t1, nt1) <- maybe (return (0, 0)) getBitvectors acc
            c1 <- maybe (return 0) count acc
            (t2, nt2) <- getBitvectors lrnRight
            c2 <- count lrnRight
            writeMutVar lsNode $ LSNode lsNode lrnRight (maybe lsNode id acc) (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
            modifyMutVar lrnRight $ \case
              LRLeaf{..} -> LRLeaf {lrlParent=lsNode, ..}
              LRNode{..} -> LRNode {lrnParent=lsNode, ..}
            deleteLR (Just lsNode) lrlParent
        Nothing -> case eq lrnRight node of
          Just Refl -> do
            lsNode <- newMutVar undefined
            (t1, nt1) <- maybe (return (0, 0)) getBitvectors acc
            c1 <- maybe (return 0) count acc
            (t2, nt2) <- getBitvectors lrnRight
            c2 <- count lrnLeft
            writeMutVar lsNode $ LSNode lsNode lrnLeft (maybe lsNode id acc) (t1 .|. t2) (nt1 .|. nt2) (c1 + c2)
            modifyMutVar lrnLeft $ \case
              LRLeaf{..} -> LRLeaf {lrlParent=lsNode, ..}
              LRNode{..} -> LRNode {lrnParent=lsNode, ..}
            deleteLR (Just lsNode) lrlParent
          Nothing -> error "impossible"
    LRNode {} -> undefined

deleteLS ::
  (PrimState m ~ s, PrimMonad m) =>
  StructuralTree 'False 'False 'True 'False 'False s v -> m (Maybe (StructuralTree 'False 'False 'True 'False 'False s v), StructuralTree 'False 'True 'False 'False 'False s v)
deleteLS node = readMutVar node >>= \LSNode {..} ->
  if lsNext == node
    then readMutVar lsPrev >>= \case
      prev'@LSNode {} -> do
        writeMutVar lsPrev prev' {lsNext = lsPrev}
        (,) (Just lsPrev) <$> getParentSNode lsPrev
      SNode {..} -> return (Nothing, lsPrev)
    else do
      modifyMutVar' lsNext $ \LSNode {..} -> LSNode {lsPrev = lsPrev, ..}
      readMutVar lsPrev >>= \case
        prev'@LSNode {} -> do
          writeMutVar lsPrev prev' {lsNext = lsNext}
          (,) (Just lsPrev) <$> getParentSNode lsPrev
        SNode {..} -> return (Just lsNext, lsPrev)

mergeLS ::
  (PrimState m ~ s, PrimMonad m) =>
  StructuralTree 'False 'False 'True 'False 'False s v ->
  StructuralTree 'False 'False 'True 'False 'False s v -> m (StructuralTree 'False 'False 'True 'False 'False s v)
mergeLS als bls = do
  als' <- readMutVar als
  bls' <- readMutVar bls
  aCount <- case als' of
    LSNode {..} -> count lsLocalRankTree
  bCount <- case bls' of
    LSNode {..} -> count lsLocalRankTree
  case compare aCount bCount of
    LT -> do
      if lsNext bls' == bls
        then do
          writeMutVar als $ case als' of
            LSNode {..} -> LSNode {lsPrev = bls, ..}
          writeMutVar bls $ bls' {lsNext = als}
          return bls
        else do
          cls <- mergeLS als (lsNext bls')
          cls' <- readMutVar cls
          clrRank <- case cls' of
            LSNode {..} -> rank lsLocalRankTree
          blrRank <- case bls' of
            LSNode {..} -> rank $ lsLocalRankTree
          if clrRank == blrRank
            then do
              newLR <- case cls' of
                LSNode {lsLocalRankTree = clrt} -> case bls' of
                  LSNode {lsLocalRankTree = blrt} -> mergeLR clrt blrt
              modifyMutVar' newLR $ \case newLR'@LRNode{..} -> LRNode {lrnParent = cls, ..}
              writeMutVar cls $ case cls' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, ..}
              return cls
            else do
              modifyMutVar' cls $ \case LSNode {..} -> LSNode {lsPrev = bls, ..}
              writeMutVar bls $ bls' {lsNext = cls}
              return bls
    EQ -> do
      newLR <- case als' of
        LSNode {lsLocalRankTree = alrt} -> case bls' of
          LSNode {lsLocalRankTree = blrt} -> mergeLR alrt blrt
      case (lsNext als' == als, lsNext bls' == bls) of
        (_, True) -> do
          writeMutVar als $ case als' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, ..}
          -- modifyMutVar' als $ \case LSNode {..} -> LSNode {lsLocalRankTree = newLR, ..}
          modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = als, ..}
          return als
        (True, False) -> do
          writeMutVar bls $ case bls' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, ..}
          modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = bls, ..}
          return bls
        (False, False) -> do
          cls <- mergeLS (lsNext als') (lsNext bls')
          writeMutVar als $ case als' of LSNode {..} -> LSNode {lsNext = cls, lsLocalRankTree = newLR, ..}
          modifyMutVar cls $ \case LSNode {..} -> LSNode {lsPrev = als, ..}
          modifyMutVar' newLR $ \case LRNode {..} -> LRNode {lrnParent = als, ..}
          return als
    GT -> do
      if lsNext als' == als
        then do
          writeMutVar bls $ case bls' of LSNode {..} -> LSNode {lsPrev = als, ..}
          writeMutVar als $ case als' of LSNode {..} -> LSNode {lsNext = bls, ..}
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
              writeMutVar cls $ case cls' of LSNode {..} -> LSNode {lsLocalRankTree = newLR, ..}
              return cls
            else do
              modifyMutVar' cls $ \case LSNode {..} -> LSNode {lsPrev = als, ..}
              writeMutVar als $ case als' of LSNode{..} -> LSNode {lsNext = cls, ..}
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

insertEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => Graph s v -> v -> v -> m ()
insertEdge g@Graph {..} a b = do
  ae <- readMutVar allEdges
  case HMS.lookup (a, b) ae of
    Just _ -> return ()
    Nothing -> do
      av <- readMutVar allVertices
      conn <- connected g a b
      case conn of
        Nothing -> error "insertEdge: TODO"
        Just True -> do
          let Just ast = Map.lookup a av
          let Just bst = Map.lookup b av
          ast' <- readMutVar ast
          bst' <- readMutVar bst
          writeMutVar ast $! ast' {groups = Map.insertWith Set.union (NonTreeEdge, 0) (Set.singleton b) (groups ast'), lNonTreeEdges = setBit (lNonTreeEdges ast') 0}
          writeMutVar bst $! bst' {groups = Map.insertWith Set.union (NonTreeEdge, 0) (Set.singleton a) (groups bst'), lNonTreeEdges = setBit (lNonTreeEdges bst') 0}
          case ast' of SLeaf {lParent} -> propagate lParent
          case bst' of SLeaf {lParent} -> propagate lParent
          writeMutVar allEdges $ HMS.insert (a, b) (NonTreeEdge, 0) $ HMS.insert (b, a) (NonTreeEdge, 0) ae
          modifyMutVar numEdges succ
        Just False -> do
          let Just ast = Map.lookup a av
          let Just bst = Map.lookup b av
          ast' <- readMutVar ast
          bst' <- readMutVar bst
          writeMutVar ast $! ast' {groups = Map.insertWith Set.union (TreeEdge, 0) (Set.singleton b) (groups ast'), lTreeEdges = setBit (lTreeEdges ast') 0}
          writeMutVar bst $! bst' {groups = Map.insertWith Set.union (TreeEdge, 0) (Set.singleton a) (groups bst'), lTreeEdges = setBit (lTreeEdges bst') 0}
          aRoot <- getRoot ast
          bRoot <- getRoot bst
          mergeST aRoot bRoot
          readMutVar ast >>= \case SLeaf {lParent} -> propagate lParent
          readMutVar bst >>= \case SLeaf {lParent} -> propagate lParent
          writeMutVar allEdges $ HMS.insert (a, b) (TreeEdge, 0) $ HMS.insert (b, a) (TreeEdge, 0) ae
          modifyMutVar' numEdges succ

-- Go to the highest ancestor with level greater or equal to the given level. Return Nothing if it would be a leaf node or if the input node's level is too low
goToLevelGEQ ::
  (PrimState m ~ s, PrimMonad m) =>
  Level -> StructuralTree a0 a1 False False False s v -> m (Maybe (StructuralTree False True False False False s v))
goToLevelGEQ l a = do
  a' <- readMutVar a
  case a' of
    SLeaf {lParent} -> getParentSNode lParent >>= goToLevelGEQ l
    SNode {nParent, level} ->
      if level < l
        then return Nothing
        else getParentSNode nParent >>= goToLevelGEQ l >>= \case
          Nothing -> return $ Just a
          Just b -> return $ Just b

getLevel ::
  (PrimState m ~ s, PrimMonad m) =>
  Level -> StructuralTree a0 a1 False False False s v -> m (StructuralTree False True False False False s v)
getLevel = undefined

sizeWithout ::
  forall m s v. (PrimMonad m, s ~ PrimState m, Ord v) =>
  Graph s v -> Level -> v -> v -> m (Sum Int, Set (v, v))
sizeWithout graph@Graph{..} level a b = do
  av <- readMutVar allVertices
  let Just bl = Map.lookup b av
  bSucc <- goToLevelGEQ (level + 1) bl
  (bNeighbourCounts, bEdges) <- case bSucc of
    Nothing -> foldSize bl
    Just bAncestor -> do
      foldSize bAncestor
  bSize <-  maybe (return 1) count bSucc
  return (Sum bSize <> bNeighbourCounts, bEdges)
  where
    foldSize :: StructuralTree a0 a1 a2 a3 a4 s v -> m (Sum Int, Set (v, v))
    foldSize node = do
      readMutVar node >>= \case
        SLeaf {lTreeEdges, groups, vertex}
          | testBit lTreeEdges level -> case Map.lookup (TreeEdge, level) groups of
              Nothing -> return mempty
              Just s -> do
                let neighbours = Set.filter (\v -> (vertex, v) /= (a, b) && (vertex, v) /= (b, a)) s
                neighbourResults <- mconcat <$> mapM (sizeWithout graph level vertex) (Set.toList neighbours)
                return $ neighbourResults <> (Sum 0, Set.map (vertex,) neighbours <> Set.map (,vertex) neighbours)
          | otherwise -> return mempty
        SNode {nTreeEdges, children}
          | testBit nTreeEdges level -> foldSize children
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

deleteEdge :: (Eq v, Hashable v, PrimMonad m, s ~ PrimState m, Ord v) => Graph s v -> v -> v -> m ()
deleteEdge graph@Graph {..} a b = do
  ae <- readMutVar allEdges
  av <- readMutVar allVertices
  case HMS.lookup (a, b) ae of
    Nothing -> return ()
    Just (NonTreeEdge, level) -> do
      let Just al = Map.lookup a av
      let Just bl = Map.lookup b av
      modifyMutVar' al $ \al' -> al' {groups = Map.adjust (Set.delete b) (NonTreeEdge, level) $ groups al'}
      modifyMutVar' bl $ \bl' -> bl' {groups = Map.adjust (Set.delete a) (NonTreeEdge, level) $ groups bl'}
    Just (TreeEdge, level) -> do
      (Sum bSize, bEdges) <- sizeWithout graph level a b
      (Sum aSize, aEdges) <- sizeWithout graph level b a
      if aSize <= bSize
        then do
          mapM_ (increaseLevel level) $ Set.toList aEdges
        else
          undefined
  where
    increaseLevel lvl (v1, v2) = do
      av <- readMutVar allVertices
      let Just v1' = Map.lookup v1 av
      let Just v2' = Map.lookup v2 av
      modifyMutVar v1' $ \v1'' ->
        let Just s = Map.lookup (TreeEdge, lvl) (groups v1'') in
        let s' = Set.delete v2 s in
        if Set.null s'
          then v1'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v2) $
                      Map.delete (TreeEdge, lvl) (groups v1'')}
          else v1'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v2) $
                      Map.insert (TreeEdge, lvl) s' (groups v1'')}
      modifyMutVar v2' $ \v2'' ->
        let Just s = Map.lookup (TreeEdge, lvl) (groups v2'') in
        let s' = Set.delete v1 s in
        if Set.null s'
          then v2'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v1) $
                      Map.delete (TreeEdge, lvl) (groups v2'')}
          else v2'' {groups = Map.insertWith Set.union (TreeEdge, lvl + 1) (Set.singleton v1) $
                      Map.insert (TreeEdge, lvl) s' (groups v2'')}
      propagate v1'
      propagate v2'
      v1Ancestor <- getLevel (lvl + 1) v1'
      v2Ancestor <- getLevel (lvl + 1) v2'
      mergeST v1Ancestor v2Ancestor -- FIXME
      modifyMutVar' allEdges $ HMS.insert (v1, v2) (TreeEdge, lvl + 1) . HMS.insert (v2, v1) (TreeEdge, lvl + 1)
