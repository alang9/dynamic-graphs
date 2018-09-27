{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MTree.Avl where

import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.Primitive
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))
import Data.Primitive.MutVar
import qualified Data.Tree as Tree

import Debug.Trace

data Node s a v = Node
  { parent :: Maybe (Tree s a v)
  , lower :: Maybe (LowerNode s a v)
  }

data LowerNode s a v = LowerNode
  { label :: a
  , height :: !Int
  , value :: !v
  , aggregate :: !v
  , leftChild :: !(Tree s a v)
  , rightChild :: !(Tree s a v)
  }

type Tree s a v = MutVar s (Node s a v)

-- | Join 2 trees, with a new node in the middle. Returns the new node (not necessarily the root of the tree)
merge :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> a -> v -> Tree s a v -> m (Tree s a v)
merge l ca cv r = do
  newL <- empty
  newR <- empty
  new <- newMutVar Node {parent = Nothing, lower = Just LowerNode {label = ca, value = cv, aggregate = cv, leftChild = newL, rightChild = newR, height = 0}}
  merge' l new r
  return new

merge' :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> Tree s a v -> Tree s a v -> m ()
merge' l c r = do
  l' <- readMutVar l
  r' <- readMutVar r
  let lh = height' l'
  let rh = height' r'
  let la = aggregate' l'
  let ra = aggregate' r'
  case () of
    _ | abs (lh - rh) <= 1 -> do
        void $ cutChildren c
        void $ cutParent c
        c' <- readMutVar c
        case lower c' of
          Nothing -> error "merge': c must have lower node"
          Just c_ -> do
            let newHeight = max lh rh + 1
            let newAggregate = la <> value c_ <> ra
            writeMutVar c $ c' {lower = Just c_ {leftChild = l, rightChild = r, height = newHeight, aggregate = newAggregate}}
            modifyMutVar l $ \l' -> l' {parent = Just c}
            modifyMutVar r $ \r' -> r' {parent = Just c}
        case (parent l', parent r') of
          (Nothing, Nothing) -> return ()
          (Just p, Nothing) -> do
            modifyMutVar p $ \p' -> p' {lower = Just (fromJust $ lower p') {rightChild = c}}
            modifyMutVar c $ \c' -> c' {parent = Just p}
            rebalance p
          (Nothing, Just p) -> do
            modifyMutVar p $ \p' -> p' {lower = Just (fromJust $ lower p') {leftChild = c}}
            modifyMutVar c $ \c' -> c' {parent = Just p}
            rebalance p
          (Just _, Just _) -> error "merge': illegal state"
      | lh > rh -> do
        l' <- readMutVar l
        let Just l_ = lower l'
        merge' (rightChild l_) c r
      | rh > lh -> do
        r' <- readMutVar r
        let Just r_ = lower r'
        merge' l c (leftChild r_)
      | otherwise -> error "merge': impossible clause"
  where
    height' Node {..} = maybe (-1) height lower
    aggregate' Node {..} = maybe mempty aggregate lower
    parent' Nothing = return Nothing
    parent' (Just n) = parent <$> readMutVar n

deleteLeftmost :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> m (Maybe (Tree s a v, Tree s a v))
deleteLeftmost t = do
  t' <- readMutVar t
  case lower t' of
    Nothing -> return Nothing
    Just t_ -> do
      try <- deleteLeftmost (leftChild t_)
      case try of
        Just _ -> return try
        Nothing -> do
          m'tc <- cutChildren t
          case m'tc of
            Nothing -> error "deleteLeftmost: invalid state"
            Just (_, tr) -> do
              void $ swapChild t tr
              rebalanceParent tr
              return $ Just (t, tr)

deleteRightmost :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> m (Maybe (Tree s a v, Tree s a v))
deleteRightmost t = do
  t' <- readMutVar t
  case lower t' of
    Nothing -> return Nothing
    Just t_ -> do
      try <- deleteRightmost (rightChild t_)
      case try of
        Just _ -> return try
        Nothing -> do
          m'tc <- cutChildren t
          case m'tc of
            Nothing -> error "deleteRightmost: invalid state"
            Just (tl, _) -> do
              void $ swapChild t tl
              rebalanceParent tl
              return $ Just (tl, t)

append :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> Tree s a v -> m Bool
append a b = do
  m'aSplit <- deleteRightmost a
  case m'aSplit of
    Just (al, ar) -> do
      alRoot <- root al
      merge' alRoot ar b
      return True
    Nothing -> do
      m'bSplit <- deleteLeftmost b
      case m'bSplit of
        Just (bl, br) -> do
          brRoot <- root br
          merge' a bl brRoot
          return True
        Nothing -> return False

cutChildren :: (PrimMonad m, s ~ PrimState m) => Tree s a v -> m (Maybe (Tree s a v, Tree s a v))
cutChildren t = do
  t' <- readMutVar t
  case lower t' of
    Nothing -> return Nothing
    Just t_ -> do
      newL <- empty
      newR <- empty
      writeMutVar t $ t' {lower = Just t_ {leftChild = newL, rightChild = newR}}
      modifyMutVar' (leftChild t_) $ \l' -> l' {parent = Nothing}
      modifyMutVar' (rightChild t_) $ \r' -> r' {parent = Nothing}
      return $ Just (leftChild t_, rightChild t_)

cutParent :: (PrimMonad m, s ~ PrimState m) => Tree s a v -> m (Maybe (Tree s a v))
cutParent t = do
  t' <- readMutVar t
  case parent t' of
    Nothing -> return Nothing
    Just p -> do
      writeMutVar t $ t' {parent = Nothing}
      newChild <- empty
      modifyMutVar p $ \p' -> case lower p' of
        Nothing -> error "cutParent: invalid state"
        Just p_
          | leftChild p_ == t -> p' {lower = Just p_ {leftChild = newChild}}
          | rightChild p_ == t -> p' {lower = Just p_ {rightChild = newChild}}
          | otherwise -> error "cutParent: invalid State"
      return (Just p)

-- | replaced a by b in a's parent. Returns False on failure. Doesn't rebalance the parent.
swapChild :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> Tree s a v -> m Bool
swapChild a b = do
  a' <- readMutVar a
  case parent a' of
    Nothing -> return False
    Just p -> do
      p' <- readMutVar p
      case lower p' of
        Nothing -> error "swapChild: invalid state"
        Just p_
          | leftChild p_ == a -> writeMutVar p p' {lower = Just p_ { leftChild = b }}
          | rightChild p_ == a -> writeMutVar p p' {lower = Just p_ { rightChild = b }}
          | otherwise -> error "swapChild: invalid state"
      writeMutVar a a' {parent = Nothing}
      modifyMutVar' b $ \b' -> b' {parent = Just p}
      return True

empty :: (PrimMonad m, s ~ PrimState m) => m (Tree s a v)
empty = newMutVar (Node Nothing Nothing)

split ::
  (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) =>
  Tree s a v -> m (Tree s a v, Tree s a v)
split t = do
  t' <- readMutVar t
  m'children <- cutChildren t
  case m'children of
    Just (l, r) -> go l r
    Nothing -> do
      l <- empty
      r <- empty
      go l r
  where
    go l r = do
      t' <- readMutVar t
      l' <- readMutVar l
      r' <- readMutVar r
      case parent t' of
        Nothing -> return (l, r)
        Just p -> do
          p' <- readMutVar p
          let Just p_ = lower p'
          case () of
            _ | leftChild p_ == t -> do
                  pcc <- cutChildren p
                  let Just (_, pr) = pcc
                  swapChild p t
                  merge' r p pr
                  pRoot <- root p
                  go l pRoot
              | rightChild p_ == t -> do
                  pcc <- cutChildren p
                  let Just (pl, _) = pcc
                  swapChild p t
                  merge' pl p l
                  pRoot <- root p
                  go pRoot r
              | otherwise -> error $ "split: invalid state"

root :: (PrimMonad m, s ~ PrimState m) => Tree s a v -> m (Tree s a v)
root t = do
  t' <- readMutVar t
  case parent t' of
    Nothing -> return t
    Just p -> root p

snoc ::
  (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> a -> v -> m (Tree s a v)
snoc t a v = do
  emptyTree <- empty
  merge t a v emptyTree

cons ::
  (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => a -> v -> Tree s a v -> m (Tree s a v)
cons a v t = do
  emptyTree <- empty
  merge emptyTree a v t

fromList ::
  forall m s a v. (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => [(a, v)] -> m (Tree s a v)
fromList xs = do
  (emptyTree :: Tree s a v) <- empty
  foldM (\t (a, v) -> snoc t a v >>= root) emptyTree xs

toList :: (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> m [a]
toList t = do
  t' <- readMutVar t
  case lower t' of
    Nothing -> return []
    Just LowerNode {..} -> do
      left <- toList leftChild
      right <- toList rightChild
      return $ left ++ label : right

freeze :: (PrimMonad m, s ~ PrimState m) => Tree s a v -> m (Maybe (Tree.Tree a))
freeze t = do
  Node {..} <- readMutVar t
  case lower of
    Nothing -> return Nothing
    Just LowerNode {..} -> do
      children'm <- sequence [freeze leftChild, freeze rightChild]
      let children = catMaybes children'm
      return $ Just $ Tree.Node label children

rebalanceParent :: forall m s a v. (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> m ()
rebalanceParent t = do
  t' <- readMutVar t
  case parent t' of
    Nothing -> return ()
    Just p -> rebalance p

rebalance :: forall m s a v. (PrimMonad m, s ~ PrimState m, Monoid v, Eq v) => Tree s a v -> m ()
rebalance n = do
  n' <- readMutVar n
  case lower n' of
    Nothing -> return ()
    Just n_ -> do
      (l'' :: Node s a v) <- readMutVar $ leftChild n_
      (r'' :: Node s a v) <- readMutVar $ rightChild n_
      let lh = height' l''
      let rh = height' r''
      let la = aggregate' l''
      let ra = aggregate' r''
      let newHeight = max lh rh + 1
      let newAggregate = la <> value n_ <> ra
      case () of
        _ | abs (lh - rh) <= 1 ->
          if newHeight == height n_ && newAggregate == aggregate n_
            then return ()
            else do
              writeMutVar n $ n' {lower = Just n_ {height = newHeight, aggregate = newAggregate}}
              mapM_ rebalance $ parent n'
          | lh > rh, Just l_ <- lower l'' -> do
              ll' <- readMutVar $ leftChild l_
              lr' <- readMutVar $ rightChild l_
              if height' ll' >= height' lr'
                then do
                  rightRotate n
                  mapM_ rebalance $ parent l''
                else do
                  leftRotate $ leftChild n_
                  rightRotate n
                  maybe err rebalance (parent lr')
          | lh < rh, Just r_ <- lower r'' -> do
              rl' <- readMutVar $ leftChild r_
              rr' <- readMutVar $ rightChild r_
              if height' rl' <= height' rr'
                then do
                  leftRotate n
                  mapM_ rebalance $ parent r''
                else do
                  rightRotate $ rightChild n_
                  leftRotate n
                  maybe err rebalance (parent rl')
          | otherwise -> err
  where
    err = error "rebalance: invalid state"
    height' Node {..} = maybe (-1) height lower
    aggregate' Node {..} = maybe mempty aggregate lower

leftRotate :: (PrimMonad m, s ~ PrimState m, Monoid v) => Tree s a v -> m ()
leftRotate n = do
  n' <- readMutVar n
  let Just n_ = lower n'
  l'' <- readMutVar $ leftChild n_
  r'' <- readMutVar $ rightChild n_
  let Just r_ = lower r''
  rl'' <- readMutVar $ leftChild r_
  rr'' <- readMutVar $ rightChild r_
  let newNHeight = max (height' rl'') (height' l'') + 1
  let newNAggregate = aggregate' l'' <> value n_ <> aggregate' rl''
  let newRHeight = max (height' rr'') (newNHeight) + 1
  let newRAggregate = newNAggregate <> value r_ <> aggregate' rr''
  writeMutVar n $ n' {parent = Just (rightChild n_), lower = Just n_ {rightChild = leftChild r_, height = newNHeight, aggregate = newNAggregate}}
  modifyMutVar' (leftChild r_) $ \rl' -> rl' {parent = Just n}
  writeMutVar (rightChild n_) $ r'' {parent = parent n', lower = Just r_ {leftChild = n, height = newRHeight, aggregate = newRAggregate}}
  forM_ (parent n') $ \p -> modifyMutVar' p $ \p' -> case lower p' of
    Nothing -> error "leftRotate"
    Just p_ | leftChild p_ == n -> p' {lower = Just p_ { leftChild = rightChild n_ }}
            | rightChild p_ == n -> p' {lower = Just p_ { rightChild = rightChild n_ }}
            | otherwise -> error "leftRotate"
  where
    height' Node {..} = maybe (-1) height lower
    aggregate' Node {..} = maybe mempty aggregate lower

rightRotate :: (PrimMonad m, s ~ PrimState m, Monoid v) => Tree s a v -> m ()
rightRotate n = do
  n' <- readMutVar n
  let Just n_ = lower n'
  l'' <- readMutVar $ leftChild n_
  r'' <- readMutVar $ rightChild n_
  let Just l_ = lower l''
  ll'' <- readMutVar $ leftChild l_
  lr'' <- readMutVar $ rightChild l_
  let newNHeight = max (height' lr'') (height' r'') + 1
  let newNAggregate = aggregate' lr'' <> value n_ <> aggregate' r''
  let newLHeight = max (height' ll'') (newNHeight) + 1
  let newLAggregate = aggregate' ll'' <> value l_ <> newNAggregate
  writeMutVar n $ n' {parent = Just (leftChild n_), lower = Just n_ {leftChild = rightChild l_, height = newNHeight, aggregate = newNAggregate}}
  modifyMutVar' (rightChild l_) $ \lr' -> lr' {parent = Just n}
  writeMutVar (leftChild n_) $ l'' {parent = parent n', lower = Just l_ {rightChild = n, height = newLHeight, aggregate = newLAggregate}}
  forM_ (parent n') $ \p -> modifyMutVar' p $ \p' -> case lower p' of
    Nothing -> error "rightRotate"
    Just p_ | leftChild p_ == n -> p' {lower = Just p_ { leftChild = leftChild n_ }}
            | rightChild p_ == n -> p' {lower = Just p_ { rightChild = leftChild n_ }}
            | otherwise -> error "rightRotate"
  where
    height' Node {..} = maybe (-1) height lower
    aggregate' Node {..} = maybe mempty aggregate lower

checkValid :: (PrimMonad m, s ~ PrimState m) => Tree s a v -> m Bool
checkValid t = do
  t' <- readMutVar t
  case lower t' of
    Nothing -> return True
    Just t_ -> do
      l' <- readMutVar $ leftChild t_
      r' <- readMutVar $ rightChild t_
      case (parent l', parent r') of
        (Just lp, Just rp) | lp == t && rp == t -> do
          ancestry <- (&&) <$> checkValid (leftChild t_) <*> checkValid (rightChild t_)
          return $ ancestry && max (height' l') (height' r') + 1 == height' t'
        _ -> return False
  where
    height' n = case lower n of
      Nothing -> -1
      Just LowerNode {..} -> height
