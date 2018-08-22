{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MTree.Avl where

import Control.Monad
import Control.Monad.Primitive
import Data.Maybe (fromJust, catMaybes)
import Data.Primitive.MutVar
import qualified Data.Tree as Tree

import Debug.Trace

data Node s a = Node
  { parent :: Maybe (Tree s a)
  , lower :: Maybe (LowerNode s a)
  }

data LowerNode s a = LowerNode
  { label :: a
  , height :: !Int
  , leftChild :: !(Tree s a)
  , rightChild :: !(Tree s a)
  }

type Tree s a = MutVar s (Node s a)

-- | Join 2 trees, with a new node in the middle. Returns the new node (not necessarily the root of the tree)
merge :: (PrimMonad m, s ~ PrimState m) => Tree s a -> a -> Tree s a -> m (Tree s a)
merge l c r = do
  newL <- empty
  newR <- empty
  new <- newMutVar Node {parent = Nothing, lower = Just LowerNode {label = c, leftChild = newL, rightChild = newR, height = 0}}
  merge' l new r
  return new

merge' :: (PrimMonad m, s ~ PrimState m) => Tree s a -> Tree s a -> Tree s a -> m ()
merge' l c r = do
  l' <- readMutVar l
  r' <- readMutVar r
  let lh = height' l'
  let rh = height' r'
  case () of
    _ | abs (lh - rh) <= 1 -> do
        let newHeight = max lh rh + 1
        void $ cutChildren c
        void $ cutParent c
        c' <- readMutVar c
        case lower c' of
          Nothing -> error "merge': c must have lower node"
          Just c_ -> do
            writeMutVar c $ c' {lower = Just c_ {leftChild = l, rightChild = r, height = newHeight}}
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
    parent' Nothing = return Nothing
    parent' (Just n) = parent <$> readMutVar n

deleteLeftmost :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m (Maybe (Tree s a, Tree s a))
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
              return $ Just (t, tr)

deleteRightmost :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m (Maybe (Tree s a, Tree s a))
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
              return $ Just (tl, t)

append :: (PrimMonad m, s ~ PrimState m) => Tree s a -> Tree s a -> m Bool
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

cutChildren :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m (Maybe (Tree s a, Tree s a))
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

cutParent :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m (Maybe (Tree s a))
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

-- | replaced a by b in a's parent. Returns False on failure.
swapChild :: (PrimMonad m, s ~ PrimState m) => Tree s a -> Tree s a -> m Bool
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
      rebalance p
      return True

empty :: (PrimMonad m, s ~ PrimState m) => m (Tree s a)
empty = newMutVar (Node Nothing Nothing)

split :: (PrimMonad m, s ~ PrimState m, Show a) => Tree s a -> m (Tree s a, Tree s a)
split t = do
  t' <- readMutVar t
  (l, r) <- case lower t' of
    Nothing -> do
      (,) <$> empty <*> empty
    Just t_ -> do
      newL <- empty
      newR <- empty
      writeMutVar t $ t' {lower = Just t_ {leftChild = newL, rightChild = newR}}
      modifyMutVar' (leftChild t_) $ \l' -> l' {parent = Nothing}
      modifyMutVar' (rightChild t_) $ \r' -> r' {parent = Nothing}
      return (leftChild t_, rightChild t_)
  go l r
  where
    go l r = do
      t' <- readMutVar t
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
              | otherwise -> error $ "split: invalid state: " ++ show (label p_)

root :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m (Tree s a)
root t = do
  t' <- readMutVar t
  case parent t' of
    Nothing -> return t
    Just p -> root p

snoc :: (PrimMonad m, s ~ PrimState m) => Tree s a -> a -> m (Tree s a)
snoc t a = do
  emptyTree <- empty
  merge t a emptyTree

cons :: (PrimMonad m, s ~ PrimState m) => a -> Tree s a -> m (Tree s a)
cons a t = do
  emptyTree <- empty
  merge emptyTree a t

fromList :: forall m s a. (PrimMonad m, s ~ PrimState m) => [a] -> m (Tree s a)
fromList xs = do
  (emptyTree :: Tree s a) <- empty
  foldM (\t a -> snoc t a >>= root) emptyTree xs

freeze :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m (Maybe (Tree.Tree a))
freeze t = do
  Node {..} <- readMutVar t
  case lower of
    Nothing -> return Nothing
    Just LowerNode {..} -> do
      children'm <- sequence [freeze leftChild, freeze rightChild]
      let children = catMaybes children'm
      return $ Just $ Tree.Node label children

rebalance :: forall m s a. (PrimMonad m, s ~ PrimState m) => Tree s a -> m ()
rebalance n = do
  n' <- readMutVar n
  case lower n' of
    Nothing -> return ()
    Just n_ -> do
      (l'' :: Node s a) <- readMutVar $ leftChild n_
      (r'' :: Node s a) <- readMutVar $ rightChild n_
      let lh = height' l''
      let rh = height' r''
      let newHeight = max lh rh + 1
      case () of
        _ | abs (lh - rh) <= 1 ->
          if newHeight == height n_
            then return ()
            else do
              writeMutVar n $ n' {lower = Just n_ {height = newHeight}}
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

leftRotate :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m ()
leftRotate n = do
  n' <- readMutVar n
  let Just n_ = lower n'
  l'' <- readMutVar $ leftChild n_
  r'' <- readMutVar $ rightChild n_
  let Just r_ = lower r''
  rl'' <- readMutVar $ leftChild r_
  rr'' <- readMutVar $ rightChild r_
  let newNHeight = max (height' rl'') (height' l'') + 1
  let newRHeight = max (height' rr'') (newNHeight) + 1
  writeMutVar n $ n' {parent = Just (rightChild n_), lower = Just n_ {rightChild = leftChild r_, height = newNHeight}}
  modifyMutVar' (leftChild r_) $ \rl' -> rl' {parent = Just n}
  writeMutVar (rightChild n_) $ r'' {parent = parent n', lower = Just r_ {leftChild = n, height = newRHeight}}
  forM_ (parent n') $ \p -> modifyMutVar' p $ \p' -> case lower p' of
    Nothing -> error "leftRotate"
    Just p_ | leftChild p_ == n -> p' {lower = Just p_ { leftChild = rightChild n_ }}
            | rightChild p_ == n -> p' {lower = Just p_ { rightChild = rightChild n_ }}
            | otherwise -> error "leftRotate"
  where
    height' Node {..} = maybe (-1) height lower

rightRotate :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m ()
rightRotate n = do
  n' <- readMutVar n
  let Just n_ = lower n'
  l'' <- readMutVar $ leftChild n_
  r'' <- readMutVar $ rightChild n_
  let Just l_ = lower l''
  ll'' <- readMutVar $ leftChild l_
  lr'' <- readMutVar $ rightChild l_
  let newNHeight = max (height' lr'') (height' r'') + 1
  let newLHeight = max (height' ll'') (newNHeight) + 1
  writeMutVar n $ n' {parent = Just (leftChild n_), lower = Just n_ {leftChild = rightChild l_, height = newNHeight}}
  modifyMutVar' (rightChild l_) $ \lr' -> lr' {parent = Just n}
  writeMutVar (leftChild n_) $ l'' {parent = parent n', lower = Just l_ {rightChild = n, height = newLHeight}}
  forM_ (parent n') $ \p -> modifyMutVar' p $ \p' -> case lower p' of
    Nothing -> error "rightRotate"
    Just p_ | leftChild p_ == n -> p' {lower = Just p_ { leftChild = leftChild n_ }}
            | rightChild p_ == n -> p' {lower = Just p_ { rightChild = leftChild n_ }}
            | otherwise -> error "rightRotate"
  where
    height' Node {..} = maybe (-1) height lower

checkValid :: (PrimMonad m, s ~ PrimState m) => Tree s a -> m Bool
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
