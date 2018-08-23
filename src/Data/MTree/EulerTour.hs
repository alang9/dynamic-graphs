{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Data.MTree.EulerTour where

import Control.Monad
import Control.Monad.Primitive
import Data.List
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Data.MTree.Avl as Avl

newtype EulerTourForest s v = EulerTourForest {etf :: Map.Map (v, v) (Avl.Tree s (v, v) ())}

empty :: EulerTourForest s v
empty = EulerTourForest Map.empty

-- values in nodes must be unique
fromTree :: (PrimMonad m, s ~ PrimState m, Ord v) => Tree.Tree v -> m (EulerTourForest s v)
fromTree tree = do
  initial <- Avl.empty
  EulerTourForest . snd <$> go initial Map.empty tree
  where
    go node m tn@(Tree.Node l children) = do
      root1 <- Avl.root node
      newNode <- Avl.snoc root1 (l, l) ()
      let m' = Map.insert (l, l) newNode m
      (newNode2, m'') <- foldM (go' l) (root1, m') children
      valid <- Avl.checkValid newNode2
      if valid
        then return (newNode2, m'')
        else error "fromTree: invalid"
    go' parent (node, m) tr@(Tree.Node l _) = do
      root1 <- Avl.root node
      newNode <- Avl.snoc root1 (parent, l) ()
      (lastNode, m'') <- go newNode m tr
      root2 <- Avl.root lastNode
      newNode2 <- Avl.snoc root2 (l, parent) ()
      let m' = Map.insert (l, parent) newNode2 $ Map.insert (parent, l) newNode m''
      return (newNode2, m')

findRoot :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> EulerTourForest s v -> Maybe (m (Avl.Tree s (v, v) ()))
findRoot v (EulerTourForest m) = Avl.root <$> Map.lookup (v, v) m

cut :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> Maybe (m (EulerTourForest s v))
cut a b (EulerTourForest etf) = case (Map.lookup (a, b) etf, Map.lookup (b, a) etf) of
  _ | a == b -> Nothing -- Can't cut self-loops
  (Nothing, Nothing) -> Nothing -- No edge to cut
  (Just ab, Just ba) -> Just $ do
    (part1, part2) <- Avl.split ab
    root1 <- Avl.root part1
    root2 <- Avl.root part2
    rootBa <- Avl.root ba
    (l, c, r) <- case () of
      _ | root1 == root2 -> error "cut: invalid state"
        | root1 == rootBa -> do
            (part3, part4) <- Avl.split ba
            root3 <- Avl.root part3
            root4 <- Avl.root part4
            return (root3, root4, root2)
        | root2 == rootBa -> do
            (part3, part4) <- Avl.split ba
            root3 <- Avl.root part3
            root4 <- Avl.root part4
            return (root1, root3, root4)
        | otherwise -> error "cut: invalid state"
    Avl.append l r
    return $ EulerTourForest $ Map.delete (a, b) $ Map.delete (b, a) etf
  _ -> error "cut: Invalid state"

-- | reroot the represented tree by shifting the euler tour
reroot :: (PrimMonad m, s ~ PrimState m) => Avl.Tree s a () -> m ()
reroot t = do
  (pre, post) <- Avl.split t
  emp <- Avl.empty
  Avl.merge' emp t post
  newPre <- Avl.root t
  success <- Avl.append newPre pre
  case success of
    True -> return ()
    False -> error "reroot: impossible"

connected :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> Maybe (m Bool)
connected a b (EulerTourForest etf) = case (Map.lookup (a, a) etf, Map.lookup (b, b) etf) of
  (Just aLoop, Just bLoop) -> Just $ connectedTree aLoop bLoop
  _ -> Nothing

connectedTree :: (PrimMonad m, s ~ PrimState m) => Avl.Tree s a () -> Avl.Tree s a () -> m Bool
connectedTree a b = do
    aRoot <- Avl.root a
    bRoot <- Avl.root b
    return $ aRoot == bRoot

link :: (PrimMonad m, s ~ PrimState m, Ord v) => v -> v -> EulerTourForest s v -> m (Maybe (EulerTourForest s v))
link a b (EulerTourForest etf) = case (Map.lookup (a, a) etf, Map.lookup (b, b) etf) of
  (Just aLoop, Just bLoop) -> connectedTree aLoop bLoop >>= \case
      True -> return Nothing
      False -> do
        reroot bLoop
        bLoopRoot <- Avl.root bLoop
        abNode <- Avl.cons (a, b) () bLoopRoot
        bLoopRoot2 <- Avl.root bLoopRoot
        baNode <- Avl.snoc bLoopRoot2 (b, a) ()
        bLoopRoot3 <- Avl.root bLoopRoot2
        (aPre, aPost) <- Avl.split aLoop
        aPreRoot <- Avl.root aPre
        aPostRoot <- Avl.root aPost
        Avl.merge' aPreRoot aLoop bLoopRoot3
        aLoopRoot <- Avl.root aLoop
        Avl.append aLoopRoot aPostRoot
        return $ Just $ EulerTourForest $ Map.insert (a, b) abNode $ Map.insert (b, a) baNode etf
  _ -> return Nothing

tree1 :: Tree.Tree Int
tree1 = Tree.unfoldTree buildNode 1
  where
    buildNode x = if 2*x + 1 > 10 then (x, []) else (x, [2*x, 2*x+1])

showEtf :: EulerTourForest RealWorld Int -> IO ()
showEtf t = do
  roots <- mapM Avl.root $ Map.elems $ etf t
  let roots' = nub roots
  frozen <- mapM Avl.freeze roots'
  let frozen' = map (fmap (Tree.drawTree . fmap show)) frozen
  print (length frozen')
  mapM_ (\f -> case f of Nothing -> putStrLn "Nothing"; Just t -> putStrLn t) frozen'

discreteForest :: (PrimMonad m, s ~ PrimState m, Ord v) => [v] -> m (EulerTourForest s v)
discreteForest vs =
  EulerTourForest <$> foldM go Map.empty vs
  where
    go m v = do
      newL <- Avl.empty
      newR <- Avl.empty
      node <- Avl.merge newL (v, v) () newR
      return $ Map.insert (v, v) node m

-- main :: IO ()
-- main = do
--   putStrLn $ Tree.drawTree $ fmap show tree1
--   t1 <- fromTree tree1
--   showEtf t1
--   case t1 of
--     EulerTourForest t1' -> case Map.lookup (1, 2) t1' of
--       Nothing -> traceShowM ("BAD", Map.keys t1')
--       Just n -> do
--         nRoot <- Avl.root n
--         Avl.checkValid nRoot >>= print . (,) "validity"
--   traceM "cut"
--   let Just t2 = cut 1 2 t1
--   t3 <- t2
--   traceM "link"
--   Just t4 <- link 1 2 t3
--   let t = t4
--   showEtf t
