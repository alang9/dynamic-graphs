{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Properties where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Primitive.MutVar
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.MTree.EulerTour
import qualified Data.MGraph as MGraph

import Action
import Graph

runSlowForestAction ::
  Graph -> Action t -> (Graph, Maybe Bool)
runSlowForestAction Graph {..} (Cut x y) = (Graph {edges = Set.delete (x', y') (Set.delete (y', x') edges), ..}, Nothing)
  where
    x' = mod x numNodes
    y' = mod y numNodes
runSlowForestAction sf@Graph {..} (Link x y) = (newSf, Nothing)
  where
    newSf = if alreadyConnected then sf else Graph {edges = Set.insert (x', y') (Set.insert (y', x') edges), ..}
    alreadyConnected = Set.member y' $ component x' sf
    x' = mod x numNodes
    y' = mod y numNodes
runSlowForestAction sf@Graph {..} (Toggle x y) = (newSf, Nothing)
  where
    newSf = if Set.member (x', y') edges
      then Graph {edges = Set.delete (x', y') (Set.delete (y', x') edges), ..}
      else if alreadyConnected
             then sf
             else Graph {edges = Set.insert (x', y') (Set.insert (y', x') edges), ..}
    alreadyConnected = Set.member y' $ component x' sf
    x' = mod x numNodes
    y' = mod y numNodes
runSlowForestAction sf@Graph {..} (Query x y) = (sf, Just (Set.member y' $ component x' sf))
  where
    x' = mod x numNodes
    y' = mod y numNodes

runForestAction ::
  Int -> EulerTourForest s Int -> [Bool] -> Action t -> ST s [Bool]
runForestAction n etf xs (Cut x y) = cut x' y' etf >> return xs
  where
    x' = mod x n
    y' = mod y n
runForestAction n etf xs (Link x y) = link x' y' etf >> return xs
  where
    x' = mod x n
    y' = mod y n
runForestAction n etf xs (Toggle x y) = hasEdge x' y' etf >>= \case
  True -> cut x' y' etf >> return xs
  False -> link x' y' etf >> return xs
  where
    x' = mod x n
    y' = mod y n
runForestAction n etf xs (Query x y) = connected x' y' etf >>= \case
  Nothing -> return xs
  Just q -> return (q:xs)
  where
    x' = mod x n
    y' = mod y n

prop_forest_linkcut :: Positive Int -> [Action 'LinkCut] -> Property
prop_forest_linkcut (Positive n) actions = slowResult === result
  where
    initialGraph = discreteGraph n
    slowResult = catMaybes $ snd $ mapAccumL runSlowForestAction initialGraph actions
    result :: [Bool]
    result = runST $ do
      initialForest <- discreteForest [0..n-1]
      results <- foldM (runForestAction n initialForest) [] actions
      return $ reverse results

prop_forest_toggle :: Positive Int -> [Action 'Toggl] -> Property
prop_forest_toggle (Positive n) actions = slowResult === result
  where
    initialGraph = discreteGraph n
    slowResult = catMaybes $ snd $ mapAccumL runSlowForestAction initialGraph actions
    result :: [Bool]
    result = runST $ do
      initialForest <- discreteForest [0..n-1]
      results <- foldM (runForestAction n initialForest) [] actions
      return $ reverse results

runSlowGraphAction ::
  Graph -> Action t -> (Graph, Maybe Bool)
runSlowGraphAction Graph {..} (Cut x y) = (Graph {edges = Set.delete (x', y') (Set.delete (y', x') edges), ..}, Nothing)
  where
    x' = mod x numNodes
    y' = mod y numNodes
runSlowGraphAction sf@Graph {..} (Link x y) = (newSf, Nothing)
  where
    newSf = Graph {edges = Set.insert (x', y') (Set.insert (y', x') edges), ..}
    x' = mod x numNodes
    y' = mod y numNodes
runSlowGraphAction sf@Graph {..} (Toggle x y) = (newSf, Nothing)
  where
    newSf = if Set.member (x', y') edges
      then Graph {edges = Set.delete (x', y') (Set.delete (y', x') edges), ..}
      else Graph {edges = Set.insert (x', y') (Set.insert (y', x') edges), ..}
    x' = mod x numNodes
    y' = mod y numNodes
runSlowGraphAction sf@Graph {..} (Query x y) = (sf, Just (Set.member y' $ component x' sf))
  where
    x' = mod x numNodes
    y' = mod y numNodes

runGraphAction ::
  Int -> MGraph.Levels s Int -> [Bool] -> Action t -> ST s [Bool]
runGraphAction n levels xs (Cut x y) = do
    MGraph.delete x' y' levels
    return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Link x y) = do
  MGraph.insert x' y' levels
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Toggle x y) = do
  MGraph.L {..} <- readMutVar levels
  if Set.member (x', y') allEdges
    then MGraph.delete x' y' levels
    else MGraph.insert x' y' levels
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Query x y) = MGraph.connected x' y' levels >>= \case
  Nothing -> return xs
  Just q -> do
    return (q:xs)
  where
    x' = mod x n
    y' = mod y n

prop_graph_linkcut :: Positive Int -> [Action 'LinkCut] -> Property
prop_graph_linkcut (Positive n) actions = slowResult === result
  where
    initialSlowGraph = discreteGraph n
    slowResult = catMaybes $ snd $ mapAccumL runSlowGraphAction initialSlowGraph actions
    result :: [Bool]
    result = runST $ do
      initialGraph <- MGraph.fromVertices [0..n-1]
      results <- foldM (runGraphAction n initialGraph) [] actions
      return $ reverse results

prop_graph_toggle :: Positive Int -> [Action 'Toggl] -> Property
prop_graph_toggle (Positive n) actions = slowResult === result
  where
    initialSlowGraph = discreteGraph n
    slowResult = catMaybes $ snd $ mapAccumL runSlowGraphAction initialSlowGraph actions
    result :: [Bool]
    result = runST $ do
      initialGraph <- MGraph.fromVertices [0..n-1]
      results <- foldM (runGraphAction n initialGraph) [] actions
      return $ reverse results

tests :: Test
tests = $testGroupGenerator
