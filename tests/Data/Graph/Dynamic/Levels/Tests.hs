{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Graph.Dynamic.Levels.Tests where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Primitive.MutVar
import qualified Data.Set as Set
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Graph.Dynamic.Levels as Levels

import Action
import Graph

runSlowGraphAction ::
  Graph -> Action t -> (Graph, Maybe Bool)
runSlowGraphAction Graph {..} (Cut x y) = (Graph {edges = Set.delete (x', y') (Set.delete (y', x') edges), ..}, Nothing)
  where
    x' = mod x numNodes
    y' = mod y numNodes
runSlowGraphAction Graph {..} (Link x y) = (newSf, Nothing)
  where
    newSf = Graph {edges = Set.insert (x', y') (Set.insert (y', x') edges), ..}
    x' = mod x numNodes
    y' = mod y numNodes
runSlowGraphAction Graph {..} (Toggle x y) = (newSf, Nothing)
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
  Int -> Levels.Graph s Int -> [Bool] -> Action t -> ST s [Bool]
runGraphAction n levels xs (Cut x y) = do
    Levels.cut levels x' y'
    return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Link x y) = do
  Levels.link levels x' y'
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Toggle x y) = do
  Levels.hasEdge levels x' y' >>= \case
    True  -> Levels.cut levels x' y'
    False -> Levels.link levels x' y'
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Query x y) = Levels.connected levels x' y' >>= \case
  Nothing -> return xs
  Just q -> do
    return (q:xs)
  where
    x' = mod x n
    y' = mod y n

checkActions :: Positive Int -> [Action t] -> Property
checkActions (Positive n) actions = slowResult === result
  where
    initialSlowGraph = discreteGraph n
    slowResult = catMaybes $ snd $ mapAccumL runSlowGraphAction initialSlowGraph actions
    result :: [Bool]
    result = runST $ do
      initialGraph <- Levels.fromVertices [0..n-1]
      results <- foldM (runGraphAction n initialGraph) [] actions
      return $ reverse results

prop_graph_linkcut :: Positive Int -> [Action 'LinkCut] -> Property
prop_graph_linkcut = checkActions

prop_graph_toggle :: Positive Int -> [Action 'Toggl] -> Property
prop_graph_toggle = checkActions

tests :: Test
tests = $testGroupGenerator
