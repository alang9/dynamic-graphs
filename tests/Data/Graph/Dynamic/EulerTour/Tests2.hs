{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.EulerTour.Tests2 where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Graph.Dynamic.EulerTour as ET
import Data.Graph.Dynamic.Internal.Tree (Tree)

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

runForestAction :: (Monoid a, Tree tree) =>
  Int -> ET.Forest tree a s Int -> [Bool] -> Action t -> ST s [Bool]
runForestAction n etf xs (Cut x y) = ET.deleteEdge etf x' y' >> return xs
  where
    x' = mod x n
    y' = mod y n
runForestAction n etf xs (Link x y) = ET.insertEdge etf x' y' >> return xs
  where
    x' = mod x n
    y' = mod y n
runForestAction n etf xs (Toggle x y) = ET.hasEdge etf x' y' >>= \case
  True -> ET.deleteEdge etf x' y' >> return xs
  False -> ET.insertEdge etf x' y' >> return xs
  where
    x' = mod x n
    y' = mod y n
runForestAction n etf xs (Query x y) = ET.connected etf x' y' >>= \case
  Nothing -> return xs
  Just q -> return (q:xs)
  where
    x' = mod x n
    y' = mod y n

checkActions :: Positive Int -> [Action t] -> Property
checkActions (Positive n) actions = slowResult === result
  where
    initialGraph = discreteGraph n
    slowResult = catMaybes $ snd $ mapAccumL runSlowForestAction initialGraph actions
    result :: [Bool]
    result = runST $ do
      initialForest <- ET.discreteForest' (\_ _ -> ()) [0..n-1]
      results <- foldM (runForestAction n initialForest) [] actions
      return $ reverse results

prop_forest_linkcut :: Positive Int -> [Action 'LinkCut] -> Property
prop_forest_linkcut = checkActions

prop_forest_toggle :: Positive Int -> [Action 'Toggl] -> Property
prop_forest_toggle = checkActions

tests :: Test
tests = $testGroupGenerator
