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
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Graph.Dynamic.EulerTour as ET
import Data.Graph.Dynamic.Internal.Tree (Tree)
import qualified Data.Graph.Dynamic.Slow as Slow

import Action

runSlowForestAction
    :: Slow.Graph Int -> Action t -> (Slow.Graph Int, Maybe Bool)
runSlowForestAction graph (Cut x y) =
    (Slow.deleteEdge x' y' graph, Nothing)
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)
runSlowForestAction graph (Link x y)
    | Slow.connected x' y' graph = (graph, Nothing)
    | otherwise                  = (Slow.insertEdge x' y' graph, Nothing)
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)
runSlowForestAction graph (Toggle x y)
    | Slow.hasEdge x' y' graph   = (Slow.deleteEdge x' y' graph, Nothing)
    | Slow.connected x' y' graph = (graph, Nothing)
    | otherwise                  = (Slow.insertEdge x' y' graph, Nothing)
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)
runSlowForestAction graph (Query x y) =
    (graph, Just (Slow.connected x' y' graph))
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)

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
    initialGraph = Slow.fromVertices [0..n-1]
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
