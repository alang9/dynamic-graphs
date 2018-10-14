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
    :: Slow.Graph Int -> Action t Int -> (Slow.Graph Int, Maybe Bool)
runSlowForestAction graph (Cut x y) =
    (Slow.deleteEdge x y graph, Nothing)
runSlowForestAction graph (Link x y)
    | Slow.connected x y graph = (graph, Nothing)
    | otherwise                  = (Slow.insertEdge x y graph, Nothing)
runSlowForestAction graph (Toggle x y)
    | Slow.hasEdge x y graph   = (Slow.deleteEdge x y graph, Nothing)
    | Slow.connected x y graph = (graph, Nothing)
    | otherwise                  = (Slow.insertEdge x y graph, Nothing)
runSlowForestAction graph (Query x y) =
    (graph, Just (Slow.connected x y graph))

runForestAction :: (Monoid a, Tree tree) =>
  ET.Forest tree a s Int -> [Bool] -> Action t Int -> ST s [Bool]
runForestAction etf xs (Cut x y) = ET.deleteEdge etf x y >> return xs
runForestAction etf xs (Link x y) = ET.insertEdge etf x y >> return xs
runForestAction etf xs (Toggle x y) = ET.hasEdge etf x y >>= \case
  True -> ET.deleteEdge etf x y >> return xs
  False -> ET.insertEdge etf x y >> return xs
runForestAction etf xs (Query x y) = ET.connected etf x y >>= \case
  Nothing -> return xs
  Just q -> return (q:xs)

checkActions :: Positive Int -> [Action t Int] -> Property
checkActions (Positive n) actions = slowResult === result
  where
    actions' = map (fmap (`mod` n)) actions
    initialGraph = Slow.fromVertices [0..n-1]
    slowResult = catMaybes $ snd $ mapAccumL runSlowForestAction initialGraph actions'
    result :: [Bool]
    result = runST $ do
      initialForest <- ET.discreteForest' (\_ _ -> ()) [0..n-1]
      results <- foldM (runForestAction initialForest) [] actions'
      return $ reverse results

prop_forest_linkcut :: Positive Int -> [Action 'LinkCut Int] -> Property
prop_forest_linkcut = checkActions

prop_forest_toggle :: Positive Int -> [Action 'Toggl Int] -> Property
prop_forest_toggle = checkActions

tests :: Test
tests = $testGroupGenerator
