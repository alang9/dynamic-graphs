{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.Levels.Tests2 where

import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Maybe                           (catMaybes)
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.QuickCheck

import           Data.Graph.Dynamic.Internal.Tree
import qualified Data.Graph.Dynamic.Levels            as Levels

import           Action
import qualified Data.Graph.Dynamic.Slow              as Slow

runSlowGraphAction :: Slow.Graph Int -> Action t -> (Slow.Graph Int, Maybe Bool)
runSlowGraphAction graph (Cut x y) =
    (Slow.deleteEdge x' y' graph, Nothing)
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)
runSlowGraphAction graph (Link x y) =
    (Slow.insertEdge x' y' graph, Nothing)
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)
runSlowGraphAction graph (Toggle x y)
    | Slow.hasEdge x' y' graph = (Slow.deleteEdge x' y' graph, Nothing)
    | otherwise                = (Slow.insertEdge x' y' graph, Nothing)
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)
runSlowGraphAction graph (Query x y) =
    (graph, Just (Slow.connected x' y' graph))
  where
    x' = mod x (Slow.numVertices graph)
    y' = mod y (Slow.numVertices graph)

runGraphAction :: Tree tree =>
  Int -> Levels.Graph tree s Int -> [Bool] -> Action t -> ST s [Bool]
runGraphAction n levels xs (Cut x y) = do
    Levels.deleteEdge levels x' y'
    return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Link x y) = do
  _ <- Levels.insertEdge levels x' y'
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n levels xs (Toggle x y) = do
  Levels.hasEdge levels x' y' >>= \case
    True  -> Levels.deleteEdge levels x' y'
    False -> void $ Levels.insertEdge levels x' y'
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
    initialSlowGraph = Slow.fromVertices [0..n-1]
    slowResult = catMaybes $ snd $ mapAccumL runSlowGraphAction initialSlowGraph actions
    result :: [Bool]
    result = runST $ do
      initialGraph <- Levels.fromVertices' [0..n-1]
      results <- foldM (runGraphAction n initialGraph) [] actions
      return $ reverse results

prop_graph_linkcut :: Positive Int -> [Action 'LinkCut] -> Property
prop_graph_linkcut = checkActions

prop_graph_toggle :: Positive Int -> [Action 'Toggl] -> Property
prop_graph_toggle = checkActions

tests :: Test
tests = $testGroupGenerator
