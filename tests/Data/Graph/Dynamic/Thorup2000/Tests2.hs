{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.Thorup2000.Tests2 where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Primitive.MutVar
import qualified Data.Set as Set
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Thorup2000 as T

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

runGraphAction :: (PrimMonad m, s ~ PrimState m) =>
  Int -> T.Graph s Int -> [Bool] -> Action t -> m [Bool]
runGraphAction n graph xs (Cut x y) = do
    T.deleteEdge graph x' y'
    return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n graph xs (Link x y) = do
  T.insertEdge graph x' y'
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n graph xs (Toggle x y) = do
  T.hasEdge graph x' y' >>= \case
    True  -> T.deleteEdge graph x' y'
    False -> void $ T.insertEdge graph x' y'
  return xs
  where
    x' = mod x n
    y' = mod y n
runGraphAction n graph xs (Query x y) = T.connected graph x' y' >>= \case
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
      initialGraph <- T.fromVertices [0..n-1]
      results <- foldM (runGraphAction n initialGraph) [] actions
      return $ reverse results

prop_graph_linkcut :: Positive Int -> [Action 'LinkCut] -> Property
prop_graph_linkcut = checkActions

prop_graph_toggle :: Positive Int -> [Action 'Toggl] -> Property
prop_graph_toggle = checkActions

tests :: Test
tests = $testGroupGenerator
