{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Graph.Dynamic.EulerTour.Tests where

import           Control.Monad                        (foldM)
import           Control.Monad.ST
import           Data.Graph.Dynamic.Action
import qualified Data.Graph.Dynamic.EulerTour         as ET
import qualified Data.Graph.Dynamic.Internal.Random   as Random
import           Data.Graph.Dynamic.Internal.Tree     (Tree)
import           Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Slow              as Slow
import           Data.Hashable                        (Hashable)
import           Data.List                            (mapAccumL)
import           Data.Maybe                           (catMaybes)
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import qualified Test.QuickCheck                      as QC

runForestAction
    :: (Eq v, Hashable v, Monoid a, Tree tree)
    => ET.Forest tree a s v -> [Bool] -> Action t v -> ST s [Bool]
runForestAction etf xs (Cut x y) = ET.deleteEdge etf x y >> return xs
runForestAction etf xs (Link x y) = ET.insertEdge etf x y >> return xs
runForestAction etf xs (Toggle x y) = ET.hasEdge etf x y >>= \case
  True -> ET.deleteEdge etf x y >> return xs
  False -> ET.insertEdge etf x y >> return xs
runForestAction etf xs (Query x y) = ET.connected etf x y >>= \case
  Nothing -> return xs
  Just q -> return (q:xs)

checkActions :: QC.Positive Int -> [Action t Int] -> QC.Property
checkActions (QC.Positive n) actions = slowResult QC.=== result
  where
    actions' = map (fmap (`mod` n)) actions
    initialGraph = Slow.fromVertices [0..n-1]
    slowResult = catMaybes $ snd $ mapAccumL runSlowForestAction initialGraph actions'
    result :: [Bool]
    result = runST $ do
      initialForest <- ET.discreteForest' (\_ _ -> ()) [0..n-1]
      results <- foldM (runForestAction initialForest) [] actions'
      return $ reverse results

prop_forest_linkcut :: QC.Positive Int -> [Action 'LinkCut Int] -> QC.Property
prop_forest_linkcut = checkActions

prop_forest_toggle :: QC.Positive Int -> [Action 'Toggl Int] -> QC.Property
prop_forest_toggle = checkActions

prop_program :: IntTreeProgram -> ()
prop_program (IntTreeProgram p) = runST go
  where
    go :: forall s. ST s ()
    go = do
        f <- ET.new (\_ _ -> ()) :: ST s (ET.Graph Random.Tree s Int)
        runProgram f p

tests :: Test
tests = $testGroupGenerator
