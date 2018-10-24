{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Graph.Dynamic.Levels.Tests where

import           Control.Monad                        (foldM, forM_)
import           Control.Monad.ST
import           Data.Graph.Dynamic.Action
import           Data.Graph.Dynamic.Internal.Tree     (Tree)
import qualified Data.Graph.Dynamic.Levels            as Levels
import qualified Data.Graph.Dynamic.Program           as Program
import qualified Data.Graph.Dynamic.Slow              as Slow
import           Data.Hashable                        (Hashable)
import           Data.List                            (foldl', mapAccumL)
import           Data.Maybe                           (catMaybes)
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import qualified Test.QuickCheck                      as QC

runGraphAction
    :: (Eq v, Hashable v, Tree tree)
    => Levels.Graph tree s v -> [Bool] -> Action t v -> ST s [Bool]
runGraphAction levels xs (Cut x y) = do
    Levels.cut_ levels x y
    return xs
runGraphAction levels xs (Link x y) = do
  Levels.link_ levels x y
  return xs
runGraphAction levels xs (Toggle x y) = do
  Levels.edge levels x y >>= \case
    True  -> Levels.cut_ levels x y
    False -> Levels.link_ levels x y
  return xs
runGraphAction levels xs (Query x y) = Levels.connected levels x y >>= \case
  Nothing -> return xs
  Just q -> do
    return (q:xs)

checkActions :: QC.Positive Int -> [Action t Int] -> QC.Property
checkActions (QC.Positive n) actions = slowResult QC.=== result
  where
    actions' = map (fmap (`mod` n)) actions
    initialSlowGraph = Slow.edgeless [0..n-1]
    slowResult = catMaybes $ snd $ mapAccumL runSlowGraphAction initialSlowGraph actions'
    result :: [Bool]
    result = runST $ do
      initialGraph <- Levels.edgeless' [0..n-1]
      results <- foldM (runGraphAction initialGraph) [] actions'
      return $ reverse results

prop_graph_linkcut :: QC.Positive Int -> [Action 'LinkCut Int] -> QC.Property
prop_graph_linkcut = checkActions

prop_graph_toggle :: QC.Positive Int -> [Action 'Toggl Int] -> QC.Property
prop_graph_toggle = checkActions

prop_program :: Program.IntGraphProgram -> ()
prop_program (Program.IntGraphProgram p) = runST $ do
    f <- Levels.empty'
    Program.runProgram f p

prop_spanningTree :: QC.Positive Int -> [Action 'LinkCut Int] -> QC.Property
prop_spanningTree (QC.Positive n) actions =
    Slow.isSpanningForest spanningForest slow QC.=== True
  where
    actions' = map (fmap (`mod` n)) actions

    spanningForest = runST $ do
        et <- Levels.edgeless' [0 .. n - 1]
        forM_ actions' $ \action -> runGraphAction et [] action
        Levels.spanningForest et

    slow = foldl'
        (\g a -> fst $ runSlowGraphAction g a)
        (Slow.edgeless [0 .. n - 1])
        actions'

tests :: Test
tests = $testGroupGenerator
