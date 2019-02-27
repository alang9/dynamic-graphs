{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.Thorup2000.Tests2 where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Graph.Dynamic.Action            as A
import qualified Data.Graph.Dynamic.Slow              as Slow
import           Data.List
import           Data.Maybe
import           Data.Primitive.MutVar
import qualified Data.Set                             as Set
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.QuickCheck

import           Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Thorup2000        as T

runGraphAction :: (PrimMonad m, s ~ PrimState m) =>
  Int -> T.Graph s Int -> [Bool] -> A.Action t Int -> m [Bool]
runGraphAction n graph xs (A.Cut x y) = do
  T.cut graph x y
  return xs
runGraphAction n graph xs (A.Link x y) = do
  T.link graph x y
  return xs
runGraphAction n graph xs (A.Toggle x y) = do
  T.hasEdge graph x y >>= \case
    True  -> T.cut graph x y
    False -> void $ T.link graph x y
  return xs
runGraphAction n graph xs (A.Query x y) =
  (:xs) <$> T.connected graph x y

checkActions :: Positive Int -> [A.Action t Int] -> Property
checkActions (Positive n) actions = slowResult === result
  where
    actions' = map (fmap (`mod` n)) actions
    initialSlowGraph = Slow.edgeless [0..n-1]
    slowResult = catMaybes $ snd $ mapAccumL A.runSlowGraphAction initialSlowGraph actions'
    result :: [Bool]
    result = runST $ do
      initialGraph <- T.fromVertices [0..n-1]
      results <- foldM (runGraphAction n initialGraph) [] actions'
      return $ reverse results

prop_graph_linkcut :: Positive Int -> [A.Action 'A.LinkCut Int] -> Property
prop_graph_linkcut = checkActions

prop_graph_toggle :: Positive Int -> [A.Action 'A.Toggl Int] -> Property
prop_graph_toggle = checkActions

tests :: Test
tests = $testGroupGenerator
