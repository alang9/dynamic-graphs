{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import GHC.Generics
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.MTree.EulerTour

data SlowForest = SlowForest
  { numNodes :: !Int
  , edges :: Set.Set (Int, Int)
  }

data Action
  = Cut !Int !Int
  | Link !Int !Int
  | Query !Int !Int
  deriving (Show, Generic)

instance Arbitrary Action where
  arbitrary = oneof
    [ Cut <$> arbitrary <*> arbitrary
    , Link <$> arbitrary <*> arbitrary
    , Query <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

discreteSlowForest :: Int -> SlowForest
discreteSlowForest n = SlowForest n $ Set.fromList [(v,v) | v <- [0..n-1]]

component :: Int -> SlowForest -> Set.Set Int
component v (SlowForest n edges) = go [v] Set.empty
  where
    go [] comp = comp
    go (x:xs) comp
      | Set.member x comp = go xs comp
      | otherwise = go newQueue $ Set.insert x comp
      where
        newQueue = [v' | v' <- [0..n - 1], Set.member (x, v') edges, Set.notMember v' comp] ++ xs

runSlowAction :: SlowForest -> Action -> (SlowForest, Maybe Bool)
runSlowAction SlowForest {..} (Cut x y) = (SlowForest {edges = Set.delete (x', y') (Set.delete (y', x') edges), ..}, Nothing)
  where
    x' = mod x numNodes
    y' = mod y numNodes
runSlowAction sf@SlowForest {..} (Link x y) = (newSf, Nothing)
  where
    newSf = if alreadyConnected then sf else SlowForest {edges = Set.insert (x', y') (Set.insert (y', x') edges), ..}
    alreadyConnected = Set.member y' $ component x' sf
    x' = mod x numNodes
    y' = mod y numNodes
runSlowAction sf@SlowForest {..} (Query x y) = (sf, Just (Set.member y' $ component x' sf))
  where
    x' = mod x numNodes
    y' = mod y numNodes

runAction :: Int -> (EulerTourForest s Int, [Bool]) -> Action -> ST s (EulerTourForest s Int, [Bool])
runAction n (etf, xs) (Cut x y) = case cut x' y' etf of
  Nothing -> return (etf, xs)
  Just etf' -> (,xs) <$> etf'
  where
    x' = mod x n
    y' = mod y n
runAction n (etf, xs) (Link x y) = link x' y' etf >>= \case
  Nothing -> return (etf, xs)
  Just etf' -> return (etf', xs)
  where
    x' = mod x n
    y' = mod y n
runAction n (etf, xs) (Query x y) = case connected x' y' etf of
  Nothing -> return (etf, xs)
  Just q -> do
    q' <- q
    return (etf, q':xs)
  where
    x' = mod x n
    y' = mod y n

prop_slowMatch :: Positive Int -> [Action] -> Property
prop_slowMatch (Positive n) actions = slowResult === result
  where
    initialSlowForest = discreteSlowForest n
    slowResult = catMaybes $ snd $ mapAccumL runSlowAction initialSlowForest actions
    result :: [Bool]
    result = runST $ do
      initialForest <- discreteForest [0..n-1]
      (_finalForest, results) <- foldM (runAction n) (initialForest, []) actions
      return $ reverse results

main :: IO ()
main = $defaultMainGenerator
