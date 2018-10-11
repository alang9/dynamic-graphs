{-# LANGUAGE BangPatterns #-}
import           Criterion.Main

import           Control.Monad.Primitive            (RealWorld)
import qualified Data.Graph.Dynamic.EulerTour       as ETF
import qualified Data.Graph.Dynamic.Internal.Random as Random
import qualified Data.Graph.Dynamic.Levels          as Levels

import           Debug.Trace

main :: IO ()
main = defaultMainWith defaultConfig
  [ bgroup "tree" $ map tree [64 * n | n <- [1..16]]
  , bgroup "graph" $ map graph [16 * n | n <- [1..16]]
  ]
  where
    graph n = bench (show n) $ nfIO (completeGraph n)
    tree n = bench (show n) $ nfIO (completeBinaryTree n)

completeGraph :: Int -> IO [(Maybe Bool, Maybe Bool)]
completeGraph n = do
  levels <- Levels.fromVertices [0..n-1]
    :: IO (Levels.Graph Random.Tree RealWorld Int)
  mapM_ (\(x, y) -> Levels.insertEdge levels x y) edges
  mapM (\(x, y) -> do
           c1 <- Levels.connected levels x y
           Levels.deleteEdge levels x y
           c2 <- Levels.connected levels x y
           return (c1, c2)
       ) edges
  where
    edges = [(x, y) | x <- [0..n-1], y <- [x + 1.. n - 1]]

completeBinaryTree :: Int -> IO [(Maybe Bool, Maybe Bool)]
completeBinaryTree n = do
  etf <- ETF.discreteForest (\_ _ -> ()) [0..n-1]
    :: IO (ETF.Graph Random.Tree RealWorld Int)
  mapM_ (\(x, y) -> ETF.insertEdge etf x y) edges
  mapM (\(x, y) -> do
           c1 <- ETF.connected etf x y
           ETF.deleteEdge etf x y
           c2 <- ETF.connected etf x y
           return (c1, c2)
       ) edges
  return []
  where
    edges = [(x, y) | x <- [0..n-1], y <- filter (< n) [2 * x, 2 * x + 1]]
