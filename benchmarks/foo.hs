
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fprof-auto #-}

import qualified Data.Graph.Dynamic.Levels as Levels
import qualified Data.Graph.Dynamic.EulerTour as ETF
import Control.DeepSeq

main :: IO ()
main = do
  foo <- completeGraph 250
  return $ rnf foo

completeGraph :: Int -> IO [(Maybe Bool, Maybe Bool)]
completeGraph n = do
  levels <- Levels.fromVertices vertices
  mapM_ (\(x, y) -> Levels.link levels x y) edges
  mapM (\(x, y) -> do
           c1 <- Levels.connected levels x y
           Levels.cut levels x y
           c2 <- Levels.connected levels x y
           return (c1, c2)
       ) edges
  where
    vertices = [(x, y, z) | x <- [0..n-1], y <- [0..n-1], z <- [0..n-1]]
    dist (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
    adjVecs = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]
    addV3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
    valid (x, y, z) = x >= 0 && x < n && y >= 0 && y < n && z >= 0 && z < n
    edges = [(x, y) | x <- vertices, d <- adjVecs, let y = addV3 x d, valid y]

completeBinaryTree :: Int -> IO [(Maybe Bool, Maybe Bool)]
completeBinaryTree n = do
  etf <- ETF.discreteForest [0..n-1]
  mapM_ (\(x, y) -> ETF.link etf x y) edges
  mapM (\(x, y) -> do
           c1 <- ETF.connected etf x y
           ETF.cut etf x y
           c2 <- ETF.connected etf x y
           return (c1, c2)
       ) edges
  return []
  where
    edges = [(x, y) | x <- [0..n-1], y <- filter (< n) [2 * x, 2 * x + 1]]
