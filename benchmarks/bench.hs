{-# LANGUAGE BangPatterns #-}
import Criterion.Main

import qualified Data.MGraph as MGraph
import qualified Data.MTree.EulerTour as ETF

import Debug.Trace

main :: IO ()
main = defaultMainWith defaultConfig
  [ bgroup "tree" $ map tree [64 * n | n <- [1..32]]
  -- , bgroup "graph"
  --   [ graph 16
  --   , graph 32
  --   , graph 48
  --   , graph 64
  --   , graph 80
  --   -- , foo 128
  --   ]
  ]
  where
    graph n = bench (show n) $ nfIO (completeGraph n)
    tree n = bench (show n) $ nfIO (completeBinaryTree n)

completeGraph :: Int -> IO [(Maybe Bool, Maybe Bool)]
completeGraph n = traceShow edges $ do
  levels <- MGraph.fromVertices [0..n-1]
  mapM_ (\(x, y) -> MGraph.insert x y levels) edges
  mapM (\(x, y) -> do
           c1 <- MGraph.connected x y levels
           MGraph.delete x y levels
           c2 <- MGraph.connected x y levels
           return (c1, c2)
       ) edges
  where
    edges = [(x, y) | x <- [0..n-1], y <- [x + 1.. n - 1]]

completeBinaryTree :: Int -> IO [(Maybe Bool, Maybe Bool)]
completeBinaryTree n = traceShow edges $ do
  etf <- ETF.discreteForest [0..n-1]
  mapM_ (\(x, y) -> ETF.link x y etf) edges
  mapM (\(x, y) -> do
           c1 <- ETF.connected x y etf
           ETF.cut x y etf
           c2 <- ETF.connected x y etf
           return (c1, c2)
       ) edges
  return []
  where
    edges = [(x, y) | x <- [0..n-1], y <- filter (< n) [2 * x, 2 * x + 1]]
