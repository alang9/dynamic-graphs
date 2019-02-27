{-# LANGUAGE BangPatterns #-}
import           Criterion.Main
import           Data.Maybe                    (fromMaybe)

import qualified Data.Graph.Dynamic.EulerTour  as ETF
import qualified Data.Graph.Dynamic.Levels     as Levels
import qualified Data.Graph.Dynamic.Thorup2000 as T2000

main :: IO ()
main = defaultMainWith defaultConfig
    [ bgroup "etf" $ map tree [64 * n | n <- [1..16]]
    , bgroup "levels" $ map levels [16 * n | n <- [1..16]]
    , bgroup "t2000" $ map t2000 [16 * n | n <- [1..16]]
    ]
  where
    levels n = bench (show n) $ nfIO (completeGraphLevels n)
    t2000 n = bench (show n) $ nfIO (completeGraphT2000 n)
    tree n = bench (show n) $ nfIO (completeBinaryTree n)

completeGraphLevels :: Int -> IO [(Bool, Bool)]
completeGraphLevels n = do
  levels <- Levels.edgeless' [0..n-1]
  mapM_ (\(x, y) -> Levels.link levels x y) edges
  mapM (\(x, y) -> do
           c1 <- Levels.connected levels x y
           Levels.cut levels x y
           c2 <- Levels.connected levels x y
           return (c1, c2)
       ) edges
  where
    edges = [(x, y) | x <- [0..n-1], y <- [x + 1.. n - 1]]

completeGraphT2000 :: Int -> IO [(Bool, Bool)]
completeGraphT2000 n = do
  levels <- T2000.fromVertices [0..n-1]
  mapM_ (\(x, y) -> T2000.link levels x y) edges
  mapM (\(x, y) -> do
           c1 <- fromMaybe False <$> T2000.connected levels x y
           T2000.cut levels x y
           c2 <- fromMaybe False <$> T2000.connected levels x y
           return (c1, c2)
       ) edges
  where
    edges = [(x, y) | x <- [0..n-1], y <- [x + 1.. n - 1]]

completeBinaryTree :: Int -> IO [(Bool, Bool)]
completeBinaryTree n = do
  etf <- ETF.edgeless' [0..n-1]
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
