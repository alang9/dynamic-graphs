{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Graph.Dynamic.Levels.Tests where

import           Control.Monad.ST
import qualified Data.Graph.Dynamic.Internal.Random   as Random
import qualified Data.Graph.Dynamic.Levels            as Levels
import           Data.Graph.Dynamic.Program
import           Data.Maybe
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

prop_program :: IntGraphProgram -> ()
prop_program (IntGraphProgram p) = runST go
  where
    go :: forall s. ST s ()
    go = do
        f <- Levels.new :: ST s (Levels.Graph Random.Tree s Int)
        runProgram f p

tests :: Test
tests = $testGroupGenerator
