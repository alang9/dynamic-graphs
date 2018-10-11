{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Data.Graph.Dynamic.EulerTour.Tests where

import           Control.Monad.ST
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import qualified Data.Graph.Dynamic.EulerTour         as ET
import qualified Data.Graph.Dynamic.Internal.Random   as Random
import           Data.Graph.Dynamic.Program

prop_program :: IntTreeProgram -> ()
prop_program (IntTreeProgram p) = runST go
  where
    go :: forall s. ST s ()
    go = do
        f <- ET.new (\_ _ -> ()) :: ST s (ET.Graph Random.Tree s Int)
        runProgram f p

tests :: Test
tests = $testGroupGenerator
