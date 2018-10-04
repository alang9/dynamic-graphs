{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Graph.Dynamic.Levels.Tests where

import Control.Monad.ST
import Data.Maybe
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Levels as Levels

prop_program :: IntGraphProgram -> ()
prop_program (IntGraphProgram p) = runST $ do
    f <- Levels.new
    runProgram f p

tests :: Test
tests = $testGroupGenerator
