{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.EulerTour.Tests where

import Control.Monad (void)
import Control.Monad.ST
import Data.Maybe
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import qualified Data.Graph.Dynamic.EulerTour as ET
import Data.Graph.Dynamic.Program

instance Interpreter ET.Forest where
    insertVertex     = ET.insertVertex
    insertEdge f x y = void $ ET.insertEdge f x y
    deleteVertex     = ET.deleteVertex
    deleteEdge f x y = void $ ET.deleteEdge f x y
    connected f x y  = fromMaybe False <$> ET.connected f x y

prop_program :: IntTreeProgram -> ()
prop_program (IntTreeProgram p) = runST $ do
    f <- ET.new
    runProgram f p

tests :: Test
tests = $testGroupGenerator
