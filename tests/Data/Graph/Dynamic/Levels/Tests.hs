{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.Levels.Tests where

import Control.Monad.ST
import Data.Maybe
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Levels as Levels

instance Interpreter Levels.Graph where
    insertVertex    = Levels.insertVertex
    insertEdge      = Levels.insertEdge
    deleteVertex    = Levels.deleteVertex
    deleteEdge      = Levels.deleteEdge
    connected f x y = fromMaybe False <$> Levels.connected f x y

prop_program :: IntGraphProgram -> ()
prop_program (IntGraphProgram p) = runST $ do
    f <- Levels.new
    runProgram f p

tests :: Test
tests = $testGroupGenerator
