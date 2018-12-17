
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.Thorup2000.Tests where

import Control.Monad.ST
import Data.Maybe
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Thorup2000 as Thorup2000

instance Interpreter Thorup2000.Graph where
    insert          = Thorup2000.insertVertex
    link            = Thorup2000.insertEdge
    delete          = undefined
    cut             = Thorup2000.deleteEdge
    connected f x y = fromMaybe False <$> Thorup2000.connected f x y

prop_program :: IntGraphProgram -> ()
prop_program (IntGraphProgram p) = runST $ do
    f <- Thorup2000.new
    runProgram f p

tests :: Test
tests = $testGroupGenerator
