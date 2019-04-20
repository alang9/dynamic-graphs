{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Dynamic.Thorup2000.Tests where

import Control.Monad.ST
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Data.Graph.Dynamic.Program
import qualified Data.Graph.Dynamic.Thorup2000 as Thorup2000

instance Interpreter Thorup2000.Graph where
    insert          = Thorup2000.insert
    link            = Thorup2000.link
    delete          = Thorup2000.delete
    cut             = Thorup2000.cut
    connected = Thorup2000.connected

prop_program :: IntGraphProgram -> ()
prop_program (IntGraphProgram p) = runST $ do
    f <- Thorup2000.new
    runProgram f p

tests :: Test
tests = $testGroupGenerator
