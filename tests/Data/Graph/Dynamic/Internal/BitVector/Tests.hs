{-# LANGUAGE TemplateHaskell #-}
module Data.Graph.Dynamic.Internal.BitVector.Tests
    ( tests
    ) where

import           Control.Monad.ST                      (runST)
import qualified Data.Graph.Dynamic.Internal.BitVector as BitVector
import           Test.Framework                        (Test)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.Framework.TH                     (testGroupGenerator)
import qualified Test.QuickCheck                       as QC

prop_write_read :: QC.Positive Int -> QC.Positive Int -> Bool -> QC.Property
prop_write_read (QC.Positive size) (QC.Positive idx0) val0 = runST $ do
    bv <- BitVector.new size
    BitVector.write bv idx val0
    val1 <- BitVector.read bv idx
    return $ val0 QC.=== val1
  where
    idx = idx0 `mod` size

tests :: Test
tests = $(testGroupGenerator)
