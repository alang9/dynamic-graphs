{-# LANGUAGE TemplateHaskell #-}
module Data.Graph.Dynamic.Internal.Splay.Tests
    ( tests
    ) where

import qualified Data.Graph.Dynamic.Internal.Splay      as Splay
import qualified Data.Graph.Dynamic.Internal.Tree.Tests as Class
import           Data.Monoid                            (Sum)
import           Data.Proxy                             (Proxy (..))
import           Test.Framework                         (Test)
import           Test.Framework.Providers.QuickCheck2   (testProperty)
import           Test.Framework.TH                      (testGroupGenerator)
import qualified Test.QuickCheck                        as QC

prop_append :: Class.BuildTree Int (Sum Int) -> QC.Property
prop_append = Class.prop_build (Proxy :: Proxy Splay.Tree)

prop_split :: Int -> Class.BuildTree Int (Sum Int) -> QC.Property
prop_split = Class.prop_split (Proxy :: Proxy Splay.Tree)

tests :: Test
tests = $(testGroupGenerator)
