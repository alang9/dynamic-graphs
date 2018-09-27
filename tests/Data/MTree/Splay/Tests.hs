{-# LANGUAGE TemplateHaskell #-}
module Data.MTree.Splay.Tests
    ( tests
    ) where

import           Control.Monad.ST                     (runST)
import           Control.Monad.ST
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.MTree.Splay                     as Splay
import           Data.Semigroup                       ((<>))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import           Test.QuickCheck                      (Arbitrary (..))

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (NonEmpty.:|) <$> arbitrary <*> arbitrary
    shrink (x NonEmpty.:| xs) = do
        xs' <- shrink xs
        return (x NonEmpty.:| xs')

prop_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_append x y =
    NonEmpty.toList (x <> y) ==
    runST (do
        tx <- Splay.fromNonEmpty $ NonEmpty.zip x $ NonEmpty.repeat ()
        ty <- Splay.fromNonEmpty $ NonEmpty.zip y $ NonEmpty.repeat ()
        tr <- Splay.append tx ty
        Splay.toList tr)

tests :: Test
tests = $(testGroupGenerator)
