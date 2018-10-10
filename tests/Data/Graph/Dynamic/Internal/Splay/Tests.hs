{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Graph.Dynamic.Internal.Splay.Tests
    ( tests
    ) where

import Control.Monad
import           Control.Monad.Primitive              (PrimMonad (..))
import           Control.Monad.ST                     (runST)
import qualified Data.Graph.Dynamic.Internal.Splay    as Splay
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Monoid                          (Sum (..))
import           Data.Semigroup                       ((<>))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import qualified Test.QuickCheck                      as QC

instance QC.Arbitrary a => QC.Arbitrary (NonEmpty a) where
    arbitrary = (NonEmpty.:|) <$> QC.arbitrary <*> QC.arbitrary
    shrink (x NonEmpty.:| xs) = map (x NonEmpty.:|) (QC.shrink xs)

data Appends a v
    = Singleton a v
    | Append (Appends a v) (Appends a v)
    deriving (Show)

arbitraryAppends
    :: (QC.Arbitrary a, QC.Arbitrary v) => Int -> QC.Gen (Appends a v)
arbitraryAppends n
    | n <= 0    = Singleton <$> QC.arbitrary <*> QC.arbitrary
    | otherwise = QC.oneof
        [ Singleton <$> QC.arbitrary <*> QC.arbitrary
        , Append <$> arbitraryAppends (n - 1) <*> arbitraryAppends (n - 1)
        ]

instance (QC.Arbitrary a, QC.Arbitrary v) => QC.Arbitrary (Appends a v) where
    arbitrary = QC.sized arbitraryAppends

    shrink (Singleton _ _) = []
    shrink (Append l r)  =
        [l, r] ++
        [Append l' r | l' <- QC.shrink l] ++
        [Append l r' | r' <- QC.shrink r]

-- | Returns pointers to all nodes.
appendsToTree
    :: (PrimMonad m, Monoid v, Show a)
    => Appends a v
    -> m (Splay.Tree (PrimState m) a v, NonEmpty (Splay.Tree (PrimState m) a v))
appendsToTree = go
  where
    go (Singleton a v) = do
        s <- Splay.singleton a v
        return (s, s NonEmpty.:| [])
    go (Append jl jr)  = do
        (l, lps) <- go jl
        (r, rps) <- go jr
        rt       <- Splay.append l r
        return (rt, lps <> rps)

appendsToList :: Appends a v -> [a]
appendsToList (Singleton a _) = [a]
appendsToList (Append l r)    = appendsToList l ++ appendsToList r

prop_append :: Appends Int (Sum Int) -> QC.Property
prop_append appends = runST $ do
    (t, _) <- appendsToTree appends
    Splay.assertInvariants t

    l <- Splay.toList t
    return $ l QC.=== appendsToList appends

prop_concat :: NonEmpty (Appends Int (Sum Int)) -> QC.Property
prop_concat appends = runST $ do
    ts <- NonEmpty.map fst <$> mapM appendsToTree appends
    t  <- Splay.concat ts
    Splay.assertInvariants t

    l <- Splay.toList t
    return $ l QC.=== concatMap appendsToList (NonEmpty.toList appends)

prop_updateValue :: Appends Int String -> [(Int, String)] -> QC.Property
prop_updateValue appends updates = runST $ do
    (t, nodes) <- appendsToTree appends
    Splay.assertInvariants t
    let l = NonEmpty.length nodes
    forM_ updates $ \(idx, str) -> do
      let n = nodes NonEmpty.!! mod idx l
      Splay.updateValue n str
      Splay.assertInvariants n
    Splay.splay t
    Splay.assertInvariants t
    return $ True QC.=== True

prop_split :: Appends Int String -> Int -> QC.Property
prop_split appends idx = runST $ do
    (t, nodes) <- appendsToTree appends
    Splay.assertInvariants t
    let l = NonEmpty.length nodes
    let n = nodes NonEmpty.!! mod idx l
    (ml, mr) <- Splay.split n
    forM_ ml $ Splay.assertInvariants
    forM_ mr $ Splay.assertInvariants
    Splay.assertInvariants n
    return $ True QC.=== True

tests :: Test
tests = $(testGroupGenerator)
