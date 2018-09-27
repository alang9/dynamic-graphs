{-# LANGUAGE TemplateHaskell #-}
module Data.MTree.Splay.Tests
    ( tests
    ) where

import           Control.Monad.Primitive              (PrimMonad (..))
import           Control.Monad.ST                     (runST)
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.MTree.Splay                     as Splay
import           Data.Semigroup                       ((<>))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import qualified Test.QuickCheck                      as QC

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

prop_append :: Appends Int () -> Bool
prop_append appends = runST $ do
    (t, _) <- appendsToTree appends
    Splay.assertInvariants t

    l <- Splay.toList t
    return $ l == appendsToList appends

tests :: Test
tests = $(testGroupGenerator)
