{-# LANGUAGE TemplateHaskell #-}
module Data.MTree.FastAvl.Tests where

import           Control.Monad.Primitive              (PrimMonad (..))
import           Control.Monad.ST                     (runST)
import           Control.Monad.ST
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.MTree.FastAvl                   as FastAvl
import qualified Data.MTree.Splay                     as Splay
import           Data.Semigroup                       ((<>))
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import           Test.QuickCheck

data Joins a v
    = Singleton a v
    | Join (Joins a v) a v (Joins a v)
    deriving (Show)

arbitraryJoins :: (Arbitrary a, Arbitrary v) => Int -> Gen (Joins a v)
arbitraryJoins n
    | n <= 0    = Singleton <$> arbitrary <*> arbitrary
    | otherwise = oneof
        [ Singleton <$> arbitrary <*> arbitrary
        , Join
            <$> arbitraryJoins (n - 1)
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryJoins (n - 1)
        ]

instance (Arbitrary a, Arbitrary v) => Arbitrary (Joins a v) where
    arbitrary = sized arbitraryJoins

    shrink (Singleton _ _) = []
    shrink (Join l v a r)  =
        [l, Singleton v a, r] ++
        [Join l' v a r | l' <- shrink l] ++
        [Join l v a r' | r' <- shrink r]

-- | Performs the joins and returns the root in addition to a list of pointers
-- to the individual nodes.
joinsToTree
    :: (PrimMonad m, Monoid v, Show a)
    => Joins a v
    -> m
        ( FastAvl.Tree (PrimState m) a v
        , NonEmpty (FastAvl.Tree (PrimState m) a v)
        )
joinsToTree (Singleton a v) = do
    s <- FastAvl.singleton a v
    return (s, s NonEmpty.:| [])
joinsToTree (Join jl a v jr)  = do
    (l, lps) <- joinsToTree jl
    c <- FastAvl.singleton a v
    (r, rps) <- joinsToTree jr
    rt <- FastAvl.join (Just l) c (Just r)
    return (rt, lps <> (c NonEmpty.:| []) <> rps)

joinsToList :: Joins a v -> [a]
joinsToList (Singleton a _) = [a]
joinsToList (Join l a _ r)  = joinsToList l ++ [a] ++ joinsToList r

prop_join :: Joins Int () -> Bool
prop_join j = runST $ do
    (t, _) <- joinsToTree j
    FastAvl.assertInvariants t
    FastAvl.assertRoot t

    l <- FastAvl.toList t
    return $ l == joinsToList j

prop_split :: Int -> Joins Int () -> Bool
prop_split idx j = runST $ do
    (t, ptrs) <- joinsToTree j
    let ptr = ptrs NonEmpty.!! (idx `mod` NonEmpty.length ptrs)

    (mbL, mbR) <- FastAvl.split ptr
    case mbL of
        Just l -> do
            FastAvl.assertInvariants l
            FastAvl.assertRoot l
        _ -> return ()

    case mbR of
        Just r -> do
            FastAvl.assertInvariants r
            FastAvl.assertRoot r
        _ -> return ()

    FastAvl.assertInvariants ptr
    FastAvl.assertSingleton ptr

    lList <- maybe (return []) FastAvl.toList mbL
    cList <- FastAvl.toList ptr
    rList <- maybe (return []) FastAvl.toList mbR

    return $ lList ++ cList ++ rList == joinsToList j

prop_append :: Joins Int () -> Joins Int () -> Bool
prop_append lj rj = runST $ do
    (lt, _) <- joinsToTree lj
    (rt, _) <- joinsToTree rj

    r <- FastAvl.append lt rt
    FastAvl.assertInvariants r
    FastAvl.assertRoot r

    l <- FastAvl.toList r
    return $ l == joinsToList lj ++ joinsToList rj

tests :: Test
tests = $(testGroupGenerator)
