{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.Graph.Dynamic.Internal.Tree.Tests
    ( BuildTree
    , prop_build
    , prop_split
    ) where

import           Control.Monad.Primitive          (PrimMonad (..))
import           Control.Monad.ST                 (runST)
import           Data.Graph.Dynamic.Internal.Tree
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Proxy                       (Proxy)

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid                      ((<>))
#endif

import qualified Test.QuickCheck                  as QC

data BuildTree a v
    = Singleton a v
    | Append (BuildTree a v) (BuildTree a v)
    | Snoc (BuildTree a v) a v
    | Cons a v (BuildTree a v)
    deriving (Show)

arbitraryBuildTree
    :: (QC.Arbitrary a, QC.Arbitrary v) => Int -> QC.Gen (BuildTree a v)
arbitraryBuildTree n
    | n <= 0    = Singleton <$> QC.arbitrary <*> QC.arbitrary
    | otherwise = QC.oneof
        [ Singleton <$> QC.arbitrary <*> QC.arbitrary
        , Append <$> arbitraryBuildTree (n - 1) <*> arbitraryBuildTree (n - 1)
        , Snoc <$> arbitraryBuildTree (n - 1) <*> QC.arbitrary <*> QC.arbitrary
        , Cons <$> QC.arbitrary <*> QC.arbitrary <*> arbitraryBuildTree (n - 1)
        ]

instance (QC.Arbitrary a, QC.Arbitrary v) => QC.Arbitrary (BuildTree a v) where
    arbitrary = QC.sized arbitraryBuildTree

    shrink (Singleton _ _) = []
    shrink (Snoc l a v)    = [l] ++ [Snoc l' a v | l' <- QC.shrink l]
    shrink (Cons a v r)    = [r] ++ [Cons a v r' | r' <- QC.shrink r]
    shrink (Append l r)    =
        [l, r] ++
        [Append l' r | l' <- QC.shrink l] ++
        [Append l r' | r' <- QC.shrink r]

-- | Returns pointers to all nodes.
appendsToTree
    :: (Tree t, PrimMonad m, Monoid v)
    => Proxy t
    -> TreeGen t (PrimState m)
    -> BuildTree a v
    -> m (t (PrimState m) a v, NonEmpty (t (PrimState m) a v))
appendsToTree _proxy gen = go
  where
    go (Singleton a v) = do
        s <- singleton gen a v
        return (s, s NonEmpty.:| [])
    go (Snoc b a v) = do
        (l, lps) <- go b
        s        <- singleton gen a v
        rt       <- l `snoc` s
        return (rt, lps <> (s NonEmpty.:| []))
    go (Cons a v b) = do
        s        <- singleton gen a v
        (r, rps) <- go b
        rt       <- s `cons` r
        return (rt, (s NonEmpty.:| []) <> rps)
    go (Append bl br)  = do
        (l, lps) <- go bl
        (r, rps) <- go br
        rt       <- append l r
        return (rt, lps <> rps)

appendsToList :: BuildTree a v -> [a]
appendsToList (Singleton a _) = [a]
appendsToList (Snoc l a _)    = appendsToList l ++ [a]
appendsToList (Cons a _ r)    = [a] ++ appendsToList r
appendsToList (Append l r)    = appendsToList l ++ appendsToList r

prop_build
    :: (TestTree t, Eq a, Show a, Eq v, Monoid v, Show v)
    => Proxy t -> BuildTree a v -> QC.Property
prop_build proxy appends = runST $ do
    gen    <- newTreeGen proxy
    (t, _) <- appendsToTree proxy gen appends
    assertInvariants t

    l <- toList t
    return $ l QC.=== appendsToList appends

prop_split
    :: (TestTree t, Eq a, Show a, Eq v, Monoid v, Show v)
    => Proxy t -> Int -> BuildTree a v -> QC.Property
prop_split proxy idx0 appends = runST $ do
    gen        <- newTreeGen proxy
    (_t, ptrs) <- appendsToTree proxy gen appends
    let idx = idx0 `mod` NonEmpty.length ptrs
        ptr = ptrs NonEmpty.!! idx

    (mbL, mbR) <- split ptr
    case mbL of
        Just l -> do
            assertInvariants l
            assertRoot l
        _ -> return ()

    case mbR of
        Just r -> do
            assertInvariants r
            assertRoot r
        _ -> return ()

    assertInvariants ptr
    assertSingleton ptr

    lList <- maybe (return []) toList mbL
    cList <- toList ptr
    rList <- maybe (return []) toList mbR

    return $ lList ++ cList ++ rList QC.=== appendsToList appends
