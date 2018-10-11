-- | Randomly balanced tree.
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Graph.Dynamic.Internal.Random
    ( Tree

    , singleton
    , append
    , split
    , connected
    , root
    , aggregate
    , toList

    -- * Debugging only
    , freeze
    , print
    , assertInvariants
    , assertSingleton
    , assertRoot
    ) where

import           Control.Monad                    (when)
import           Control.Monad.Primitive          (PrimMonad (..))
import           Data.Graph.Dynamic.Internal.Tree (Edge (..), edgeSize)
import qualified Data.Graph.Dynamic.Internal.Tree as Class
import           Data.Primitive.MutVar            (MutVar)
import qualified Data.Primitive.MutVar            as MutVar
import qualified Data.Tree                        as Tree
import           Prelude                          hiding (concat, print)
import           System.IO.Unsafe                 (unsafePerformIO)
import qualified System.Random.MWC                as MWC
import           Unsafe.Coerce                    (unsafeCoerce)

data T s = T
    { tParent :: {-# UNPACK #-} !(Tree s)
    , tLeft   :: {-# UNPACK #-} !(Tree s)
    , tRight  :: {-# UNPACK #-} !(Tree s)
    , tRandom :: {-# UNPACK #-} !Int
    , tEdge   :: {-# UNPACK #-} !Edge
    , tAgg    :: {-# UNPACK #-} !Int
    }

-- | NOTE (jaspervdj): There are two ways of indicating the parent / left /
-- right is not set (we want to avoid Maybe's since they cause a lot of
-- indirections).
--
-- Imagine that we are considering tLeft.
--
-- 1.  We can set tLeft of x to the MutVar that holds the tree itself (i.e. a
--     self-loop).
-- 2.  We can set tLeft to some nil value.
--
-- They seem to offer similar performance.  We choose to use the latter since it
-- is less likely to end up in infinite loops that way, and additionally, we can
-- move easily move e.g. x's left child to y's right child, even it is an empty
-- child.
nil :: Tree s
nil = unsafeCoerce $ unsafePerformIO $ Tree <$> MutVar.newMutVar undefined
{-# NOINLINE nil #-}

newtype Tree s = Tree (MutVar s (T s)) deriving (Eq)

singleton
    :: PrimMonad m
    => MWC.Gen (PrimState m) -> Edge -> m (Tree (PrimState m))
singleton gen edge = do
    random <- MWC.uniform gen
    Tree <$> MutVar.newMutVar (T nil nil nil random edge (edgeSize edge))

root :: PrimMonad m => Tree (PrimState m) -> m (Tree (PrimState m))
root (Tree tv) = do
    T {..} <- MutVar.readMutVar tv
    if tParent == nil then return (Tree tv) else root tParent

-- | Appends two trees.  Returns the root of the tree.
append
    :: (PrimMonad m)
    => Tree (PrimState m)
    -> Tree (PrimState m)
    -> m (Tree (PrimState m))
append = merge

merge
    :: (PrimMonad m)
    => Tree (PrimState m)
    -> Tree (PrimState m)
    -> m (Tree (PrimState m))
merge xt@(Tree xv) yt@(Tree yv)
    | xt == nil = return yt
    | yt == nil = return xt
    | otherwise = do
        x <- MutVar.readMutVar xv
        y <- MutVar.readMutVar yv
        if tRandom x < tRandom y then do
            rt@(Tree rv) <- merge xt (tLeft y)
            MutVar.writeMutVar yv $! y {tLeft = rt, tAgg = tAgg x + tAgg y}
            MutVar.modifyMutVar rv $ \r -> r {tParent = yt}
            return yt
        else do
            rt@(Tree rv) <- merge (tRight x) yt
            MutVar.writeMutVar xv $! x {tRight = rt, tAgg = tAgg x + tAgg y}
            MutVar.modifyMutVar rv $ \r -> r {tParent = xt}
            return xt

split
    :: (PrimMonad m)
    => Tree (PrimState m)
    -> m (Maybe (Tree (PrimState m)), Maybe (Tree (PrimState m)))
split xt@(Tree xv) = do
    x <- MutVar.readMutVar xv
    let pv = tParent x
        lt = tLeft x
        rt = tRight x

    when (lt /= nil) (removeParent lt)
    when (rt /= nil) (removeParent rt)
    MutVar.writeMutVar xv $!
        x {tParent = nil, tLeft = nil, tRight = nil, tAgg = edgeSize (tEdge x)}

    mergeUp pv xt lt rt

mergeUp
    :: (PrimMonad m)
    => Tree (PrimState m)  -- Current node
    -> Tree (PrimState m)  -- Eliminated node
    -> Tree (PrimState m)  -- Left tree accumulator
    -> Tree (PrimState m)  -- Right tree accumulator
    -> m (Maybe (Tree (PrimState m)), Maybe (Tree (PrimState m)))
mergeUp xt _ lacc racc | xt == nil =
    return
        ( if lacc == nil then Nothing else Just lacc
        , if racc == nil then Nothing else Just racc
        )
mergeUp xt@(Tree xv) ct lacc racc = do
    x <- MutVar.readMutVar xv
    let pt = tParent x
        lt = tLeft x
        rt = tRight x
    if ct == lt then do
        ra <- if rt == nil then return 0 else aggregate rt
        MutVar.writeMutVar xv $! x {tParent = nil, tLeft = nil, tAgg = edgeSize (tEdge x) + ra}
        racc' <- merge racc xt
        mergeUp pt xt lacc racc'
    else do
        la <- if lt == nil then return 0 else aggregate lt
        MutVar.writeMutVar xv $! x {tParent = nil, tRight = nil, tAgg = la + edgeSize (tEdge x)}
        lacc' <- merge xt lacc
        mergeUp pt xt lacc' racc

connected
    :: (PrimMonad m)
    => Tree (PrimState m)
    -> Tree (PrimState m)
    -> m Bool
connected xv yv = do
    xr <- root xv
    yr <- root yv
    return $ xr == yr

aggregate
    :: (PrimMonad m)
    => Tree (PrimState m)
    -> m Int
aggregate (Tree xv) = tAgg <$> MutVar.readMutVar xv

-- | For debugging/testing.
toList
    :: PrimMonad m => Tree (PrimState m) -> m [Edge]
toList = go []
  where
    go acc0 (Tree mv) = do
        T {..} <- MutVar.readMutVar mv
        acc1 <- if tRight == nil then return acc0 else go acc0 tRight
        let acc2 = tEdge : acc1
        if tLeft == nil then return acc2 else go acc2 tLeft

removeParent, _removeLeft, _removeRight
    :: PrimMonad m
    => Tree (PrimState m)  -- Parent
    -> m ()
removeParent (Tree xv) = MutVar.modifyMutVar' xv $ \x -> x {tParent = nil}
_removeLeft  (Tree xv) = MutVar.modifyMutVar' xv $ \x -> x {tLeft = nil}
_removeRight (Tree xv) = MutVar.modifyMutVar' xv $ \x -> x {tRight = nil}

-- | For debugging/testing.
freeze :: PrimMonad m => Tree (PrimState m) -> m (Tree.Tree Edge)
freeze (Tree mv) = do
    T {..} <- MutVar.readMutVar mv
    children  <- sequence $
        [freeze tLeft | tLeft /= nil] ++
        [freeze tRight | tRight /= nil]
    return $ Tree.Node tEdge children

print :: Tree (PrimState IO) -> IO ()
print = go 0
  where
    go d (Tree mv) = do
        T {..} <- MutVar.readMutVar mv
        when (tLeft /= nil) $ go (d + 1) tLeft
        putStrLn $ replicate d ' ' ++ show tEdge
        when (tRight /= nil) $ go (d + 1) tRight

assertInvariants
    :: (PrimMonad m) => Tree (PrimState m) -> m ()
assertInvariants t = do
    _ <- computeAgg nil t
    return ()
  where
    -- TODO: Check average
    computeAgg pt xt@(Tree xv) = do
        x <- MutVar.readMutVar xv
        let pt' = tParent x
        when (pt /= pt') $ fail "broken parent pointer"

        let lt = tLeft x
        let rt = tRight x
        la <- if lt == nil then return 0 else computeAgg xt lt
        ra <- if rt == nil then return 0 else computeAgg xt rt

        let actualAgg = la + (edgeSize (tEdge x)) + ra
        let storedAgg = tAgg x

        when (actualAgg /= storedAgg) $ fail $
            "error in stored aggregates: " ++ show storedAgg ++
            ", actual: " ++ show actualAgg

        return actualAgg

assertSingleton :: PrimMonad m => Tree (PrimState m) -> m ()
assertSingleton (Tree xv) = do
    T {..} <- MutVar.readMutVar xv
    when (tLeft /= nil || tRight /= nil || tParent /= nil) $
        fail "not a singleton"

assertRoot :: PrimMonad m => Tree (PrimState m) -> m ()
assertRoot (Tree xv) = do
    T {..} <- MutVar.readMutVar xv
    when (tParent /= nil) $ fail "not the root"

instance Class.Tree Tree where
    type TreeGen Tree = MWC.Gen
    newTreeGen _ = MWC.create

    singleton  = singleton
    append     = append
    split      = split
    connected  = connected
    root       = root
    aggregate  = aggregate
    toList     = toList

instance Class.TestTree Tree where
    print            = print
    assertInvariants = assertInvariants
    assertSingleton  = assertSingleton
    assertRoot       = assertRoot
