{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Graph.Dynamic.Internal.Splay
    ( Tree

    , singleton
    , append
    , concat
    , split
    , connected
    , root
    , aggregate
    , toList

    -- * Debugging only
    , getRoot
    , freeze
    , print
    , assertInvariants
    ) where

import           Control.Monad           (foldM, when)
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Monoid             ((<>))
import           Data.Primitive.MutVar   (MutVar)
import qualified Data.Primitive.MutVar   as MutVar
import qualified Data.Tree               as Tree
import           Prelude                 hiding (concat, print)


data Tree' s a v = Tree
    { tParent :: {-# UNPACK #-} !(Tree s a v)
    , tLeft   :: {-# UNPACK #-} !(Tree s a v)
    , tRight  :: {-# UNPACK #-} !(Tree s a v)
    , tLabel  :: !a
    , tValue  :: !v
    , tAgg    :: !v
    }

-- instance Eq (Tree s a v) where
--     -- Reference equality through a MutVar.
--     t1 == t2 = tParent t1 == tParent t2

type Tree s a v = MutVar s (Tree' s a v)

singleton :: PrimMonad m => a -> v -> m (Tree (PrimState m) a v)
singleton tLabel tValue = do
    tree <- MutVar.newMutVar undefined
    MutVar.writeMutVar tree $! Tree tree tree tree tLabel tValue tValue
    return tree

getRoot :: PrimMonad m => Tree (PrimState m) a v -> m (Tree (PrimState m) a v)
getRoot tree = do
    Tree{..} <- MutVar.readMutVar tree
    if tParent == tree then return tree else getRoot tParent

-- | Appends two trees.  Returns the root of the tree.
append
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
append x y = do
    rm <- getRightMost x
    _  <- splay rm
    -- _  <- splay y
    setRight rm y
    updateAggregate rm
    return rm

concat
    :: (PrimMonad m, Monoid v)
    => NonEmpty (Tree (PrimState m) a v)
    -> m (Tree (PrimState m) a v)
concat (x0 NonEmpty.:| xs0) =
    foldM append x0 xs0

split
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Maybe (Tree (PrimState m) a v), Maybe (Tree (PrimState m) a v))
split x = do
    _ <- splay x
    Tree{..} <- MutVar.readMutVar x
    removeParent tLeft  -- Works even if l is x
    removeParent tRight
    removeLeft  x
    removeRight x
    return
        ( if tLeft == x then Nothing else Just tLeft
        , if tRight == x then Nothing else Just tRight
        )

connected
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m Bool
connected x y = do
    _  <- splay x
    x' <- splay y
    return $ x == x'

root
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
root x = do
    _ <- splay x
    return x

aggregate
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m v
aggregate x = tAgg <$> MutVar.readMutVar x

-- | For debugging/testing.
toList
    :: PrimMonad m => Tree (PrimState m) a v -> m [a]
toList = go []
  where
    go acc0 tree = do
        Tree{..} <- MutVar.readMutVar tree
        acc1   <- if tRight == tree then return acc0 else go acc0 tRight
        let acc2 = tLabel : acc1
        if tLeft  == tree then return acc2 else go acc2 tLeft

splay
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)  -- Returns the old root.
splay x0 = go x0 x0
  where
    go closestToRootFound x = do
        p <- tParent <$> MutVar.readMutVar x
        if p == x then do
            return closestToRootFound
        else do
            p' <- MutVar.readMutVar p
            let gp = tParent p'
            let lp = tLeft p'
            if gp == p
                then do
                    -- ZIG
                    if lp == x then rotateRight p x else rotateLeft p x
                else do
                    lgp <- tLeft <$> MutVar.readMutVar gp
                    if  | lp == x && lgp == p -> do
                            -- ZIGZIG
                            rotateRight gp p
                            rotateRight p x
                        | lp /= x && lgp /= p -> do
                            -- ZIGZIG
                            rotateLeft gp p
                            rotateLeft p x
                        | lp == x -> do
                            -- ZIGZAG
                            rotateRight p x
                            rotateLeft gp x
                        | otherwise -> do
                            -- ZIGZAG
                            rotateLeft p x
                            rotateRight gp x

            go gp x

getRightMost
    :: PrimMonad m
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
getRightMost t = do
    tr <- tRight <$> MutVar.readMutVar t
    if t == tr then return t else getRightMost tr

rotateLeft, rotateRight
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v  -- X's parent
    -> Tree (PrimState m) a v  -- X
    -> m ()
rotateLeft pv xv = do
    p0 <- MutVar.readMutVar pv
    x0 <- MutVar.readMutVar xv
    let gpv = tParent p0

    when (gpv /= pv) $ MutVar.modifyMutVar' gpv $ \gp ->
        if tLeft gp == pv then gp {tLeft = xv} else gp {tRight = xv}

    when (tLeft x0 /= xv) $ MutVar.modifyMutVar' (tLeft x0) $ \l ->
        l {tParent = pv}

    MutVar.writeMutVar xv $! x0
        { tAgg    = tAgg p0
        , tLeft   = pv
        , tParent = if gpv == pv then xv else gpv
        }

    let plv = tLeft p0
    pla <- if plv == pv then return mempty else tAgg <$> MutVar.readMutVar plv
    pra <- if tLeft x0 == xv then return mempty else tAgg <$> MutVar.readMutVar (tLeft x0)

    MutVar.writeMutVar pv $! p0
        { tRight  = if tLeft x0 == xv then pv else tLeft x0
        , tParent = xv
        , tAgg    = pla <> tValue p0 <> pra
        }

rotateRight pv xv = do
    p0 <- MutVar.readMutVar pv
    x0 <- MutVar.readMutVar xv
    let gpv = tParent p0

    when (gpv /= pv) $ MutVar.modifyMutVar' gpv $ \gp ->
        if tLeft gp == pv then gp {tLeft = xv} else gp {tRight = xv}

    when (tRight x0 /= xv) $ MutVar.modifyMutVar' (tRight x0) $ \l ->
        l {tParent = pv}

    MutVar.writeMutVar xv $! x0
        { tAgg    = tAgg p0
        , tRight  = pv
        , tParent = if gpv == pv then xv else gpv
        }

    let prv = tRight p0
    pla <- if tRight x0 == xv then return mempty else tAgg <$> MutVar.readMutVar (tRight x0)
    pra <- if prv == pv then return mempty else tAgg <$> MutVar.readMutVar prv

    MutVar.writeMutVar pv $! p0
        { tLeft   = if tRight x0 == xv then pv else tRight x0
        , tParent = xv
        , tAgg    = pla <> tValue p0 <> pra
        }

setLeft, setRight
    :: PrimMonad m
    => Tree (PrimState m) a v  -- Parent
    -> Tree (PrimState m) a v  -- New child
    -> m ()
setLeft p x = do
    MutVar.modifyMutVar' x $ \x' -> x' {tParent = p}
    MutVar.modifyMutVar' p $ \p' -> p' {tLeft = x}
setRight p x = do
    MutVar.modifyMutVar' x $ \x' -> x' {tParent = p}
    MutVar.modifyMutVar' p $ \p' -> p' {tRight = x}

removeParent, removeLeft, removeRight
    :: PrimMonad m
    => Tree (PrimState m) a v -- Parent
    -> m ()
removeParent x = MutVar.modifyMutVar' x $ \x' -> x' {tParent = x}
removeLeft   x = MutVar.modifyMutVar' x $ \x' -> x' {tLeft = x}
removeRight  x = MutVar.modifyMutVar' x $ \x' -> x' {tRight = x}

-- | Recompute the aggregate of a node.
updateAggregate
    :: (Monoid v, PrimMonad m)
    => Tree (PrimState m) a v
    -> m ()
updateAggregate t = do
    t' <- MutVar.readMutVar t
    let l = tLeft t'
    la <- if l == t then return mempty else tAgg <$> MutVar.readMutVar l
    let r = tRight t'
    ra <- if r == t then return mempty else tAgg <$> MutVar.readMutVar r
    let !agg = la <> tValue t' <> ra
    MutVar.writeMutVar t $! t' {tAgg = agg}

-- | For debugging/testing.
freeze :: PrimMonad m => Tree (PrimState m) a v -> m (Tree.Tree a)
freeze tree = do
    Tree {..} <- MutVar.readMutVar tree
    children  <- sequence $
        [freeze tLeft | tLeft /= tree] ++
        [freeze tRight | tRight /= tree]
    return $ Tree.Node tLabel children

print :: Show a => Tree (PrimState IO) a v -> IO ()
print = go 0
  where
    go d t = do
        Tree{..} <- MutVar.readMutVar t
        when (tLeft /= t) $ go (d + 1) tLeft

        putStrLn $ replicate d ' ' ++ show tLabel

        when (tRight /= t) $ go (d + 1) tRight

assertInvariants
    :: (PrimMonad m, Monoid v, Eq v, Show v) => Tree (PrimState m) a v -> m ()
assertInvariants t = do
    _ <- computeAgg t t
    return ()
  where
    -- TODO: Check average
    computeAgg p x = do
        x' <- MutVar.readMutVar x
        let p' = tParent x'
        when (p /= p') $ fail "broken parent pointer"

        let l = tLeft x'
        let r = tRight x'
        la <- if l == x then return mempty else computeAgg x l
        ra <- if r == x then return mempty else computeAgg x r

        let actualAgg = la <> (tValue x') <> ra
        let storedAgg = tAgg x'

        when (actualAgg /= storedAgg) $ fail $
            "error in stored aggregates: " ++ show storedAgg ++
            ", actual: " ++ show actualAgg

        return actualAgg
