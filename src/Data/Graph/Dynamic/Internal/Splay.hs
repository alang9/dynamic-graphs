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

data Tree s a v = Tree
    { tParent :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tLeft   :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tRight  :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tLabel  :: !a
    , tValue  :: !v
    , tAgg    :: {-# UNPACK #-} !(MutVar s v)
    }

instance Eq (Tree s a v) where
    -- Reference equality through a MutVar.
    t1 == t2 = tParent t1 == tParent t2

singleton :: PrimMonad m => a -> v -> m (Tree (PrimState m) a v)
singleton tLabel tValue = do
    tParent <- MutVar.newMutVar undefined
    tLeft   <- MutVar.newMutVar undefined
    tRight  <- MutVar.newMutVar undefined
    tAgg    <- MutVar.newMutVar tValue
    let tree = Tree {..}
    MutVar.writeMutVar tParent tree
    MutVar.writeMutVar tLeft   tree
    MutVar.writeMutVar tRight  tree
    return tree

getRoot :: PrimMonad m => Tree (PrimState m) a v -> m (Tree (PrimState m) a v)
getRoot tree@Tree {..} = do
    parent <- MutVar.readMutVar tParent
    if parent == tree then return tree else getRoot parent

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
    l <- MutVar.readMutVar (tLeft x)
    r <- MutVar.readMutVar (tRight x)
    removeParent l  -- Works even if l is x
    removeParent r
    removeLeft  x
    removeRight x
    return
        ( if l == x then Nothing else Just l
        , if r == x then Nothing else Just r
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
aggregate x = MutVar.readMutVar (tAgg x)

-- | For debugging/testing.
toList
    :: PrimMonad m => Tree (PrimState m) a v -> m [a]
toList = go []
  where
    go acc0 tree@Tree {..} = do
        left   <- MutVar.readMutVar tLeft
        right  <- MutVar.readMutVar tRight
        acc1   <- if right == tree then return acc0 else go acc0 right
        let acc2 = tLabel : acc1
        if left  == tree then return acc2 else go acc2 left

splay
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)  -- Returns the old root.
splay x0 = go x0 x0
  where
    go closestToRootFound x = do
        p <- MutVar.readMutVar (tParent x)
        if p == x then
            return closestToRootFound
        else do
            gp <- MutVar.readMutVar (tParent p)
            lp <- MutVar.readMutVar (tLeft p)
            if gp == p
                then do
                    -- ZIG
                    if lp == x then rotateRight p x else rotateLeft p x
                else do
                    lgp <- MutVar.readMutVar (tLeft gp)
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
getRightMost t@Tree {..} = do
    tr <- MutVar.readMutVar tRight
    if t == tr then return t else getRightMost tr

rotateLeft, rotateRight
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v  -- X's parent
    -> Tree (PrimState m) a v  -- X
    -> m ()
rotateLeft p x = do
    pa <- MutVar.readMutVar (tAgg p)
    b  <- MutVar.readMutVar (tLeft x)
    if b == x then removeRight p else setRight p b
    gp <- MutVar.readMutVar (tParent p)
    if gp == p then removeParent x else replace gp p x
    setLeft x p
    MutVar.writeMutVar (tAgg x) pa
    updateAggregate p
rotateRight p x = do
    pa <- MutVar.readMutVar (tAgg p)
    b  <- MutVar.readMutVar (tRight x)
    if b == x then removeLeft p else setLeft p b
    gp <- MutVar.readMutVar (tParent p)
    if gp == p then removeParent x else replace gp p x
    setRight x p
    MutVar.writeMutVar (tAgg x) pa
    updateAggregate p

setLeft, setRight
    :: PrimMonad m
    => Tree (PrimState m) a v  -- Parent
    -> Tree (PrimState m) a v  -- New child
    -> m ()
setLeft p x = do
    MutVar.writeMutVar (tParent x) p
    MutVar.writeMutVar (tLeft p) x
setRight p x = do
    MutVar.writeMutVar (tParent x) p
    MutVar.writeMutVar (tRight p) x

removeParent, removeLeft, removeRight
    :: PrimMonad m
    => Tree (PrimState m) a v -- Parent
    -> m ()
removeParent x = MutVar.writeMutVar (tParent x) x
removeLeft   x = MutVar.writeMutVar (tLeft x)   x
removeRight  x = MutVar.writeMutVar (tRight x)  x

-- | Replace X by Y in the tree.  X must have a parent.
replace
    :: PrimMonad m
    => Tree (PrimState m) a v  -- ^ X's parent
    -> Tree (PrimState m) a v  -- ^ X
    -> Tree (PrimState m) a v  -- ^ Y
    -> m ()
replace p x y = do
    pl <- MutVar.readMutVar (tLeft p)
    MutVar.writeMutVar (tParent y) p
    if pl == x
        then MutVar.writeMutVar (tLeft p) y
        else MutVar.writeMutVar (tRight p) y

-- | Recompute the aggregate of a node.
updateAggregate
    :: (Monoid v, PrimMonad m)
    => Tree (PrimState m) a v
    -> m ()
updateAggregate t = do
    l  <- MutVar.readMutVar (tLeft t)
    la <- if l == t then return mempty else MutVar.readMutVar (tAgg l)
    r  <- MutVar.readMutVar (tRight t)
    ra <- if r == t then return mempty else MutVar.readMutVar (tAgg r)
    let !agg = la <> tValue t <> ra
    MutVar.writeMutVar (tAgg t) agg

-- | For debugging/testing.
freeze :: PrimMonad m => Tree (PrimState m) a v -> m (Tree.Tree a)
freeze tree@Tree {..} = do
    left  <- MutVar.readMutVar tLeft
    right <- MutVar.readMutVar tRight
    children  <- sequence $
        [freeze left  | left /= tree] ++
        [freeze right | right /= tree]
    return $ Tree.Node tLabel children

print :: Show a => Tree (PrimState IO) a v -> IO ()
print = go 0
  where
    go d t@Tree {..} = do
        left <- MutVar.readMutVar tLeft
        when (left /= t) $ go (d + 1) left

        putStrLn $ replicate d ' ' ++ show tLabel

        right <- MutVar.readMutVar tRight
        when (right /= t) $ go (d + 1) right

assertInvariants
    :: (PrimMonad m, Monoid v, Eq v, Show v) => Tree (PrimState m) a v -> m ()
assertInvariants t = do
    _ <- computeAgg t t
    return ()
  where
    -- TODO: Check average
    computeAgg p x = do
        p' <- MutVar.readMutVar (tParent x)
        when (p /= p') $ fail "broken parent pointer"

        l <- MutVar.readMutVar (tLeft x)
        r <- MutVar.readMutVar (tRight x)
        la <- if l == x then return mempty else computeAgg x l
        ra <- if r == x then return mempty else computeAgg x r

        let actualAgg = la <> (tValue x) <> ra
        storedAgg <- MutVar.readMutVar (tAgg x)

        when (actualAgg /= storedAgg) $ fail $
            "error in stored aggregates: " ++ show storedAgg ++
            ", actual: " ++ show actualAgg

        return actualAgg
