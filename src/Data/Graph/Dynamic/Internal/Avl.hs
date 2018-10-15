{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
module Data.Graph.Dynamic.Internal.Avl
    ( Tree

    , singleton
    , append
    , concat
    , join
    , split
    , root
    , connected
    , aggregate
    , toList

    -- * Debugging only
    , freeze
    , print
    , assertInvariants
    , assertSingleton
    , assertRoot
    ) where

import           Control.Monad                    (foldM, when)
import           Control.Monad.Primitive          (PrimMonad (..))
import qualified Data.Graph.Dynamic.Internal.Tree as Class
import           Data.List.NonEmpty               (NonEmpty)
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Monoid                      ((<>))
import           Data.Primitive.MutVar            (MutVar)
import qualified Data.Primitive.MutVar            as MutVar
import qualified Data.Tree                        as Tree
import           Prelude                          hiding (concat, print)

data Tree s a v = Tree
    { tParent :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tLeft   :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tRight  :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tAggs   :: {-# UNPACK #-} !(MutVar s (Aggs v))
    , tLabel  :: !a
    , tValue  :: !v
    }

instance Eq (Tree s a v) where
    -- Reference equality through a MutVar.
    t1 == t2 = tParent t1 == tParent t2

data Aggs v = Aggs
    { aHeight    :: {-# UNPACK #-} !Int
    , aAggregate :: !v
    } deriving (Eq, Show)

emptyAggs :: Monoid v => Aggs v
emptyAggs = Aggs 0 mempty

singletonAggs :: v -> Aggs v
singletonAggs = Aggs 1

joinAggs :: Monoid v => Aggs v -> v -> Aggs v -> Aggs v
joinAggs (Aggs lh la) a (Aggs rh ra) =
    Aggs (max lh rh + 1) (la <> a <> ra)

singleton :: PrimMonad m => a -> v -> m (Tree (PrimState m) a v)
singleton tLabel tValue = do
    tParent <- MutVar.newMutVar undefined
    tLeft   <- MutVar.newMutVar undefined
    tRight  <- MutVar.newMutVar undefined
    tAggs   <- MutVar.newMutVar $ singletonAggs tValue
    let tree = Tree {..}
    MutVar.writeMutVar tParent tree
    MutVar.writeMutVar tLeft   tree
    MutVar.writeMutVar tRight  tree
    return tree

root :: PrimMonad m => Tree (PrimState m) a v -> m (Tree (PrimState m) a v)
root tree@Tree {..} = do
    parent <- MutVar.readMutVar tParent
    if parent == tree then return tree else root parent

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
split x0 = do
    (mbL, mbR, p, left) <- cut x0
    if p == x0 then
        return (mbL, mbR)
    else do
        upwards mbL mbR p left
  where
    upwards lacc0 racc0 x left0 = do
        (mbL, mbR, p, left1) <- cut x
        if left0 then do
            racc1 <- join racc0 x mbR
            if p == x then
                return (lacc0, Just racc1)
            else
                upwards lacc0 (Just racc1) p left1
        else do
            lacc1 <- join mbL x lacc0
            if p == x then
                return (Just lacc1, racc0)
            else
                upwards (Just lacc1) racc0 p left1

    cut x = do
        p  <- MutVar.readMutVar (tParent x)
        pl <- MutVar.readMutVar (tLeft p)
        l <- MutVar.readMutVar (tLeft x)
        r <- MutVar.readMutVar (tRight x)
        when (l /= x) $ removeParent l
        when (r /= x) $ removeParent r
        removeParent x
        removeLeft  x
        removeRight x
        updateAggs x
        if pl == x then removeLeft p else removeRight p
        return
            ( if l == x then Nothing else Just l
            , if r == x then Nothing else Just r
            , p
            , pl == x
            )

append
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
append l0 r0 = do
    -- NOTE: there is a faster way to do this by just following the right spine
    -- and joining along the way.
    rm <- getRightMost l0
    (mbL, mbR) <- split rm
    case mbR of
        Just _ -> error "append: invalid state"
        _      -> assertSingleton rm
    join mbL rm (Just r0)
  where
    getRightMost x = do
        r <- MutVar.readMutVar (tRight x)
        if r == x then return x else getRightMost r

connected
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m Bool
connected x y = do
    xr <- root x
    yr <- root y
    return $ xr == yr

aggregate :: (PrimMonad m, Monoid v) => Tree (PrimState m) a v -> m v
aggregate = fmap aAggregate . MutVar.readMutVar . tAggs

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

join
    :: (PrimMonad m, Monoid v)
    => Maybe (Tree (PrimState m) a v)
    -> Tree (PrimState m) a v  -- Must be a singleton
    -> Maybe (Tree (PrimState m) a v)
    -> m (Tree (PrimState m) a v)
join mbL c mbR = do
    lh <- maybe (return 0) (fmap aHeight . MutVar.readMutVar . tAggs) mbL
    rh <- maybe (return 0) (fmap aHeight . MutVar.readMutVar . tAggs) mbR
    if  | lh > rh + 1, Just l <- mbL ->
            joinRight l c mbR
        | rh > lh + 1, Just r <- mbR ->
            joinLeft mbL c r
        | otherwise -> do
            case mbL of Just l -> setLeft  c l; _ -> return ()
            case mbR of Just r -> setRight c r; _ -> return ()
            updateAggs c
            return c

joinLeft
    :: (PrimMonad m, Monoid v)
    => Maybe (Tree (PrimState m) a v)
    -> Tree (PrimState m) a v  -- Must be a singleton
    -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
joinLeft mbL c r = do
    rl  <- MutVar.readMutVar (tLeft r)
    rla <- leftAggs r rl

    rr  <- MutVar.readMutVar (tRight r)
    rra <- rightAggs r rr

    la  <- maybe (return emptyAggs) (MutVar.readMutVar . tAggs) mbL

    if aHeight rla <= aHeight la + 1 then do
        setLeft r c
        when (rl /= r) $ setRight c rl
        case mbL of Just l -> setLeft c l; _ -> return ()

        let !ca = joinAggs rla (tValue c) la

        -- Invalidity in the parent is fixed with two rotations
        if aHeight rra + 1 < aHeight ca then do
            rotateLeft c rl
            rotateRight r rl

            updateAggs c
            updateAggs r
            updateAggsToRoot rl
        else do
            -- One rotation
            updateAggs c
            updateAggs r
            upLeft r
    else
        joinLeft mbL c rl

upLeft
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
upLeft l = do
    p <- MutVar.readMutVar (tParent l)
    if p == l then
        return l
    else do
        r <- MutVar.readMutVar (tRight p)
        ra <- rightAggs p r
        la <- leftAggs p l
        if aHeight ra + 1 < aHeight la then do
            rotateRight p l
            updateAggs p
            updateAggsToRoot l
        else do
            updateAggs p  -- Stuff below us might have changed.
            upLeft p

joinRight
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v  -- Must be a singleton
    -> Maybe (Tree (PrimState m) a v)
    -> m (Tree (PrimState m) a v)
joinRight l c mbR = do
    lr  <- MutVar.readMutVar (tRight l)
    lra <- rightAggs l lr

    ll  <- MutVar.readMutVar (tLeft l)
    lla <- leftAggs l ll

    ra <- maybe (return emptyAggs) (MutVar.readMutVar . tAggs) mbR

    if aHeight lra <= aHeight ra + 1 then do
        setRight l c
        when (lr /= l) $ setLeft c lr
        case mbR of Just r -> setRight c r; _ -> return ()

        let !ca = joinAggs lra (tValue c) ra

        -- Invalidity in the parent is fixed with two rotations
        if aHeight lla + 1 < aHeight ca then do
            rotateRight c lr
            rotateLeft l lr

            -- Many of these are already computed...
            updateAggs l
            updateAggs c
            updateAggsToRoot lr
        else do
            -- One rotation
            updateAggs c
            updateAggs l
            upRight l
    else
        joinRight lr c mbR

upRight
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
upRight r = do
    p <- MutVar.readMutVar (tParent r)
    if p == r then
        return p
    else do
        l <- MutVar.readMutVar (tLeft p)
        la <- leftAggs p l
        ra <- rightAggs p r
        if aHeight la + 1 < aHeight ra then do
            rotateLeft p r
            updateAggs p
            updateAggsToRoot r
        else do
            updateAggs p  -- Stuff below us might have changed.
            upRight p

rotateLeft, rotateRight
    :: PrimMonad m
    => Tree (PrimState m) a v  -- X's parent
    -> Tree (PrimState m) a v  -- X
    -> m ()
rotateLeft p x = do
    b <- MutVar.readMutVar (tLeft x)
    if b == x then removeRight p else setRight p b
    gp <- MutVar.readMutVar (tParent p)
    if gp == p then removeParent x else replace gp p x
    setLeft x p
rotateRight p x = do
    b <- MutVar.readMutVar (tRight x)
    if b == x then removeLeft p else setLeft p b
    gp <- MutVar.readMutVar (tParent p)
    if gp == p then removeParent x else replace gp p x
    setRight x p

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

leftAggs, rightAggs
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v  -- Parent
    -> Tree (PrimState m) a v  -- Left or right child
    -> m (Aggs v)
leftAggs  p l =
    if p == l then return emptyAggs else MutVar.readMutVar (tAggs l)
rightAggs p r =
    if p == r then return emptyAggs else MutVar.readMutVar (tAggs r)

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

-- | Recompute the aggregate and height of a node.
updateAggs
    :: (Monoid v, PrimMonad m)
    => Tree (PrimState m) a v
    -> m ()
updateAggs t = do
    l  <- MutVar.readMutVar (tLeft t)
    r  <- MutVar.readMutVar (tRight t)
    la <- leftAggs  t l
    ra <- rightAggs t r
    let !agg = joinAggs la (tValue t) ra
    MutVar.writeMutVar (tAggs t) agg

-- | Recompute aggregate and height all the way to the root of the tree.
updateAggsToRoot
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
updateAggsToRoot x = do
    updateAggs x
    p <- MutVar.readMutVar (tParent x)
    if p == x then return x else updateAggsToRoot p

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
    _ <- computeAggs t t
    return ()
  where
    -- TODO: Check average
    computeAggs p x = do
        p' <- MutVar.readMutVar (tParent x)
        when (p /= p') $ fail "broken parent pointer"

        l <- MutVar.readMutVar (tLeft x)
        r <- MutVar.readMutVar (tRight x)
        la <- if l == x then return emptyAggs else computeAggs x l
        ra <- if r == x then return emptyAggs else computeAggs x r

        let actualAggs = joinAggs la (tValue x) ra
        storedAggs <- MutVar.readMutVar (tAggs x)

        when (actualAggs /= storedAggs) $ fail $
            "error in stored aggregates: " ++ show storedAggs ++
            ", actual: " ++ show actualAggs

        when (abs (aHeight la - aHeight ra) > 1) $ fail "inbalanced"
        return actualAggs

assertSingleton :: PrimMonad m => Tree (PrimState m) a v -> m ()
assertSingleton x = do
    l <- MutVar.readMutVar (tLeft x)
    r <- MutVar.readMutVar (tRight x)
    p <- MutVar.readMutVar (tParent x)
    when (l /= x || r /= x || p /= x) $ fail "not a singleton"

assertRoot :: PrimMonad m => Tree (PrimState m) a v -> m ()
assertRoot x = do
    p <- MutVar.readMutVar (tParent x)
    when (p /= x) $ fail "not the root"

data TreeGen s = TreeGen

instance Class.Tree Tree where
    type TreeGen Tree = TreeGen
    newTreeGen _ = return TreeGen

    singleton _ = singleton
    append      = append
    split       = split
    connected   = connected
    root        = root
    aggregate   = aggregate
    toList      = toList

instance Class.TestTree Tree where
    print            = print
    assertInvariants = assertInvariants
    assertSingleton  = assertSingleton
    assertRoot       = assertRoot
