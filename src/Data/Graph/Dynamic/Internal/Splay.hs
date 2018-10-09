{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dynamic.Internal.Splay
    ( Tree

    , singleton
    , cons
    , snoc
    , append
    , concat
    , split
    , connected
    , root
    , aggregate
    , toList
    , propagate
    , isNil

    -- * Debugging only
    , Tree' (..)
    , getRoot
    , freeze
    , print
    , assertInvariants
    , splay
    ) where

import           Control.Monad           (foldM, when, void)
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Monoid             ((<>))
import           Data.Primitive.MutVar   (MutVar)
import qualified Data.Primitive.MutVar   as MutVar
import qualified Data.Tree               as Tree
import           Prelude                 hiding (concat, print)
import           System.IO.Unsafe        (unsafePerformIO)
import           Unsafe.Coerce           (unsafeCoerce)

data Tree' s a v = Tree
    { tParent :: {-# UNPACK #-} !(Tree s a v)
    , tLeft   :: {-# UNPACK #-} !(Tree s a v)
    , tRight  :: {-# UNPACK #-} !(Tree s a v)
    , tLabel  :: !a
    , tValue  :: !v
    , tAgg    :: !v
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
nil :: Tree s a v
nil = unsafeCoerce $ unsafePerformIO $ MutVar.newMutVar undefined
{-# NOINLINE nil #-}

type Tree s a v = MutVar s (Tree' s a v)

isNil :: Tree s a v -> Bool
isNil v = v == nil

singleton :: PrimMonad m => a -> v -> m (Tree (PrimState m) a v)
singleton tLabel tValue =
    MutVar.newMutVar $! Tree nil nil nil tLabel tValue tValue

getRoot :: PrimMonad m => Tree (PrimState m) a v -> m (Tree (PrimState m) a v)
getRoot tree = do
    Tree{..} <- MutVar.readMutVar tree
    if tParent == nil then return tree else getRoot tParent

-- | `lv` must be a singleton tree
cons
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
cons lv rv = do
    r <- MutVar.readMutVar rv
    MutVar.modifyMutVar' lv $ \l -> l {tRight = rv, tAgg = tAgg l <> tAgg r}
    MutVar.writeMutVar rv $! r {tParent = lv}
    return lv

-- | `rv` must be a singleton tree
snoc
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
snoc lv rv = do
    l <- MutVar.readMutVar lv
    MutVar.modifyMutVar' rv $ \r -> r {tLeft = lv, tAgg = tAgg l <> tAgg r}
    MutVar.writeMutVar lv $! l {tParent = rv}
    return rv

-- | Appends two trees.  Returns the root of the tree.
append
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
append xv yv = do
    rm <- getRightMost xv
    _  <- splay rm
    y  <- MutVar.readMutVar yv
    MutVar.modifyMutVar rm $ \r -> r {tRight = yv, tAgg = tAgg r <> tAgg y}
    MutVar.writeMutVar yv $! y {tParent = rm}
    return rm
  where
    getRightMost tv = do
        t <- MutVar.readMutVar tv
        if tRight t == nil then return tv else getRightMost (tRight t)

concat
    :: forall m a v. (PrimMonad m, Monoid v)
    => NonEmpty (Tree (PrimState m) a v)
    -> m (Tree (PrimState m) a v)
concat trees0 =
    case trees0 of x NonEmpty.:| xs -> foldM append x xs

split
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Maybe (Tree (PrimState m) a v), Maybe (Tree (PrimState m) a v))
split x = do
    _ <- splay x
    Tree {..} <- MutVar.readMutVar x
    when (tLeft /= nil) (removeParent tLeft)  -- Works even if l is x
    when (tRight /= nil) (removeParent tRight)
    MutVar.writeMutVar x $ Tree {tAgg = tValue, ..}
    removeLeft  x
    removeRight x
    return
        ( if tLeft == nil then Nothing else Just tLeft
        , if tRight == nil then Nothing else Just tRight
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
        acc1 <- if tRight == nil then return acc0 else go acc0 tRight
        let acc2 = tLabel : acc1
        if tLeft == nil then return acc2 else go acc2 tLeft

propagate ::
  (PrimMonad m, Monoid v) => Tree (PrimState m) a v -> m ()
propagate xv = do
  void $ splay xv
  xv'@Tree {..} <- MutVar.readMutVar xv
  lAgg <- if tLeft == nil then return mempty else aggregate tLeft
  rAgg <- if tRight == nil then return mempty else aggregate tRight
  MutVar.writeMutVar xv $ xv' {tAgg = lAgg <> tValue <> rAgg}

splay
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)  -- Returns the old root.
splay xv = do
    -- Note (jaspervdj): Rather than repeatedly reading from/writing to xv we
    -- read x once and thread its (continuously updated) value through the
    -- entire stack of `go` calls.
    --
    -- The same is true for the left and right aggregates of x: they can be
    -- passed upwards rather than recomputed.
    x0 <- MutVar.readMutVar xv
    xla <- if tLeft x0 == nil then return mempty else tAgg <$> MutVar.readMutVar (tLeft x0)
    xra <- if tRight x0 == nil then return mempty else tAgg <$> MutVar.readMutVar (tRight x0)
    go xv xla xra x0
  where
    go closestToRootFound xla xra !x = do
        let !pv = tParent x
        if pv == nil then do
            MutVar.writeMutVar xv x
            return closestToRootFound
        else do
            p <- MutVar.readMutVar pv
            let gv = tParent p
            let plv = tLeft p
            let prv = tRight p
            let xlv = tLeft x
            let xrv = tRight x
            if  | gv == nil, plv == xv -> do
                    -- ZIG (Right)
                    --
                    --    p  =>  x
                    --   /        \
                    --  x          p
                    --   \        /
                    --    xr     xr
                    --
                    when (xrv /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                        xr {tParent = pv}

                    pra <- if prv == nil then return mempty else tAgg <$> MutVar.readMutVar prv
                    MutVar.writeMutVar pv $! p
                        { tLeft   = xrv
                        , tParent = xv
                        , tAgg    = xra <> tValue p <> pra
                        }

                    MutVar.writeMutVar xv $! x
                        { tAgg    = tAgg p
                        , tRight  = pv
                        , tParent = nil
                        }

                    return pv

                | gv == nil -> do
                    -- ZIG (Left)
                    --
                    --  p    =>    x
                    --   \        /
                    --    x      p
                    --   /        \
                    --  xl         xl
                    --
                    when (xlv /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                        xl {tParent = pv}

                    pla <- if plv == nil then return mempty else tAgg <$> MutVar.readMutVar plv
                    MutVar.writeMutVar pv $! p
                        { tRight  = xlv
                        , tParent = xv
                        , tAgg    = pla <> tValue p <> xla
                        }

                    MutVar.writeMutVar xv $! x
                        { tAgg    = tAgg p
                        , tLeft   = pv
                        , tParent = nil
                        }

                    return pv

                | otherwise -> do

                    g <- MutVar.readMutVar gv
                    let ggv = tParent g
                    let glv = tLeft g
                    let grv = tRight g
                    when (ggv /= nil) $ MutVar.modifyMutVar' ggv $ \gg ->
                        if tLeft gg == gv
                            then gg {tLeft = xv}
                            else gg {tRight = xv}

                    if  | plv == xv && glv == pv -> do
                            -- ZIGZIG (Right):
                            --
                            --       gg       gg
                            --       |        |
                            --       g        x
                            --      / \      / \
                            --     p     =>     p
                            --    / \          / \
                            --   x   pr       xr  g
                            --  / \              /
                            --     xr           pr
                            --

                            pra <- if prv == nil then return mempty else tAgg <$> MutVar.readMutVar prv
                            gra <- if grv == nil then return mempty else tAgg <$> MutVar.readMutVar grv
                            let !ga' = pra <> tValue g <> gra
                            when (prv /= nil) $ MutVar.modifyMutVar' prv $ \pr ->
                                pr {tParent = gv}

                            MutVar.writeMutVar gv $! g
                                { tParent = pv
                                , tLeft   = prv
                                , tAgg    = ga'
                                }

                            when (xrv /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = pv}

                            let !pa' = xra <> tValue p <> ga'
                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tLeft    = xrv
                                , tRight   = gv
                                , tAgg     = pa'
                                }

                            go gv xla pa' $! x
                                { tRight  = pv
                                , tAgg    = tAgg g
                                , tParent = ggv
                                }

                        | plv /= xv && glv /= pv -> do
                            -- ZIGZIG (Left):
                            --
                            --   gg               gg
                            --   |                |
                            --   g                x
                            --  / \              / \
                            --     p     =>     p
                            --    / \          / \
                            --   pl  x        g   xl
                            --      / \      / \
                            --     xl           pl
                            --
                            pla <- if plv == nil then return mempty else tAgg <$> MutVar.readMutVar plv
                            gla <- if glv == nil then return mempty else tAgg <$> MutVar.readMutVar glv
                            let !ga' = gla <> tValue g <> pla
                            when (plv /= nil) $ MutVar.modifyMutVar' plv $ \pl ->
                                pl {tParent = gv}

                            MutVar.writeMutVar gv $! g
                                { tParent = pv
                                , tRight  = plv
                                , tAgg    = ga'
                                }

                            when (xlv /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = pv}

                            let !pa' = ga' <> tValue p <> xla
                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tLeft   = gv
                                , tRight  = xlv
                                , tAgg    = pa'
                                }

                            go gv pa' xra $! x
                                { tLeft   = pv
                                , tAgg    = tAgg g
                                , tParent = ggv
                                }

                        | plv == xv -> do
                            -- ZIGZIG (Left):
                            --
                            --    gg            gg
                            --    |             |
                            --    g             x
                            --     \          /   \
                            --      p   =>  g       p
                            --     /         \     /
                            --    x           xl  xr
                            --   / \
                            --  xl  xr
                            --
                            when (xlv /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = gv}

                            gla <- if glv == nil then return mempty else tAgg <$> MutVar.readMutVar glv
                            let !ga' = gla <> tValue g <> xla
                            MutVar.writeMutVar gv $! g
                                { tParent = xv
                                , tRight  = xlv
                                , tAgg    = ga'
                                }

                            when (xrv /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = pv}

                            pra <- if prv == nil then return mempty else tAgg <$> MutVar.readMutVar prv
                            let pa' = xra <> tValue p <> pra
                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tLeft   = xrv
                                , tAgg    = pa'
                                }

                            go gv ga' pa' $! x
                                { tParent = ggv
                                , tLeft   = gv
                                , tRight  = pv
                                , tAgg    = tAgg g
                                }

                        | otherwise -> do
                            -- ZIGZIG (Right):
                            --
                            --    gg            gg
                            --    |             |
                            --    g             x
                            --   /            /   \
                            --  p       =>  p       g
                            --   \           \     /
                            --    x           xl  xr
                            --   / \
                            --  xl  xr
                            --
                            when (xrv /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = gv}

                            gra <- if grv == nil then return mempty else tAgg <$> MutVar.readMutVar grv
                            let !ga' = xra <> tValue g <> gra
                            MutVar.writeMutVar gv $! g
                                { tParent = xv
                                , tLeft   = xrv
                                , tAgg    = ga'
                                }

                            when (xlv /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = pv}

                            pla <- if plv == nil then return mempty else tAgg <$> MutVar.readMutVar plv
                            let !pa' = pla <> tValue p <> xla
                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tRight  = xlv
                                , tAgg    = pa'
                                }

                            go gv pa' ga' $! x
                                { tParent = ggv
                                , tLeft   = pv
                                , tRight  = gv
                                , tAgg    = tAgg g
                                }

removeParent, removeLeft, removeRight
    :: PrimMonad m
    => Tree (PrimState m) a v -- Parent
    -> m ()
removeParent x = MutVar.modifyMutVar' x $ \x' -> x' {tParent = nil}
removeLeft   x = MutVar.modifyMutVar' x $ \x' -> x' {tLeft = nil}
removeRight  x = MutVar.modifyMutVar' x $ \x' -> x' {tRight = nil}

-- | For debugging/testing.
freeze :: PrimMonad m => Tree (PrimState m) a v -> m (Tree.Tree a)
freeze tree = do
    Tree {..} <- MutVar.readMutVar tree
    children  <- sequence $
        [freeze tLeft | tLeft /= nil] ++
        [freeze tRight | tRight /= nil]
    return $ Tree.Node tLabel children

print :: Show a => Tree (PrimState IO) a v -> IO ()
print = go 0
  where
    go d t = do
        Tree{..} <- MutVar.readMutVar t
        when (tLeft /= nil) $ go (d + 1) tLeft

        putStrLn $ replicate d ' ' ++ show tLabel

        when (tRight /= nil) $ go (d + 1) tRight

assertInvariants
    :: (PrimMonad m, Monoid v, Eq v, Show v) => Tree (PrimState m) a v -> m ()
assertInvariants t = do
    _ <- computeAgg nil t
    return ()
  where
    -- TODO: Check average
    computeAgg p x = do
        x' <- MutVar.readMutVar x
        let p' = tParent x'
        when (p /= p') $ fail "broken parent pointer"

        let l = tLeft x'
        let r = tRight x'
        la <- if l == nil then return mempty else computeAgg x l
        ra <- if r == nil then return mempty else computeAgg x r

        let actualAgg = la <> (tValue x') <> ra
        let storedAgg = tAgg x'

        when (actualAgg /= storedAgg) $ fail $
            "error in stored aggregates: " ++ show storedAgg ++
            ", actual: " ++ show actualAgg

        return actualAgg
