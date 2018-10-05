{-# LANGUAGE BangPatterns    #-}
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
splay xv = do
    -- Note (jaspervdj): Rather than repeatedly reading from/writing to xv we
    -- read x once and thread its (continuously updated) value through the
    -- entire stack of `go` calls.
    --
    -- The same is true for the left and right aggregates of x: they can be
    -- passed upwards rather than recomputed.
    x0 <- MutVar.readMutVar xv
    xla <- if tLeft x0 == xv then return mempty else tAgg <$> MutVar.readMutVar (tLeft x0)
    xra <- if tRight x0 == xv then return mempty else tAgg <$> MutVar.readMutVar (tRight x0)
    go xv xla xra x0
  where
    go closestToRootFound xla xra !x = do
        let !pv = tParent x
        if pv == xv then do
            MutVar.writeMutVar xv x
            return closestToRootFound
        else do
            p <- MutVar.readMutVar pv
            let gv = tParent p
            let plv = tLeft p
            let prv = tRight p
            let xlv = tLeft x
            let xrv = tRight x
            if  | gv == pv, plv == xv -> do
                    -- ZIG (Right)
                    --
                    --    p  =>  x
                    --   /        \
                    --  x          p
                    --   \        /
                    --    xr     xr
                    --
                    when (xrv /= xv) $ MutVar.modifyMutVar' xrv $ \xr ->
                        xr {tParent = pv}

                    pra <- if prv == pv then return mempty else tAgg <$> MutVar.readMutVar prv

                    MutVar.writeMutVar pv $! p
                        { tLeft   = if xrv == xv then pv else xrv
                        , tParent = xv
                        , tAgg    = xra <> tValue p <> pra
                        }

                    MutVar.writeMutVar xv $! x
                        { tAgg    = tAgg p
                        , tRight  = pv
                        , tParent = xv
                        }

                    return pv

                | gv == pv -> do
                    -- ZIG (Left)
                    --
                    --  p    =>    x
                    --   \        /
                    --    x      p
                    --   /        \
                    --  xl         xl
                    --
                    when (xlv /= xv) $ MutVar.modifyMutVar' xlv $ \xl ->
                        xl {tParent = pv}

                    pla <- if plv == pv then return mempty else tAgg <$> MutVar.readMutVar plv

                    MutVar.writeMutVar pv $! p
                        { tRight  = if xlv == xv then pv else xlv
                        , tParent = xv
                        , tAgg    = pla <> tValue p <> xla
                        }

                    MutVar.writeMutVar xv $! x
                        { tAgg    = tAgg p
                        , tLeft   = pv
                        , tParent = xv
                        }

                    return pv

                | otherwise -> do

                    g <- MutVar.readMutVar gv
                    let ggv = tParent g
                    let glv = tLeft g
                    let grv = tRight g
                    when (ggv /= gv) $ MutVar.modifyMutVar' ggv $ \gg ->
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

                            pra <- if prv == pv then return mempty else tAgg <$> MutVar.readMutVar prv
                            gra <- if grv == gv then return mempty else tAgg <$> MutVar.readMutVar grv

                            let !ga' = pra <> tValue g <> gra

                            when (prv /= pv) $ MutVar.modifyMutVar' prv $ \pr ->
                                pr {tParent = gv}

                            MutVar.writeMutVar gv $! g
                                { tParent = pv
                                , tLeft   = if prv == pv then gv else prv
                                , tAgg    = ga'
                                }

                            when (xrv /= xv) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = pv}

                            let !pa' = xra <> tValue p <> ga'

                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tLeft    = if xrv == xv then pv else xrv
                                , tRight   = gv
                                , tAgg     = pa'
                                }

                            go gv xla pa' $! x
                                { tRight  = pv
                                , tAgg    = tAgg g
                                , tParent = if ggv == gv then xv else ggv
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

                            pla <- if plv == pv then return mempty else tAgg <$> MutVar.readMutVar plv
                            gla <- if glv == gv then return mempty else tAgg <$> MutVar.readMutVar glv

                            let !ga' = gla <> tValue g <> pla

                            when (plv /= pv) $ MutVar.modifyMutVar' plv $ \pl ->
                                pl {tParent = gv}

                            MutVar.writeMutVar gv $! g
                                { tParent = pv
                                , tRight  = if plv == pv then gv else plv
                                , tAgg    = ga'
                                }

                            when (xlv /= xv) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = pv}

                            let !pa' = ga' <> tValue p <> xla

                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tLeft   = gv
                                , tRight  = if xlv == xv then pv else xlv
                                , tAgg    = pa'
                                }

                            go gv pa' xra $! x
                                { tLeft   = pv
                                , tAgg    = tAgg g
                                , tParent = if ggv == gv then xv else ggv
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


                            gla <- if glv == gv then return mempty else tAgg <$> MutVar.readMutVar glv

                            when (xlv /= xv) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = gv}

                            let !ga' = gla <> tValue g <> xla
                            MutVar.writeMutVar gv $! g
                                { tParent = xv
                                , tRight  = if xlv == xv then gv else xlv
                                , tAgg    = ga'
                                }

                            pra <- if prv == pv then return mempty else tAgg <$> MutVar.readMutVar grv

                            when (xrv /= xv) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = pv}

                            let pa' = xra <> tValue p <> pra
                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tLeft   = if xrv == xv then pv else xrv
                                , tAgg    = pa'
                                }

                            go gv ga' pa' $! x
                                { tParent = if ggv == gv then xv else ggv
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
                            gra <- if grv == gv then return mempty else tAgg <$> MutVar.readMutVar grv

                            when (xrv /= xv) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = gv}

                            let !ga' = xra <> tValue g <> gra
                            MutVar.writeMutVar gv $! g
                                { tParent = xv
                                , tLeft   = if xrv == xv then gv else xrv
                                , tAgg    = ga'
                                }

                            pla <- if plv == pv then return mempty else tAgg <$> MutVar.readMutVar glv

                            when (xlv /= xv) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = pv}

                            let !pa' = pla <> tValue p <> xla
                            MutVar.writeMutVar pv $! p
                                { tParent = xv
                                , tRight  = if xlv == xv then pv else xlv
                                , tAgg    = pa'
                                }

                            go gv pa' ga' $! x
                                { tParent = if ggv == gv then xv else ggv
                                , tLeft   = pv
                                , tRight  = gv
                                , tAgg    = tAgg g
                                }

getRightMost
    :: PrimMonad m
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
getRightMost t = do
    tr <- tRight <$> MutVar.readMutVar t
    if t == tr then return t else getRightMost tr

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
