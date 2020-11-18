{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Graph.Dynamic.Internal.Splay
    ( Tree

    , singleton
    , cons
    , snoc
    , append
    , split
    , connected
    , root
    , aggregate
    , toList

    -- * Debugging only
    , readRoot
    , freeze
    , print
    , assertInvariants
    ) where

import           Control.Monad                    (when)
import           Control.Monad.Primitive          (PrimMonad (..))
import qualified Data.Graph.Dynamic.Internal.Tree as Class

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid                      ((<>))
#endif

import           Data.Primitive.MutVar            (MutVar)
import qualified Data.Primitive.MutVar            as MutVar
import qualified Data.Tree                        as Tree
import           Prelude                          hiding (concat, print)
import           System.IO.Unsafe                 (unsafePerformIO)
import           Unsafe.Coerce                    (unsafeCoerce)

data T s a v = T
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
nil = unsafeCoerce $ unsafePerformIO $ fmap Tree $ MutVar.newMutVar undefined
{-# NOINLINE nil #-}

newtype Tree s a v = Tree {unTree :: MutVar s (T s a v)}
    deriving (Eq)

singleton :: PrimMonad m => a -> v -> m (Tree (PrimState m) a v)
singleton tLabel tValue =
    fmap Tree $ MutVar.newMutVar $! T nil nil nil tLabel tValue tValue

readRoot :: PrimMonad m => Tree (PrimState m) a v -> m (Tree (PrimState m) a v)
readRoot tree = do
    T {..} <- MutVar.readMutVar (unTree tree)
    if tParent == nil then return tree else readRoot tParent

-- | `lv` must be a singleton tree
cons
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
cons lt@(Tree lv) rt@(Tree rv) = do
    r <- MutVar.readMutVar rv
    MutVar.modifyMutVar' lv $ \l -> l {tRight = rt, tAgg = tAgg l <> tAgg r}
    MutVar.writeMutVar rv $! r {tParent = lt}
    return lt

-- | `rv` must be a singleton tree
snoc
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
snoc lt@(Tree lv) rt@(Tree rv) = do
    l <- MutVar.readMutVar lv
    MutVar.modifyMutVar' rv $ \r -> r {tLeft = lt, tAgg = tAgg l <> tAgg r}
    MutVar.writeMutVar lv $! l {tParent = rt}
    return rt

-- | Appends two trees.  Returns the root of the tree.
append
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
append xt@(Tree _xv) yt@(Tree yv) = do
    rmt@(Tree rmv) <- getRightMost xt
    _              <- splay rmt
    y              <- MutVar.readMutVar yv
    MutVar.modifyMutVar rmv $ \r -> r {tRight = yt, tAgg = tAgg r <> tAgg y}
    MutVar.writeMutVar yv $! y {tParent = rmt}
    return rmt
  where
    getRightMost tt@(Tree tv) = do
        t <- MutVar.readMutVar tv
        if tRight t == nil then return tt else getRightMost (tRight t)

split
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Maybe (Tree (PrimState m) a v), Maybe (Tree (PrimState m) a v))
split xt@(Tree xv) = do
    _ <- splay xt
    T {..} <- MutVar.readMutVar xv
    when (tLeft /= nil) (removeParent tLeft)  -- Works even if l is x
    when (tRight /= nil) (removeParent tRight)
    MutVar.writeMutVar xv $ T {tAgg = tValue, ..}
    removeLeft  xt
    removeRight xt
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

label
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m a
label (Tree xv) = tLabel <$> MutVar.readMutVar xv

aggregate
    :: (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m v
aggregate (Tree xv) = tAgg <$> MutVar.readMutVar xv

-- | For debugging/testing.
toList
    :: PrimMonad m => Tree (PrimState m) a v -> m [a]
toList = go []
  where
    go acc0 (Tree tv) = do
        T {..} <- MutVar.readMutVar tv
        acc1 <- if tRight == nil then return acc0 else go acc0 tRight
        let acc2 = tLabel : acc1
        if tLeft == nil then return acc2 else go acc2 tLeft

splay
    :: forall m a v. (PrimMonad m, Monoid v)
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)  -- Returns the old root.
splay xt@(Tree xv) = do
    -- Note (jaspervdj): Rather than repeatedly reading from/writing to xv we
    -- read x once and thread its (continuously updated) value through the
    -- entire stack of `go` calls.
    --
    -- The same is true for the left and right aggregates of x: they can be
    -- passed upwards rather than recomputed.
    x0 <- MutVar.readMutVar xv
    xla <- if tLeft x0 == nil then return mempty else tAgg <$> MutVar.readMutVar (unTree $ tLeft x0)
    xra <- if tRight x0 == nil then return mempty else tAgg <$> MutVar.readMutVar (unTree $ tRight x0)
    go xt xla xra x0
  where
    go  :: Tree (PrimState m) a v -> v -> v -> T (PrimState m) a v
        -> m (Tree (PrimState m) a v)
    go closestToRootFound xla xra !x = do
        let !(pt@(Tree pv)) = tParent x
        if pt == nil then do
            MutVar.writeMutVar xv x
            return closestToRootFound
        else do
            p <- MutVar.readMutVar pv
            let gt@(Tree gv) = tParent p
            let plt@(Tree plv) = tLeft p
            let prt@(Tree prv) = tRight p
            let xlt@(Tree xlv) = tLeft x
            let xrt@(Tree xrv) = tRight x
            if  | gt == nil, plt == xt -> do
                    -- ZIG (Right)
                    --
                    --    p  =>  x
                    --   /        \
                    --  x          p
                    --   \        /
                    --    xr     xr
                    --
                    when (xrt /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                        xr {tParent = pt}

                    pra <- if prt == nil then return mempty else tAgg <$> MutVar.readMutVar prv
                    MutVar.writeMutVar pv $! p
                        { tLeft   = xrt
                        , tParent = xt
                        , tAgg    = xra <> tValue p <> pra
                        }

                    MutVar.writeMutVar xv $! x
                        { tAgg    = tAgg p
                        , tRight  = pt
                        , tParent = nil
                        }

                    return pt

                | gt == nil -> do
                    -- ZIG (Left)
                    --
                    --  p    =>    x
                    --   \        /
                    --    x      p
                    --   /        \
                    --  xl         xl
                    --
                    when (xlt /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                        xl {tParent = pt}

                    pla <- if plt == nil then return mempty else tAgg <$> MutVar.readMutVar plv
                    MutVar.writeMutVar pv $! p
                        { tRight  = xlt
                        , tParent = xt
                        , tAgg    = pla <> tValue p <> xla
                        }

                    MutVar.writeMutVar xv $! x
                        { tAgg    = tAgg p
                        , tLeft   = pt
                        , tParent = nil
                        }

                    return pt

                | otherwise -> do

                    g <- MutVar.readMutVar gv
                    let ggt@(Tree ggv) = tParent g
                    let glt@(Tree glv) = tLeft g
                    let grt@(Tree grv) = tRight g
                    when (ggt /= nil) $ MutVar.modifyMutVar' ggv $ \gg ->
                        if tLeft gg == gt
                            then gg {tLeft = xt}
                            else gg {tRight = xt}

                    if  | plt == xt && glt == pt -> do
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

                            pra <- if prt == nil then return mempty else tAgg <$> MutVar.readMutVar prv
                            gra <- if grt == nil then return mempty else tAgg <$> MutVar.readMutVar grv
                            let !ga' = pra <> tValue g <> gra
                            when (prt /= nil) $ MutVar.modifyMutVar' prv $ \pr ->
                                pr {tParent = gt}

                            MutVar.writeMutVar gv $! g
                                { tParent = pt
                                , tLeft   = prt
                                , tAgg    = ga'
                                }

                            when (xrt /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = pt}

                            let !pa' = xra <> tValue p <> ga'
                            MutVar.writeMutVar pv $! p
                                { tParent = xt
                                , tLeft   = xrt
                                , tRight  = gt
                                , tAgg    = pa'
                                }

                            go gt xla pa' $! x
                                { tRight  = pt
                                , tAgg    = tAgg g
                                , tParent = ggt
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
                            pla <- if plt == nil then return mempty else tAgg <$> MutVar.readMutVar plv
                            gla <- if glt == nil then return mempty else tAgg <$> MutVar.readMutVar glv
                            let !ga' = gla <> tValue g <> pla
                            when (plt /= nil) $ MutVar.modifyMutVar' plv $ \pl ->
                                pl {tParent = gt}

                            MutVar.writeMutVar gv $! g
                                { tParent = pt
                                , tRight  = plt
                                , tAgg    = ga'
                                }

                            when (xlt /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = pt}

                            let !pa' = ga' <> tValue p <> xla
                            MutVar.writeMutVar pv $! p
                                { tParent = xt
                                , tLeft   = gt
                                , tRight  = xlt
                                , tAgg    = pa'
                                }

                            go gt pa' xra $! x
                                { tLeft   = pt
                                , tAgg    = tAgg g
                                , tParent = ggt
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
                            when (xlt /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = gt}

                            gla <- if glt == nil then return mempty else tAgg <$> MutVar.readMutVar glv
                            let !ga' = gla <> tValue g <> xla
                            MutVar.writeMutVar gv $! g
                                { tParent = xt
                                , tRight  = xlt
                                , tAgg    = ga'
                                }

                            when (xrt /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = pt}

                            pra <- if prt == nil then return mempty else tAgg <$> MutVar.readMutVar prv
                            let pa' = xra <> tValue p <> pra
                            MutVar.writeMutVar pv $! p
                                { tParent = xt
                                , tLeft   = xrt
                                , tAgg    = pa'
                                }

                            go gt ga' pa' $! x
                                { tParent = ggt
                                , tLeft   = gt
                                , tRight  = pt
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
                            when (xrt /= nil) $ MutVar.modifyMutVar' xrv $ \xr ->
                                xr {tParent = gt}

                            gra <- if grt == nil then return mempty else tAgg <$> MutVar.readMutVar grv
                            let !ga' = xra <> tValue g <> gra
                            MutVar.writeMutVar gv $! g
                                { tParent = xt
                                , tLeft   = xrt
                                , tAgg    = ga'
                                }

                            when (xlt /= nil) $ MutVar.modifyMutVar' xlv $ \xl ->
                                xl {tParent = pt}

                            pla <- if plt == nil then return mempty else tAgg <$> MutVar.readMutVar plv
                            let !pa' = pla <> tValue p <> xla
                            MutVar.writeMutVar pv $! p
                                { tParent = xt
                                , tRight  = xlt
                                , tAgg    = pa'
                                }

                            go gt pa' ga' $! x
                                { tParent = ggt
                                , tLeft   = pt
                                , tRight  = gt
                                , tAgg    = tAgg g
                                }

removeParent, removeLeft, removeRight
    :: PrimMonad m
    => Tree (PrimState m) a v -- Parent
    -> m ()
removeParent (Tree x) = MutVar.modifyMutVar' x $ \x' -> x' {tParent = nil}
removeLeft   (Tree x) = MutVar.modifyMutVar' x $ \x' -> x' {tLeft = nil}
removeRight  (Tree x) = MutVar.modifyMutVar' x $ \x' -> x' {tRight = nil}

-- | For debugging/testing.
freeze :: PrimMonad m => Tree (PrimState m) a v -> m (Tree.Tree a)
freeze (Tree tv) = do
    T {..} <- MutVar.readMutVar tv
    children  <- sequence $
        [freeze tLeft | tLeft /= nil] ++
        [freeze tRight | tRight /= nil]
    return $ Tree.Node tLabel children

print :: Show a => Tree (PrimState IO) a v -> IO ()
print = go 0
  where
    go d (Tree tv) = do
        T {..} <- MutVar.readMutVar tv
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
    computeAgg pt xt@(Tree xv) = do
        x' <- MutVar.readMutVar xv
        let p' = tParent x'
        when (pt /= p') $ error "broken parent pointer"

        let l = tLeft x'
        let r = tRight x'
        la <- if l == nil then return mempty else computeAgg xt l
        ra <- if r == nil then return mempty else computeAgg xt r

        let actualAgg = la <> (tValue x') <> ra
        let storedAgg = tAgg x'

        when (actualAgg /= storedAgg) $ error $
            "error in stored aggregates: " ++ show storedAgg ++
            ", actual: " ++ show actualAgg

        return actualAgg

data TreeGen s = TreeGen

instance Class.Tree Tree where
    type TreeGen Tree = TreeGen
    newTreeGen _ = return TreeGen

    singleton _ = singleton
    append      = append
    split       = split
    connected   = connected
    root        = root
    readRoot    = readRoot
    label       = label
    aggregate   = aggregate
    toList      = toList

instance Class.TestTree Tree where
    print            = print
    assertInvariants = assertInvariants
    assertSingleton  = \_ -> return ()
    assertRoot       = \_ -> return ()
