{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Data.MTree.Splay
    ( Tree

    , singleton
    , fromNonEmpty
    , append

    , freeze
    , freezeValues
    , freezeAndPrint
    ) where

import           Control.Monad           (when)
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Primitive.MutVar   (MutVar)
import qualified Data.Primitive.MutVar   as MutVar
import qualified Data.Tree               as Tree

-- TODO (jaspervdj): `Tree s v a` is more idiomatic probably
data Tree s a v = Tree
    { tParent    :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tLeft      :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tRight     :: {-# UNPACK #-} !(MutVar s (Tree s a v))
    , tValue     :: !a
    , tAggregate :: {-# UNPACK #-} !(MutVar s v)
    }

instance Eq (Tree s a v) where
    -- Reference equality through a MutVar.
    t1 == t2 = tParent t1 == tParent t2

singleton :: PrimMonad m => a -> v -> m (Tree (PrimState m) a v)
singleton tValue aggregate = do
    tParent    <- MutVar.newMutVar undefined
    tLeft      <- MutVar.newMutVar undefined
    tRight     <- MutVar.newMutVar undefined
    tAggregate <- MutVar.newMutVar aggregate
    let tree = Tree {..}
    MutVar.writeMutVar tParent tree
    MutVar.writeMutVar tLeft   tree
    MutVar.writeMutVar tRight  tree
    return tree

fromNonEmpty
    :: PrimMonad m => NonEmpty.NonEmpty (a, v) -> m (Tree (PrimState m) a v)
fromNonEmpty ((x0, v0) NonEmpty.:| list) = do
    root <- singleton x0 v0
    go root list
    return root
  where
    go rightMost []            = return ()
    go rightMost ((x, v) : xs) = do
        tree <- singleton x v
        setRight rightMost tree
        go tree xs

root :: PrimMonad m => Tree (PrimState m) a v -> m (Tree (PrimState m) a v)
root tree@Tree {..} = do
    parent <- MutVar.readMutVar tParent
    if parent == tree then return tree else root parent

-- | Appends two trees.  Returns the root of the tree.
append
    :: PrimMonad m
    => Tree (PrimState m) a v
    -> Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
append x y = do
    rm <- getRightMost x
    splay rm
    setRight rm y
    return rm

splay
    :: PrimMonad m
    => Tree (PrimState m) a v
    -> m ()
splay x = do
    p <- MutVar.readMutVar (tParent x)
    when (p /= x) $ do
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
        splay x

getRightMost
    :: PrimMonad m
    => Tree (PrimState m) a v
    -> m (Tree (PrimState m) a v)
getRightMost t@Tree {..} = do
    tr <- MutVar.readMutVar tRight
    if t == tr then return t else getRightMost tr

rotateLeft' p = do
    r <- MutVar.readMutVar (tRight p)
    rotateLeft p r
    return r

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
    when (b /= x) $ setLeft p b
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

-- | For debugging/testing.
freeze :: PrimMonad m => Tree (PrimState m) a v -> m (Tree.Tree (a, v))
freeze tree@Tree {..} = do
    left      <- MutVar.readMutVar tLeft
    right     <- MutVar.readMutVar tRight
    aggregate <- MutVar.readMutVar tAggregate
    children  <- sequence $
        [freeze left  | left /= tree] ++
        [freeze right | right /= tree]
    return $ Tree.Node (tValue, aggregate) children

-- | For debugging/testing.
freezeValues
    :: PrimMonad m => Tree (PrimState m) a v -> m [a]
freezeValues tree@Tree {..} = do
    left   <- MutVar.readMutVar tLeft
    right  <- MutVar.readMutVar tRight
    leftc  <- if left  == tree then return [] else freezeValues left
    rightc <- if right == tree then return [] else freezeValues right
    return $ leftc ++ [tValue] ++ rightc

freezeAndPrint :: (Show a, Show v) => Tree (PrimState IO) a v -> IO ()
freezeAndPrint tree = do
    frozen <- freeze tree
    putStrLn $ Tree.drawTree $ fmap show frozen
