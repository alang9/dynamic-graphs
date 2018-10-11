{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.Graph.Dynamic.Internal.Tree
    ( Tree (..)
    , concat

    , TestTree (..)
    ) where

import           Control.Monad           (foldM)
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Proxy              (Proxy)
import           Prelude                 hiding (concat)

-- | The chosen represenation of the tree has a big impact on the performance of
-- the algorithms.  This typeclass allows us to swap them out more easily.
class Tree (t :: * -> * -> * -> *) where
    -- | A management structure used to create new trees
    type TreeGen t :: * -> *

    -- | Create a tree gen itself
    newTreeGen
        :: PrimMonad m => Proxy t -> m (TreeGen t (PrimState m))

    -- | Create a tree with a single element.
    singleton
        :: (PrimMonad m, Monoid v)
        => TreeGen t (PrimState m) -> a -> v -> m (t (PrimState m) a v)

    -- | Join two trees together.
    append
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v -> t (PrimState m) a v
        -> m (t (PrimState m) a v)

    -- | Prepend a singleton tree
    cons
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v -> t (PrimState m) a v
        -> m (t (PrimState m) a v)
    cons = append

    -- | Append a singleton tree
    snoc
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v -> t (PrimState m) a v
        -> m (t (PrimState m) a v)
    snoc = append

    -- | Split a tree, turning the argument into a singleton and returning the
    -- left and right halves of the tree.
    split
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v
        -> m (Maybe (t (PrimState m) a v), Maybe (t (PrimState m) a v))

    -- | Check if two nodes belong to the same tree
    connected
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v -> t (PrimState m) a v
        -> m Bool

    -- | Find the root of a tree
    root
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v
        -> m (t (PrimState m) a v)

    -- | Read the aggregate of a tree
    aggregate
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v
        -> m v

    -- | Convert a tree to a list
    toList
        :: (PrimMonad m, Monoid v)
        => t (PrimState m) a v
        -> m [a]

concat
    :: forall t m a v. (Tree t, PrimMonad m, Monoid v)
    => NonEmpty (t (PrimState m) a v)
    -> m (t (PrimState m) a v)
concat trees0 =
    case trees0 of x NonEmpty.:| xs -> foldM append x xs

-- | These methods can be used for testing and debugging.
class Tree t => TestTree t where
    print
        :: Show a
        => t (PrimState IO) a v -> IO ()

    assertInvariants
        :: (PrimMonad m, Monoid v, Eq v, Show v)
        => t (PrimState m) a v -> m ()

    assertSingleton
        :: (PrimMonad m, Monoid v, Eq v, Show v)
        => t (PrimState m) a v -> m ()

    assertRoot
        :: (PrimMonad m, Monoid v, Eq v, Show v)
        => t (PrimState m) a v -> m ()
