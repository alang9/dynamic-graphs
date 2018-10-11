{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Data.Graph.Dynamic.Internal.Tree
    ( Vertex (..)
    , Edge (..)
    , edgeSize

    , Tree (..)
    , concat

    , TestTree (..)
    ) where

import           Control.Monad           (foldM)
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.Hashable           (Hashable)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Proxy              (Proxy)
import           GHC.Generics            (Generic)
import           Prelude                 hiding (concat)

newtype Vertex = Vertex {unVertex :: Int}
    deriving (Eq, Hashable, Ord, Show)

data Edge = Edge {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Generic, Ord, Show)

instance Hashable Edge

edgeSize :: Edge -> Int
edgeSize (Edge x y) = if x == y then 1 else 0
{-# INLINE edgeSize #-}

-- | The chosen represenation of the tree has a big impact on the performance of
-- the algorithms.  This typeclass allows us to swap them out more easily.
class Tree (t :: * -> *) where
    -- | A management structure used to create new trees
    type TreeGen t :: * -> *

    -- | Create a tree gen itself
    newTreeGen
        :: PrimMonad m => Proxy t -> m (TreeGen t (PrimState m))

    -- | Create a tree with a single element.
    singleton
        :: (PrimMonad m)
        => TreeGen t (PrimState m) -> Edge -> m (t (PrimState m))

    -- | Join two trees together.
    append
        :: (PrimMonad m)
        => t (PrimState m) -> t (PrimState m)
        -> m (t (PrimState m))

    -- | Prepend a singleton tree
    cons
        :: (PrimMonad m)
        => t (PrimState m) -> t (PrimState m)
        -> m (t (PrimState m))
    cons = append

    -- | Append a singleton tree
    snoc
        :: (PrimMonad m)
        => t (PrimState m) -> t (PrimState m)
        -> m (t (PrimState m))
    snoc = append

    -- | Split a tree, turning the argument into a singleton and returning the
    -- left and right halves of the tree.
    split
        :: (PrimMonad m)
        => t (PrimState m)
        -> m (Maybe (t (PrimState m)), Maybe (t (PrimState m)))

    -- | Check if two nodes belong to the same tree
    connected
        :: (PrimMonad m)
        => t (PrimState m) -> t (PrimState m)
        -> m Bool

    -- | Find the root of a tree
    root
        :: (PrimMonad m)
        => t (PrimState m)
        -> m (t (PrimState m))

    -- | Read the aggregate of a tree
    aggregate
        :: (PrimMonad m)
        => t (PrimState m)
        -> m Int

    -- | Convert a tree to a list
    toList
        :: (PrimMonad m)
        => t (PrimState m)
        -> m [Edge]

concat
    :: forall t m. (Tree t, PrimMonad m)
    => NonEmpty (t (PrimState m))
    -> m (t (PrimState m))
concat trees0 =
    case trees0 of x NonEmpty.:| xs -> foldM append x xs

-- | These methods can be used for testing and debugging.
class Tree t => TestTree t where
    print
        :: t (PrimState IO) -> IO ()

    assertInvariants
        :: (PrimMonad m)
        => t (PrimState m) -> m ()

    assertSingleton
        :: (PrimMonad m)
        => t (PrimState m) -> m ()

    assertRoot
        :: (PrimMonad m)
        => t (PrimState m) -> m ()
