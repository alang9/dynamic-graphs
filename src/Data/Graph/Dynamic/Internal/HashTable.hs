-- | This is a very simple wrapper around the 'hashtables' library that uses
-- 'PrimMonad' rather than 'ST'.
module Data.Graph.Dynamic.Internal.HashTable
    ( HashTable
    , new
    , insert
    , delete
    , lookup

    -- Slow and debugging only
    , toList
    ) where

import           Control.Monad.Primitive  (PrimMonad (..), stToPrim)
import           Data.Hashable            (Hashable)
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import           Prelude                  hiding (lookup)

type HashTable s k v = Cuckoo.HashTable s k v

new :: PrimMonad m => m (HashTable (PrimState m) k v)
new = stToPrim Cuckoo.new
{-# INLINE new #-}

insert
    :: (Eq k, Hashable k, PrimMonad m)
    => HashTable (PrimState m) k v -> k -> v -> m ()
insert ht k v = stToPrim (Cuckoo.insert ht k v)
{-# INLINE insert #-}

delete
    :: (Eq k, Hashable k, PrimMonad m)
    => HashTable (PrimState m) k v -> k -> m ()
delete ht k = stToPrim (Cuckoo.delete ht k)
{-# INLINE delete #-}

lookup
    :: (Eq k, Hashable k, PrimMonad m)
    => HashTable (PrimState m) k v -> k -> m (Maybe v)
lookup ht k = stToPrim (Cuckoo.lookup ht k)
{-# INLINE lookup #-}

--------------------------------------------------------------------------------

-- | Slow, only for debugging and testing.
toList
    :: PrimMonad m
    => HashTable (PrimState m) k v -> m [(k, v)]
toList ht = stToPrim $ Cuckoo.foldM (\acc kv -> return (kv : acc)) [] ht
