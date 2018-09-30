{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Graph.Dynamic.Internal.BitVector
    ( BitVector
    , new
    , length
    , read
    , write
    , xor
    ) where

import           Control.Monad.Primitive  (PrimMonad (..))
import qualified Data.Bits                as Bits
import qualified Data.Primitive.ByteArray as Arr
import qualified Data.Primitive.MutVar    as MutVar
import           Data.Word                (Word8)
import           Prelude                  hiding (length, read, zipWith)

data BitVector s = BitVector
    { bvSize :: {-# UNPACK #-} !(MutVar.MutVar s Int)
    , bvData :: {-# UNPACK #-} !(Arr.MutableByteArray s)
    }

new :: PrimMonad m => Int -> m (BitVector (PrimState m))
new s = do
    bvSize <- MutVar.newMutVar s
    bvData <- Arr.newByteArray ((s + 7) `div` 8)
    return BitVector {..}

length :: PrimMonad m => BitVector (PrimState m) -> m Int
length = MutVar.readMutVar . bvSize

read :: PrimMonad m => BitVector (PrimState m) -> Int -> m Bool
read bv !i = do
    let !ibyte = i `Bits.shiftR` 3
        !ibit  = i - (ibyte `Bits.shiftL` 3)
    byte <- Arr.readByteArray (bvData bv) ibyte
    return $! Bits.testBit (byte :: Word8) ibit

write :: PrimMonad m => BitVector (PrimState m) -> Int -> Bool -> m ()
write bv !i !val = do
    let !ibyte = i `Bits.shiftR` 3
        !ibit  = i - (ibyte `Bits.shiftL` 3)
    byte <- Arr.readByteArray (bvData bv) ibyte
    Arr.writeByteArray (bvData bv) ibyte $ case val of
        True  -> Bits.setBit (byte :: Word8) ibit
        False -> Bits.clearBit byte ibit

-- | XOR's the first bitvector with the second one.  The first argument is
-- modified.
xor :: PrimMonad m => BitVector (PrimState m) -> BitVector (PrimState m) -> m ()
xor = zipWith Bits.xor

zipWith
    :: PrimMonad m
    => (Word8 -> Word8 -> Word8)
    -> BitVector (PrimState m) -> BitVector (PrimState m) -> m ()
zipWith f l r = do
    lsize <- MutVar.readMutVar (bvSize l)
    rsize <- MutVar.readMutVar (bvSize r)
    go 0 (min lsize rsize `Bits.shiftL` 3)
  where
    go i n
        | i >= n    = return ()
        | otherwise = do
            x <- Arr.readByteArray (bvData l) i
            y <- Arr.readByteArray (bvData r) i
            Arr.writeByteArray (bvData l) i $ f x y
            go (i + 1) n
