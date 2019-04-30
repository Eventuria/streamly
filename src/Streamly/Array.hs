{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "Streams/inline.hs"

-- |
-- Module      : Streamly.Array
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Arrays as implemented in this module are chunks of memory that can hold a
-- sequence of 'Storable' values of a given type. Unlike streams, arrays are
-- necessarily /finite/.  The size of an array is fixed at creation and cannot
-- be changed later.
--
-- Importantly, arrays use memory that is out of the ambit of GC and therefore
-- can hold arbitrary number of elements without adding any pressure to GC.
-- Moreover, they can be used to communicate with foreign consumers and
-- producers (e.g. file or network IO) without copying the data.

-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.  However, we can create arrays of arrays (trees) to scale to
-- arbitrarily large amounts of memory but still using small chunks of
-- contiguous memory.

module Streamly.Array
    (
      Array

    -- * Construction/Generation
    , nil
    , singleton
    , fromList
    , fromListN
    , fromStreamN

    -- * Elimination/Folds
    , foldl'
    -- , null
    , length
    , last

    , toList
    , toStream

    -- -- * IO
    -- , toHandle
    -- , fromHandleUptoN
    -- , fromHandleN
    --
    -- , fromHandlePosUptoN
    -- , fromHandlePosN

    -- -- * Transformation
    -- , map

    -- * Streams of Arrays
    -- , defaultChunkSize
    -- , fromHandleArraysUpto
    , concatArrays
    , fromHandleArrays
    -- Arrays of exactly N elements
    -- , fromHandleArraysOfN
    , toHandleArrays

    )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (Storable(..))
import System.IO (Handle, hGetBufSome, hPutBuf)
import Prelude hiding (length, null, last, map)

import GHC.Base (nullAddr#)
import GHC.ForeignPtr (ForeignPtr(..), mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Ptr (Ptr(..))

import Streamly.Streams.StreamK.Type (IsStream, mkStream)
import Streamly.Array.Types

import Streamly.Streams.Serial (SerialT)
import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
-- import qualified Streamly.Streams.StreamD.Type as D
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.Prelude as P

-------------------------------------------------------------------------------
-- Compacting streams of arrays
-------------------------------------------------------------------------------

{-
-- we can call these regroupXXX or reArrayXXX
--
-- Compact buffers in a stream such that each resulting buffer contains exactly
-- N elements.
compactN :: Int -> Int -> t m (Array a) -> t m (Array a)
compactN n vectors =

-- This can be useful if the input stream may "suspend" before generating
-- further output. So we can emit a vector early without waiting. It will emit
-- a vector of at least 1 element.
compactUpTo :: Int -> t m (Array a) -> t m (Array a)
compactUpTo hi vectors =

-- wait for minimum amount to be collected but don't wait for the upper limit
-- if the input stream suspends. But never go beyond the upper limit.
compactMinUpTo :: Int -> Int -> t m (Array a) -> t m (Array a)
compactMinUpTo lo hi vectors =

-- The buffer is emitted as soon as a complete marker sequence is detected. The
-- emitted buffer contains the sequence as suffix.
compactUpToMarker :: Array a -> t m (Array a) -> t m (Array a)
compactUpToMarker hi marker =

-- Buffer upto a max count or until timeout occurs. If timeout occurs without a
-- single element in the buffer it raises an exception.
compactUpToWithTimeout :: Int -> Int -> t m (Array a) -> t m (Array a)
compactUpToWithTimeout hi time =

-- Wait until min elements are collected irrespective of time. After collecting
-- minimum elements if timeout occurs return the buffer immediately else wait
-- upto timeout or max limit.
compactInRangeWithTimeout ::
    Int -> Int -> Int -> t m (Array a) -> t m (Array a)
compactInRangeWithTimeout lo hi time =

-- Compact the contiguous sequences into a single vector.
compactToReorder :: (a -> a -> Int) -> t m (Array a) -> t m (Array a)

-------------------------------------------------------------------------------
-- deCompact streams of arrays
-------------------------------------------------------------------------------

-- split buffers into smaller buffers
-- deCompactBuffers :: Int -> Int -> t m Buffer -> t m Buffer
-- deCompactBuffers maxSize tolerance =

-------------------------------------------------------------------------------
-- Scatter/Gather IO
-------------------------------------------------------------------------------

-- When each IO operation has a significant system overhead, it may be more
-- efficient to do gather IO. But when the buffers are too small we may want to
-- copy multiple of them in a single buffer rather than setting up a gather
-- list. In that case, a gather list may have more overhead compared to just
-- copying. If the buffer is larger than a limit we may just keep a single
-- buffer in a gather list.
--
-- gatherBuffers :: Int -> t m Buffer -> t m GatherBuffer
-- gatherBuffers maxLimit bufs =
--
-- Fold to IOVec and use writev when we do not know the size of the segment to
-- be written. For example, if we are buffering by lines and lines may be of
-- indefinite size.  Or if we are serializing a type that has big size and we
-- want to allocate memory in smaller chunks.
-}

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- XXX Use stream and toArray to create a vector.

-- Represent a null pointer for an empty vector
nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

-- | An empty array.
--
-- @
-- > toList nil
-- []
-- @
--
-- @since 0.7.0
{-# INLINE nil #-}
nil :: Storable a => Array a
nil = Array
    { aStart = nullForeignPtr
    , aEnd = Ptr nullAddr#
    , aBound = Ptr nullAddr#
    }

-- XXX we can make a special case for a singleton array, we can keep the
-- element in the constructor without allocating memory.
{-# INLINE singleton #-}
singleton :: forall a. Storable a => a -> Array a
singleton a =
    -- XXX use unsafePerformIO instead?
    let !arr = unsafeDupablePerformIO $ withNewArray 1 $ \p -> poke p a
    in (arr {aEnd = aEnd arr `plusPtr` (sizeOf (undefined :: a))})

-- | Create an Array of a given size from a stream.
{-# INLINE fromStreamN #-}
fromStreamN :: (Monad m, Storable a) => Int -> SerialT m a -> m (Array a)
fromStreamN n = FL.foldl (FL.toArrayN n)

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINE fromHandleUpto #-}
fromHandleUpto :: Int -> Handle -> IO ByteArray
fromHandleUpto size h = do
    ptr <- mallocPlainForeignPtrBytes size
    withForeignPtr ptr $ \p -> do
        n <- hGetBufSome h p size
        let v = Array
                { aStart = ptr
                , aEnd   = p `plusPtr` n
                , aBound = p `plusPtr` size
                }
        -- XXX shrink only if the diff is significant
        shrinkToFit v

-- | @fromHandleArraysUpto size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @size@.
-- 'fromHandleArraysUpto' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINE fromHandleArraysUpto #-}
fromHandleArraysUpto :: (IsStream t, MonadIO m)
    => Int -> Handle -> t m (Array Word8)
fromHandleArraysUpto size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld sng _ -> do
        vec <- liftIO $ fromHandleUpto size h
        if length vec < size
        then sng vec
        else yld vec go

-- | @fromHandleArrays h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @defaultChunkSize@.
-- 'fromHandleArrays' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINE fromHandleArrays #-}
fromHandleArrays :: (IsStream t, MonadIO m) => Handle -> t m (Array Word8)
fromHandleArrays = fromHandleArraysUpto defaultChunkSize

-------------------------------------------------------------------------------
-- Cast arrays from one type to another
-------------------------------------------------------------------------------

-- XXX The first array must be an exact multiple of (sizeOf b).
-- Can be useful to manipulate larger size elements more efficiently, e.g.
-- copying Word64 instead of Word8.
-- castArray :: (Storable a, Storable b) => Array a -> Array b
-- castArray Array{..} =

-- split an array to remove the unaligned part at the end into a separate
-- array. Useful to copy the aligned portion more efficiently.
--
-- splitUnaligned :: Int -> Array a -> (Array a, Array a)

-- Like concatArray but while concating casts the array from one type to
-- another. Useful to combine array chunks into arrays that can be manipulated
-- more efficiently.
--
-- concatCastArray

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- XXX use stream map to implement this.
{-# INLINE map #-}
map :: forall a b. (Storable a, Storable b) => (a -> b) -> Array a -> Array b
map f srcArr = runIdentity $
    fromStreamN (length srcArr) $ S.map f (toStream srcArr)
{-
map f srcArr =
    -- XXX use unsafePerformIO instead?
    let !r = unsafeDangerousPerformIO mapIO in r

    where

    mapIO = withForeignPtr (aStart srcArr) $ \srcPtr -> do
                dstArr <- unsafeNew $ (length srcArr) * sizeOf (undefined :: b)
                withForeignPtr (aStart dstArr) $ \dstPtr -> do
                    go srcPtr dstPtr
                    return dstArr

    go !src !dst
        | src == (aEnd srcArr) = return ()
        | otherwise = do
            x <- peek src
            poke dst (f x)
            go (src `plusPtr` sizeOf (undefined :: a))
               (dst `plusPtr` sizeOf (undefined :: b))
-}

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- null = (== 0) . length
{-# INLINE null #-}
null :: Array a -> Bool
null Array{..} =
    let start = unsafeForeignPtrToPtr aStart
    in assert (aEnd >= start) $ aEnd <= start

{-# INLINE last #-}
last :: forall a. Storable a => Array a -> Maybe a
last arr@Array{..} =
    if null arr
    then Nothing
    else Just $!
        let p = aEnd `plusPtr` negate (sizeOf (undefined :: a))
        in unsafeDangerousPerformIO $ do
            x <- peek p
            touchForeignPtr aStart
            return x

-------------------------------------------------------------------------------
-- Elimination/folding
-------------------------------------------------------------------------------

-- | Write a stream to a file handle
{-# INLINE toHandle #-}
toHandle :: Storable a => Handle -> Array a -> IO ()
toHandle _ v | null v = return ()
toHandle h v@Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

{-# INLINABLE toStream #-}
toStream :: (Monad m, IsStream t, Storable a) => Array a -> t m a
toStream = P.fromArray

-------------------------------------------------------------------------------
-- Streams of Arrays
-------------------------------------------------------------------------------

-- | Convert a stream of Arrays into a stream of elements.
{-# INLINE concatArrays #-}
concatArrays :: (IsStream t, Monad m, Storable a) => t m (Array a) -> t m a
concatArrays m = D.fromStreamD $ D.concatArray (D.toStreamD m)

-- XXX we should use overWrite/write
-- | Write a stream of arrays to a handle.
{-# INLINE toHandleArrays #-}
toHandleArrays :: (MonadIO m, Storable a) => Handle -> SerialT m (Array a) -> m ()
toHandleArrays h m = S.mapM_ (liftIO . toHandle h) m
