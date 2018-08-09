{-

Copyright (C) 2018 Johann Lee <mer@qinka.pro>

This fiel is part of HaskellFAI

HaskellFAI is free software: you can redistribute it and/or modify
it under the terms of the GNU Less General Public License as published by
the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.

Haskell is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warrenty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Less General Public License for more details.

You should have received a copy of the GNU Less General Public License
along with HaskellFAI. If not, see <http://www.gnu.org/licenses/>.

-}

{-|
Module: Foreign.FAI.Platform.CUDA.Debug
Description: The debug component for CUDA.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The debug component for CUDA.
-}

{-# LANGUAGE GADTs           #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Foreign.FAI.Platform.CUDA.Debug
  ( peekCUDABuffer
  , pokeCUDABuffer
  , toCUDABuffer
  , unsafePeekCUDABuffer
  , unsafeToCUDABuffer
  ) where

import           Foreign.C.Types
import           Foreign.FAI
import           Foreign.FAI.Internal
import           Foreign.FAI.Platform.CUDA
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Platform.Host.Debug
import           Foreign.FAI.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.IO.Unsafe

import qualified Language.C.Inline               as C

C.include "<cuda_runtime.h>"
C.include "<stdio.h>"

-- | Copy the data from pointer to Haskell list.
peekCUDABuffer :: (Storable b, Pf CUDA a ~ b, Shape sh, Pf Host a ~ b)
               => Buffer sh CUDA a   -- ^ Buffer
               -> IO [b]          -- ^ Haskell list
peekCUDABuffer bf =
  peekHostBuffer =<< fst <$> dup nullHostContext True bf


-- | Copy the data from Haskell list into pointer.
pokeCUDABuffer :: (Storable b, Pf CUDA a ~ b, Shape sh, Pf Host a ~ b)
               => Buffer sh CUDA a -- ^ CUDA buffer
               -> [b]           -- ^ list
               -> IO ()
pokeCUDABuffer bf ls = do
  hostBuf <- fst <$> dup nullHostContext False bf
  pokeHostBuffer hostBuf ls
  withForeignPtr (bufPtr bf) $ \dst' -> withBuffer hostBuf $ \src' -> do
    let dst = castPtr dst'
        src = castPtr src'
        size = fromIntegral $ size' dst' undefined
    _ <- [C.block| int {
        cudaError_t err = cudaMemcpy($(void *dst), $(void *src),
                                    $(int size), cudaMemcpyHostToDevice);
        if(err != cudaSuccess) {
            printf("Failed to copy memory(HC), %d", err);
            return -1;
        }
        return 0;
    }
    |]
    return ()
  where lsLen = length ls
        bfLen = bufSize bf
        len   = min bfLen lsLen
        size' :: Storable a => Ptr a -> a -> Int
        size' _ x = sizeOf x * len

-- | Transform list to CUDA buffer.
toCUDABuffer :: (Storable b, Pf CUDA a ~ b, Pf Host a ~ b)
             => [b]                 -- ^ List
             -> IO (Buffer Int CUDA a)  -- ^ CUDA buffer
toCUDABuffer ls = do
  bf <- fst <$> newBufferIO (length ls) nullCUDAContext
  pokeCUDABuffer bf ls
  return bf

-- | Unsafe peek
unsafePeekCUDABuffer :: (Storable b,Pf CUDA a ~ b, Shape sh, Pf Host a ~ b)
                     => Buffer sh CUDA a
                     -> [b]
unsafePeekCUDABuffer = unsafePerformIO . peekCUDABuffer

-- | Unsafe poke
unsafeToCUDABuffer :: (Storable b, Pf CUDA a ~ b, Pf Host a ~ b)
                   => [b]
                   -> Buffer Int CUDA a
unsafeToCUDABuffer = unsafePerformIO . toCUDABuffer

