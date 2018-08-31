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
import           Foreign.Storable
import           System.IO.Unsafe

import qualified Language.C.Inline               as C

C.include "<cuda_runtime.h>"
C.include "<stdio.h>"



-- | Copy the data from pointer to Haskell list.
peekCUDABuffer :: ( Storable c, Pf Host a ~ c, Pf CUDA a ~ c
                  , Shape sh, Buffer b
                  , BufferPlatform b ~ CUDA
                  , BufferShape    b ~ sh
                  , BufferType     b ~ a)
               => b   -- ^ Buffer
               -> IO [c]          -- ^ Haskell list
peekCUDABuffer bf = do
  hostBuf <- mallocForeginPtrArray $ bufSize bf
  withBuffer buf $ \src' -> withForeignPtr hostBuf $ \dst' -> do
    let dst = castPtr dst'
        src = castPtr src'
        size = fromIntegral $ bufByte bf
    _ <- [C.block| int {
        cudaError_t err = cudaMemcpy($(void *dst), $(void *src),
                                    $(int size), cudaMemcpyDeviceToHost);
        if(err != cudaSuccess) {
            printf("Failed to copy memory(HC), %d", err);
            return -1;
        }
        return 0;
    }
    |]
    peekArray dst'
  where lsLen = length ls
        bfLen = bufSize buf
        len   = min bfLen lsLen
        size' :: Storable a => Ptr a -> a -> Int
        size' _ x = sizeOf x * len

-- | Copy the data from Haskell list into pointer.
pokeCUDABuffer :: ( Storable c, Pf Host a ~ c, Pf CUDA a ~ c
                  , Shape sh, Buffer b
                  , BufferPlatform b ~ CUDA
                  , BufferShape    b ~ sh
                  , BufferType     b ~ a)
               => b -- ^ CUDA buffer
               -> [c]           -- ^ list
               -> IO ()
pokeCUDABuffer buf ls = do
  hostBuf <- mallocForeginPtrArray lsLen
  withBuffer buf $ \dst' -> withForeignPtr hostBuf $ \src' -> do
    pokeArray src' ls
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
        bfLen = bufSize buf
        len   = min bfLen lsLen
        size' :: Storable a => Ptr a -> a -> Int
        size' _ x = sizeOf x * len

-- | Transform list to CUDA buffer.
toCUDABuffer :: ( Storable c, Pf Host a ~ c, Pf CUDA a ~ c
                , Buffer b
                , BufferPlatform b ~ CUDA
                , BufferShape    b ~ Int
                , BufferType     b ~ a)
             => [c]                 -- ^ List
             -> IO b  -- ^ CUDA buffer
toCUDABuffer ls = do
  bf <- fst <$> newBufferIO (length ls) nullCUDAContext
  pokeCUDABuffer bf ls
  return bf

-- | Unsafe peek
unsafePeekCUDABuffer :: ( Storable c, Pf Host a ~ c, Pf CUDA a ~ c
                        , Shape sh, Buffer b
                        , BufferPlatform b ~ CUDA
                        , BufferShape    b ~ sh
                        , BufferType     b ~ a)
                     => b
                     -> [c]
unsafePeekCUDABuffer = unsafePerformIO . peekCUDABuffer

-- | Unsafe poke
unsafeToCUDABuffer :: ( Storable c, Pf Host a ~ c, Pf CUDA a ~ c
                      , Buffer b
                      , BufferPlatform b ~ CUDA
                      , BufferShape    b ~ Int
                      , BufferType     b ~ a)
                   => [c]
                   -> b
unsafeToCUDABuffer = unsafePerformIO . toCUDABuffer

