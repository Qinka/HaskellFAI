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

import           Foreign.FAI
import           Foreign.FAI.Platform.CUDA
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import qualified Language.C.Inline         as C
import           System.IO.Unsafe

C.include "<cuda_runtime.h>"
C.include "<stdio.h>"

-- | Copy the data from pointer to Haskell list.
peekCUDABuffer :: (Storable b, Pf CUDA a ~ b, Shape sh)
               => Buffer sh CUDA a   -- ^ Buffer
               -> IO [b]          -- ^ Haskell list
peekCUDABuffer bf = do
  hostBuf <- fst <$> newBufferIO nullHostContext $ bufShape bf
  let size = bufByte bf
  withForeignPtr (bufPtr bf) $ \src -> withBuffer hostBuf $ \dst -> do
    [C.block| void {
        cudaError_t err = cudaMemcpy($(void *dst), $(void *src),
                                    $(int size), cudaMemcpyDeviceToHost);
        if(err != cudaSuccess) {
            printf("Failed to copy memory(HC), %d", err);
            return -1;
        }
        return 0;
    }
    |]
    peekArray (bufSize bf) dst

-- | Copy the data from Haskell list into pointer.
pokeCUDABuffer :: (Storable b, Pf CUDA a ~ b, Shape sh)
               => Buffer sh CUDA a -- ^ CUDA buffer
               -> [b]           -- ^ list
               -> IO ()
pokeCUDABuffer (Buffer fp s) ls = do
  hostBuf <- fst <$> newBufferIO (bufShape bf) nullHostContext
  withForeignPtr (bufPtr bf) $ \dst -> withBuffer hostBuf $ \src -> do
    pokeArray src $ take len ls
    [C.block| void {
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
        bfLen = shLen s
        len   = min bfLen lsLen

-- | Transform list to CUDA buffer.
toCUDABuffer :: (Storable b, Pf CUDA a ~ b)
             => [b]                 -- ^ List
             -> IO (Buffer Int CUDA a)  -- ^ CUDA buffer
toCUDABuffer ls = do
  bf <- fst <$> newBufferIO (length ls) nullCUDAContext
  pokeCUDABuffer bf ls
  return bf
  where cc :: Context CUDA
        cc = Context undefined
-- | Unsafe peek
unsafePeekCUDABuffer :: (Storable b,Pf CUDA a ~ b, Shape sh)
                     => Buffer sh CUDA a
                     -> [b]
unsafePeekCUDABuffer = unsafePerformIO . peekCUDABuffer

-- | Unsafe poke
unsafeToCUDABuffer :: (Storable b, Pf CUDA a ~ b)
                   => [b]
                   -> Buffer Int CUDA a
unsafeToCUDABuffer = unsafePerformIO . toCUDABuffer

