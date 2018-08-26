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
Module: Foreign.FAI.Platform.CUDA
Description: The CUDA platform instance.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The CUDA platform instance.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module Foreign.FAI.Platform.CUDA
  ( CUDA(..)
  , Pf
  , nullCUDAContext
  , nullCUDAContextIO
  ) where

import           Control.Monad
import           Foreign.C.Types
import           Foreign.FAI.Platform.Host (Host)
import           Foreign.FAI.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline         as C
import           System.IO.Unsafe

C.include "<cuda_runtime.h>"
C.include "<stdio.h>"

-- | CUDA backend (cudart required)
--
--  @cudaMallc@ is used to allocate memory from data.
--
--  @cudaFree@ is used to free pointer.
--
--  @cudaMemcpy@ is used to copy data between Host and CUDA.
data CUDA = CUDA

type instance Pf CUDA Float  = Float
type instance Pf CUDA Double = Double
type instance Pf CUDA Int    = Int
type instance Pf CUDA Word   = Word

cudaMemAllocate :: CInt -> IO (Ptr a)
cudaMemAllocate n =
  castPtr <$> [C.block| void* {
                  void * ptr = 0;
                  cudaError_t err = cudaMalloc((void*)&ptr, $(int n));
                  if (err != cudaSuccess) {
                    printf("Failed to allocate memory, %d", err);
                    ptr = 0;
                  }
                  return ptr;}|]

cudaMemRelease :: Ptr a -> IO ()
cudaMemRelease n' = [C.exp| void { cudaFree($(void *n)) }|]
  where n = castPtr n'

cudaMemReleaseP :: IO (FinalizerPtr a)
cudaMemReleaseP = castPtrToFunPtr <$> [C.exp| void* {*cudaFree}|]

cudaMemCopy :: (Ptr () -> Ptr () -> CInt -> IO CInt)
            -> ForeignPtr a -> ForeignPtr b -> CInt -> IO ()
cudaMemCopy doCopy fdst fsrc size =
  withForeignPtr fdst $ \dst' ->
    withForeignPtr fsrc $ \src' ->
  let dst = castPtr dst'
      src = castPtr src'
  in do
    rt <- doCopy dst src size
    when (rt /= 0) $ error "Fail to copy."

doCopyHC :: Ptr () -> Ptr () -> CInt -> IO CInt
doCopyHC dst src size =
  [C.block| int {
      cudaError_t err = cudaMemcpy($(void *dst), $(void *src),
                                    $(int size), cudaMemcpyHostToDevice);
      if(err != cudaSuccess) {
        printf("Failed to copy memory(HC), %d", err);
        return -1;
      }
      return 0;}|]

doCopyCH :: Ptr () -> Ptr () -> CInt -> IO CInt
doCopyCH dst src size =
  [C.block| int {
      cudaError_t err = cudaMemcpy($(void *dst), $(void *src),
                                    $(int size), cudaMemcpyDeviceToHost);
      if(err != cudaSuccess) {
        printf("Failed to copy memory(HC), %d", err);
        return -1;
      }
      return 0;}|]

doCopyCC :: Ptr () -> Ptr () -> CInt -> IO CInt
doCopyCC dst src size =
  [C.block| int {
      cudaError_t err = cudaMemcpy($(void *dst), $(void *src),
                                    $(int size), cudaMemcpyDeviceToDevice);
      if(err != cudaSuccess) {
        printf("Failed to copy memory(HC), %d", err);
        return -1;
      }
      return 0;}|]

instance FAI CUDA where
  faiMemAllocate    _ = cudaMemAllocate . fromIntegral
  faiMemRelease     _ = cudaMemRelease
  faiMemReleaseP    _ = Right <$> cudaMemReleaseP
  faiDefaultContextIO = nullCUDAContextIO

instance FAICopy Host CUDA where
  faiMemCopy dst src = do
    when (bufSize dst /= bufSize src) $ error "Different size."
    cudaMemCopy doCopyHC (bufPtr dst) (bufPtr src) $ fromIntegral $ bufByte dst

instance FAICopy CUDA Host where
  faiMemCopy dst src = do
    when (bufSize dst /= bufSize src) $ error "Different size."
    cudaMemCopy doCopyCH (bufPtr dst) (bufPtr src) $ fromIntegral $ bufByte dst

instance FAICopy CUDA CUDA where
  faiMemCopy dst src = do
    when (bufSize dst /= bufSize src) $ error "Different size."
    cudaMemCopy doCopyCC (bufPtr dst) (bufPtr src) $ fromIntegral $ bufByte dst

-- | Null pointer context of CUDA
nullCUDAContextIO :: IO (Context CUDA)
nullCUDAContextIO = Context <$> newForeignPtr_ nullPtr <*> return ignoreLogger

nullCUDAContext :: Context CUDA
nullCUDAContext = unsafePerformIO nullCUDAContextIO
