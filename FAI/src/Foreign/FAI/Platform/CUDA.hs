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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Foreign.FAI.Platform.CUDA
  ( CUDA(..)
  , Pf(..)
  ) where

import qualified Language.C.Inline as C
import Foreign.FAI.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad

C.include "<cuda_runtime.h>"
C.include "<stdio.h>"

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

instance FAI CUDA where
  faiMemAllocate n = liftIO $ cudaMemAllocate $ fromIntegral n
  faiMemRelease  p = liftIO $ cudaMemRelease p
  faiMemReleaseP   = liftIO   cudaMemReleaseP

