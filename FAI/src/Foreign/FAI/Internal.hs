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
Module: Foreign.FAI.Internal
Description: The internal functions.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The internal functions.
-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Foreign.FAI.Internal
  ( autoNewForeignPtr
  , dup
  , replaceContext
  ) where

import           Control.Monad
import           Foreign.FAI.Types
import           Foreign.ForeignPtr (newForeignPtr, newForeignPtrEnv,
                                     withForeignPtr)
import           Foreign.Ptr        (Ptr)
import           Foreign.Storable   (Storable)

-- | create foreign ptr from pointer for a new Buffer
autoNewForeignPtr :: ( ContextPointer p, Buffer b
                     , BufferShape    b ~ sh
                     , BufferPlatform b ~ p
                     , BufferType     b ~ a
                     )
                  => FinalizerContextPtr p (Pf p a) -- ^ Context p concerned finalizer
                  -> p                              -- ^ Context
                  -> Ptr (Pf p a)                   -- ^ pointer
                  -> sh                             -- ^ Shape
                  -> IO b                           -- ^ buffer
autoNewForeignPtr fin cc ptr sh = fmap (`makeBuffer` sh) $ case fin of
  Left  f -> withForeignPtr (getContextPointer cc) $ \p ->
             newForeignPtrEnv f p ptr
  Right f -> newForeignPtr    f   ptr

replaceContext :: p2 -> (a, p1) -> (a, p2)
replaceContext cc (a, _) = (a, cc)

-- | Duplicate data
dup :: ( FAICopy p1 p2
       , Storable c, Pf p2 a ~ c, Pf p1 a ~ c
       , Buffer b1, Buffer b2
       , BufferPlatform b1 ~ p1
       , BufferPlatform b2 ~ p2
       , BufferType     b1 ~ a
       , BufferType     b2 ~ a
       , BufferShape    b1 ~ sh
       , BufferShape    b2 ~ sh
       , Shape sh)
       => p2           -- ^ context
       -> Bool         -- ^ whether copy data
       -> b1           -- ^ buffer (src)
       -> IO (b2, p2)  -- ^ buffer (dst)
dup cc is buf = do
  fin  <- faiMemReleaseP cc
  ptr  <- faiMemAllocate cc $ bufByte buf
  bDst <- autoNewForeignPtr fin cc ptr sh
  when is $ faiMemCopy bDst buf
  return (bDst, cc)
  where sh = getBufferShape buf
