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
Module: Foreign.FAI
Description: The Haskell Foreign Accelerate Interace.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The Haskell Foreign Accelerate Interace.
-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Foreign.FAI
  ( Pf
  , Buffer(..)
  , Shape(..)
  , Context(..)
  , Accelerate(..)
  , FAI(..)
  , FAICopy(..)
  , FinalizerContextPtr
  , Z(..)
  , (:.)(..)
  , DIM0
  , DIM1
  , DIM2
  , DIM3
  , DIM4
  , DIM5
  , accelerate
  , newBuffer
  , newBufferIO
  , dupBuffer
  , dupBufferIO
  , dupBufferD
  , liftIO
  , reshape
  , bufSize
  , withBuffer
  , bufByte
  ) where

import           Control.Monad
import           Foreign.FAI.Internal
import           Foreign.FAI.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr

-- | run the @Accelerate@.
accelerate :: Context p -> Accelerate p a -> IO a
accelerate cc = (fst <$>) . flip doAccelerate cc

-- | Allocate new buffer (IO)
newBufferIO :: (FAI p, Storable b, (Pf p a) ~ b, Shape sh)
            => sh                          -- ^ shape
            -> Context p                   -- ^ platform context
            -> IO (Buffer sh p a, Context p)  -- ^ Buffer and (new) context
newBufferIO sh cc =  do
  fin <- faiMemReleaseP cc
  ptr <- alloc cc undefined
  when (nullPtr == ptr) $ error "Can not allocate memory."
  buf <- autoNewForeignPtr fin cc ptr sh
  return (buf, cc)
  where alloc :: (FAI p, Storable b) => Context p ->  b -> IO (Ptr b)
        alloc c' u =
          faiMemAllocate c' $ shLen sh * sizeOf u

-- | Allocate buffer
newBuffer :: (FAI p, Storable b, (Pf p a) ~ b, Shape sh)
          => sh                            -- ^ shape
          -> Accelerate p (Buffer sh p a)  -- ^ buffer
newBuffer = Accelerate . newBufferIO

-- | Duplicate buffer (IO)
dupBufferIO :: ( FAICopy p1 p2, FAI p1, FAI p2
               , Storable b, Pf p2 a ~ b, Pf p1 a ~ b
               , Shape sh)
            => Bool                           -- ^ Whether copy data
            -> Buffer sh p1 a                    -- ^ buffer (src)
            -> Context p2                     -- ^ platform context
            -> IO (Buffer sh p2 a, Context p2)   -- ^ buffer (dst) and context
dupBufferIO is buf cc = dup cc is buf

-- | Duplicate buffer
dupBuffer :: ( FAICopy p1 p2, FAI p1, FAI p2
             , Storable b, Pf p2 a ~ b, Pf p1 a ~ b
             , Shape sh)
          => Bool                           -- ^ Whether copy data
          -> Buffer sh p1 a                    -- ^ buffer (src)
          -> Accelerate p2 (Buffer sh p2 a)    -- ^ buffer (dst)
dupBuffer is buf = Accelerate (dupBufferIO is buf)

-- | Duplicate buffer (for debug)
dupBufferD :: ( FAICopy p2 p1, FAI p1, FAI p2
              , Storable b, Pf p2 a ~ b, Pf p1 a ~ b
              , Shape sh)
           => Bool                           -- ^ Whether copy data
           -> Buffer sh p2 a                    -- ^ buffer (src)
           -> Accelerate p2 (Buffer sh p1 a)    -- ^ buffer (dst)
dupBufferD is buf = Accelerate $ \cc -> replaceContext cc <$>  dup undefined is buf


reshape :: (Shape sh1, Shape sh2)
        => Buffer sh1 p a
        -> sh2
        -> Buffer sh2 p a
reshape (Buffer p sh1) sh2 =
  if shLen sh1 == shLen sh2
    then Buffer p sh2
    else error "Different shape lengths"

withBuffer :: FAI p => Buffer sh p a -> (Ptr (Pf p a) -> IO b) -> IO b
withBuffer buf = withForeignPtr (bufPtr buf)
