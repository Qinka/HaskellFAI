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
  , Context(..)
  , Accelerate(..)
  , FAI(..)
  , FAICopy(..)
  , FinalizerContextPtr
  , accelerate
  , newBuffer
  , newBufferIO
  , dupBuffer
  , dupBufferIO
  , dupBufferD
  , liftIO
  ) where

import           Control.Monad
import           Foreign.FAI.Internal
import           Foreign.FAI.Types
import           Foreign.Ptr

-- | run the @Accelerate@.
accelerate :: Context p -> Accelerate p a -> IO a
accelerate cc = (fst <$>) . flip doAccelerate cc

-- | Allocate new buffer (IO)
newBufferIO :: (FAI p, Storable b, (Pf p a) ~ b)
            => Int                         -- ^ number of items
            -> Context p                   -- ^ platform context
            -> IO (Buffer p a, Context p)  -- ^ Buffer and (new) context
newBufferIO n cc =  do
  fin <- faiMemReleaseP cc
  (ptr, size) <- alloc cc undefined
  when (nullPtr == ptr) $ error "Can not allocate memory."
  buf <- autoNewForeignPtr fin cc ptr size
  return (buf, cc)
  where alloc :: (FAI p, Storable b) => Context p ->  b -> IO (Ptr b, Int)
        alloc c' u =
          let size = n * sizeOf u
          in (\p -> (p, size)) <$> faiMemAllocate c' (n * sizeOf u)

-- | Allocate buffer
newBuffer :: (FAI p, Storable b, (Pf p a) ~ b)
          => Int                        -- ^ number of items
          -> Accelerate p (Buffer p a)  -- ^ buffer
newBuffer = Accelerate .newBufferIO

-- | Duplicate buffer (IO)
dupBufferIO :: ( FAICopy p1 p2, FAI p1, FAI p2
               , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
            => Bool                           -- ^ Whether copy data
            -> Buffer p1 a                    -- ^ buffer (src)
            -> Context p2                     -- ^ platform context
            -> IO (Buffer p2 a, Context p2)   -- ^ buffer (dst) and context
dupBufferIO is buf cc = dup cc is buf

-- | Duplicate buffer
dupBuffer :: ( FAICopy p1 p2, FAI p1, FAI p2
             , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
          => Bool                           -- ^ Whether copy data
          -> Buffer p1 a                    -- ^ buffer (src)
          -> Accelerate p2 (Buffer p2 a)    -- ^ buffer (dst)
dupBuffer is buf = Accelerate (dupBufferIO is buf)

-- | Duplicate buffer (for debug)
dupBufferD :: ( FAICopy p2 p1, FAI p1, FAI p2
              , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
           => Bool                           -- ^ Whether copy data
           -> Buffer p2 a                    -- ^ buffer (src)
           -> Accelerate p2 (Buffer p1 a)    -- ^ buffer (dst)
dupBufferD is buf = Accelerate $ \cc -> replaceContext cc <$>  dup undefined is buf
