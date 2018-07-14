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
import           Foreign.ForeignPtr

-- | allocate new foreign pointer
autoNewForeignPtr :: FinalizerContextPtr p (Pf p a) -- ^ Context p concerned finalizer
                  -> Context p                      -- ^ Context
                  -> Ptr (Pf p a)                   -- ^ pointer
                  -> Int                            -- ^ Size
                  -> IO (Buffer p a)                -- ^ buffer
autoNewForeignPtr fin cc ptr size = fmap (`Buffer` size) $ case fin of
  Left  f -> newForeignPtrEnv f (unContextPtr cc) ptr
  Right f -> newForeignPtr    f                   ptr

replaceContext :: Context p2 -> (a, Context p1) -> (a, Context p2)
replaceContext cc (a, _) = (a, cc)

-- | Duplicate data
dup :: ( FAICopy p1 p2, FAI p1, FAI p2
       , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
       => Context p2                    -- ^ context
       -> Bool                          -- ^ whether copy data
       -> Buffer p1 a                   -- ^ buffer (src)
       -> IO (Buffer p2 a, Context p2)  -- ^ buffer (dst)
dup cc is buf = do
  fin <- faiMemReleaseP cc
  let size = bufSize buf
  ptr  <- faiMemAllocate cc size
  bDst <- autoNewForeignPtr fin cc ptr size
  when is $ faiMemCopy bDst buf
  return (bDst, cc)
