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
Module: Foreign.FAI.Types.FAI
Description: The types and the class of FAI
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The types and the class of FAI.
-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Foreign.FAI.Types.FAI
  ( FAI(..)
  , FAICopy(..)
  , FinalizerContextPtr
  ) where

import           Foreign.FAI.Types.Buffer  (Buffer (..), Pf)
import           Foreign.FAI.Types.Context (ContextPointer (..))
import           Foreign.FAI.Types.Shape   (Shape (..))
import           Foreign.ForeignPtr        (FinalizerEnvPtr, FinalizerPtr)
import           Foreign.Ptr               (Ptr)
import           Foreign.Storable



-- | Context concened finalizer
type FinalizerContextPtr p a =
  Either (FinalizerEnvPtr p a) (FinalizerPtr a)

-- | The methods of FAI.
class ContextPointer p => FAI p where
  faiMemAllocate :: p           -- ^ Context
                 -> Int         -- ^ size
                 -> IO (Ptr a)  -- ^ Pointer
  faiMemRelease  :: p           -- ^ Context
                 -> Ptr a       -- ^ Pointer
                 -> IO ()
  faiMemReleaseP :: p           -- ^ Context
                 -> IO (FinalizerContextPtr p a)
                      -- ^ pointer of the function
                      --   of release the pointer


-- | Copy data from platform @p1@ to platform @p2@.
class (FAI p1, FAI p2) =>  FAICopy p1 p2 where
  faiMemCopy :: ( Storable c1, (Pf p1 a) ~ c1
                , Storable c2, (Pf p2 a) ~ c2
                , BufferPlatform b1 ~ p1
                , BufferPlatform b2 ~ p2
                , BufferType     b1 ~ a
                , BufferType     b2 ~ a
                , BufferShape    b1 ~ sh
                , BufferShape    b2 ~ sh
                , Buffer b1, Buffer b2
                , Shape sh)
             => b2       -- ^ Destination
             -> b1       -- ^ Source
             -> IO ()
