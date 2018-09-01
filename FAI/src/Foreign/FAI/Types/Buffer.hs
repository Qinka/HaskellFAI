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
Module: Foreign.FAI.Types.Buffer
Description: Class of buffers.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

Class of buffers.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Foreign.FAI.Types.Buffer
  ( Buffer(..)
  , Pf
  , bufSize
  , bufByte
  ) where

import           Foreign.FAI.Types.Shape (Shape (..))
import           Foreign.ForeignPtr      (ForeignPtr)
import           Foreign.Storable        (Storable (..))

-- | Platform types.
-- The instance @Storable (Pf p t)@ means the real world infomation about storing.
type family Pf p t :: *

-- | Class Buffer defined what should be able to do with a buffer.
class Buffer b where
  type BufferPlatform b                                                                     -- ^ The platform of a buffer
  type BufferType     b                                                                     -- ^ The type of a buffer refer to
  type BufferShape    b                                                                     -- ^ The shape of Buffer
  getBufferPtr   :: b -> ForeignPtr (Pf (BufferPlatform b) (BufferType b))                  -- ^ Get pointer
  setBufferPtr   :: b -> ForeignPtr (Pf (BufferPlatform b) (BufferType b)) -> b             -- ^ Set pointer
  getBufferShape :: b -> BufferShape b                                                      -- ^ Get the shape
  setBufferShape :: b -> BufferShape b -> b                                                 -- ^ Set the shape
  makeBuffer     :: ForeignPtr (Pf (BufferPlatform b) (BufferType b)) -> BufferShape b -> b -- ^ Combine the pointer and shape.

-- | The number of elements.
bufSize :: (Shape sh, BufferShape b ~ sh, Buffer b)
        => b -> Int
bufSize = shLen . getBufferShape

-- | The bytes of buffer's elements.
bufByte :: ( Storable c, c ~ Pf p a
           , BufferType     b ~ a
           , BufferPlatform b ~ p
           , BufferShape    b ~ sh, Shape sh
           , Buffer b
           )
        => b
        -> Int
bufByte bf = size (getBufferPtr bf) undefined * shLen (getBufferShape bf)
  where size :: Storable a => ForeignPtr a -> a -> Int
        size _ = sizeOf
