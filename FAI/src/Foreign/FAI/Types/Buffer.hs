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

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts #-}

module Foreign.FAI.Types.Buffer
  ( Buffer(..)
  , Pf
  , bufSize
  , bufByte
  ) where

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.FAI.Types.Shape(Shape(..))
import Foreign.Storable(Storable(..))

-- | Platform types
type family Pf p t :: *

class Shape (BufferShape b) => Buffer b where
  type BufferPlatform b
  type BufferType     b
  type BufferShape    b
  getBufferPtr   :: b -> ForeignPtr (Pf (BufferPlatform b) (BufferType b))
  setBufferPtr   :: b -> ForeignPtr (Pf (BufferPlatform b) (BufferType b)) -> b
  getBufferShape :: b -> BufferShape b
  setBufferShape :: b -> BufferShape b -> b
  makeBuffer     :: ForeignPtr (Pf (BufferPlatform b) (BufferType b)) -> BufferShape b -> b

  
bufSize :: (Shape sh, BufferShape b ~ sh, Buffer b)
        => b -> Int
bufSize = shLen . getBufferShape

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