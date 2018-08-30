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
Module: Foreign.FAI.Types.Shape
Description: Class of shape.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

Class of shape.
-}

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}

module Foreign.FAI.Types.Shape
  ( Shape(..)
  , Z(..)
  , DIM0
  , DIM1
  , DIM2
  , DIM3
  , DIM4
  , DIM5
  ) where

class Shape sh where
  shLen :: sh -> Int

data Z = Z
  deriving(Show, Read, Eq, Ord)

infixl 3 :.
data tail :. head = !tail :. !head
  deriving(Show, Read, Eq, Ord)

type DIM0       = Z
type DIM1       = DIM0 :. Int
type DIM2       = DIM1 :. Int
type DIM3       = DIM2 :. Int
type DIM4       = DIM3 :. Int
type DIM5       = DIM4 :. Int

instance Shape () where
  shLen _ = 1

instance Shape [Int] where
  shLen = product

instance Shape Int where
  shLen = id

instance Shape (Int, Int) where
  shLen (h, w) = h * w

instance Shape (Int, Int, Int) where
  shLen (h, w, d) = h * w * d

instance Shape (Int ,Int, Int, Int) where
  shLen (h, w, d, c) = h * w * d * c

instance Shape (Int, Int, Int, Int, Int) where
  shLen (h, w, d, c, n) = h * w * d * c * n

instance Shape Z where
  shLen _ = 1

instance Shape sh => Shape (sh :. Int) where
  shLen (sh :. i) = shLen sh * i

  

-- bufSize :: Shape sh => Buffer sh p a -> Int
-- bufSize = shLen . bufShape

-- bufByte :: (Storable b, b ~ Pf p a, Shape sh)
--         => Buffer sh p a
--         -> Int
-- bufByte (Buffer fp sh) = size fp undefined * shLen sh
--   where size :: Storable a => ForeignPtr a -> a -> Int
--         size _ = sizeOf