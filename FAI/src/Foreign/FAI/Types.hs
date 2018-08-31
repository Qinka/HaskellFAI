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
Module: Foreign.FAI.Types
Description: The types and the class of FAI
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The types and the class of FAI.
-}

module Foreign.FAI.Types
  ( module Foreign.FAI.Types.Accelerate
  , module Foreign.FAI.Types.Buffer
  , module Foreign.FAI.Types.Context
  , module Foreign.FAI.Types.FAI
  , module Foreign.FAI.Types.Shape
  ) where

import           Foreign.FAI.Types.Accelerate
import           Foreign.FAI.Types.Buffer
import           Foreign.FAI.Types.Context
import           Foreign.FAI.Types.FAI
import           Foreign.FAI.Types.Shape
