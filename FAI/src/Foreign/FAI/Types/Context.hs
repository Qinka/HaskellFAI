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
Module: Foreign.FAI.Types.Context
Description: The context of points and logger
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The context of points and logger.
-}

module Foreign.FAI.Types.Context
  ( ContextPointer(..)
  , ContextLogger(..)
  , Logger
  ) where

import Foreign.ForeignPtr (ForeignPtr)
import           Control.Monad.Logger(Loc, LogSource, LogLevel, LogStr)

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

class ContextPointer p where
  getContextPointer  :: p -> ForeignPtr p
  setContextPointer  :: p -> ForeignPtr p -> p
  nullContextIO    :: IO p


class ContextLogger p where
  getContextLogger :: p -> Logger
  setContextLogger :: p -> Logger -> p
