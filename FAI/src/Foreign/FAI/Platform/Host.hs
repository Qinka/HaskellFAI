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
Module: Foreign.FAI.Platform.Host
Description: The host platform instance.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The host platform instance.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Foreign.FAI.Platform.Host
  ( Host(..)
  , Pf(..)
  ) where

import qualified Language.C.Inline as C
import Foreign.FAI.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad

C.include "<stdlib.h>"

data Host = Host

type instance Pf Host Float  = Float
type instance Pf Host Double = Double
type instance Pf Host Int    = Int
type instance Pf Host Word   = Word

hostMemAllocate :: CInt -> IO (Ptr a)
hostMemAllocate n = castPtr <$> [C.exp| void* { malloc($(int n))}|]

hostMemRelease :: Ptr a -> IO ()
hostMemRelease n' = [C.exp| void { free($(void *n)) }|]
  where n = castPtr n'

hostMemReleaseP :: IO (FinalizerPtr a)
hostMemReleaseP = castPtrToFunPtr <$> [C.exp| void* {*free}|]

instance FAI Host where
  faiMemAllocate n = liftIO $ hostMemAllocate $ fromIntegral n
  faiMemRelease  p = liftIO $ hostMemRelease p
  faiMemReleaseP   = liftIO hostMemReleaseP

