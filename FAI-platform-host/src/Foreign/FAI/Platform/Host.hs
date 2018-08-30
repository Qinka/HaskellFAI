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
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module Foreign.FAI.Platform.Host
  ( Host(..)
  , Pf
  , nullHostContext
  , nullHostContextIO
  ) where

import           Control.Monad
import           Foreign.C.Types
import           Foreign.FAI.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Language.C.Inline  as C
import           System.IO.Unsafe

C.include "<string.h>"
C.include "<stdlib.h>"

-- | Host backend (use C runtime)
--
-- The @malloc@ and @free@ are used for memory management.
data Host = Host !(ForeignPtr Host) !Logger

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

hostMemCopy :: ForeignPtr a -> ForeignPtr a -> CInt -> IO ()
hostMemCopy fdst fsrc size =
  withForeignPtr fdst $ \dst' ->
    withForeignPtr fsrc $ \src' ->
  let dst = castPtr dst'
      src = castPtr src'
  in void $ [C.exp| void* {memcpy($(void *dst), $(void *src), $(int size))} |]

instance ContextLogger Host where
  getContextLogger (Host _ l) = l
  setContextLogger (Host p _) = Host p

instance ContextPointer Host where
  getContextPointer (Host p _) = p
  setContextPointer (Host _ l) = (`Host` l)
  nullContextIO = nullHostContextIO

instance FAI Host where
  faiMemAllocate    _ = hostMemAllocate . fromIntegral
  faiMemRelease     _ = hostMemRelease
  faiMemReleaseP    _ = Right <$> hostMemReleaseP

instance FAICopy Host Host where
  faiMemCopy dst src = do
    when (bufSize dst /= bufSize src) $ error "Different size."
    hostMemCopy (getBufferPtr dst) (getBufferPtr src) $ fromIntegral $ bufByte dst

-- | Null pointer context of Host
nullHostContextIO :: IO Host
nullHostContextIO = Host <$> newForeignPtr_ nullPtr <*> return ignoreLogger

nullHostContext :: Host
nullHostContext = unsafePerformIO nullHostContextIO
