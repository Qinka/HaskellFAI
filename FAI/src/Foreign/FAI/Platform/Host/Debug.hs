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
Module: Foreign.FAI.Platform.Host.Debug
Description: The debug component for Host.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The debug component for Host.
-}

{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Foreign.FAI.Platform.Host.Debug
  ( peekHostBuffer
  , pokeHostBuffer
  , toHostBuffer
  , unsafePeekHostBuffer
  , unsafeToHostBuffer
  ) where

import           Foreign.FAI
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           System.IO.Unsafe

-- | Copy the data from pointer to Haskell list.
peekHostBuffer :: (Storable b, Pf Host a ~ b, Shape sh)
               => Buffer sh Host a   -- ^ Buffer
               -> IO [b]          -- ^ Haskell list
peekHostBuffer bf =
  withForeignPtr (bufPtr bf) $ \ptr ->
    peekArray (bufSize bf) ptr

-- | Copy the data from Haskell list into pointer.
pokeHostBuffer :: (Storable b, Pf Host a ~ b, Shape sh)
               => Buffer sh Host a -- ^ Host buffer
               -> [b]           -- ^ list
               -> IO ()
pokeHostBuffer (Buffer fp s) ls = do
  withForeignPtr fp $ \ptr ->
    pokeArray ptr $ take len ls
  return ()
  where lsLen = length ls
        bfLen = shLen s
        len   = min bfLen lsLen

-- | Transform list to host buffer.
toHostBuffer :: (Storable b, Pf Host a ~ b)
             => [b]                 -- ^ List
             -> IO (Buffer Int Host a)  -- ^ Host buffer
toHostBuffer ls = do
  bf <- fst <$> newBufferIO (length ls) cc
  withForeignPtr (bufPtr bf) $ \ptr ->
    pokeArray ptr ls
  return bf
  where cc :: Context Host
        cc = Context undefined
-- | Unsafe peek
unsafePeekHostBuffer :: (Storable b,Pf Host a ~ b, Shape sh)
                     => Buffer sh Host a
                     -> [b]
unsafePeekHostBuffer = unsafePerformIO . peekHostBuffer

-- | Unsafe poke
unsafeToHostBuffer :: (Storable b, Pf Host a ~ b)
                   => [b]
                   -> Buffer Int Host a
unsafeToHostBuffer = unsafePerformIO . toHostBuffer
