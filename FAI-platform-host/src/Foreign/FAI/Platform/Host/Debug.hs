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
import Foreign.Storable

-- | Copy the data from pointer to Haskell list.
peekHostBuffer :: ( Storable c, Pf Host a ~ c
                  , Shape sh, Buffer b
                  , BufferPlatform b ~ Host
                  , BufferShape    b ~ sh
                  , BufferType     b ~ a)
               => b   -- ^ Buffer
               -> IO [c]          -- ^ Haskell list
peekHostBuffer bf =
  withBuffer bf $ \ptr ->
    peekArray (bufSize bf) ptr

-- | Copy the data from Haskell list into pointer.
pokeHostBuffer :: ( Storable c, Pf Host a ~ c
                  , Shape sh, Buffer b
                  , BufferPlatform b ~ Host
                  , BufferShape    b ~ sh
                  , BufferType     b ~ a)
               => b -- ^ Host buffer
               -> [c]           -- ^ list
               -> IO ()
pokeHostBuffer buf ls = do
  withBuffer buf $ \ptr ->
    pokeArray ptr $ take len ls
  return ()
  where lsLen = length ls
        bfLen = bufSize buf
        len   = min bfLen lsLen

-- | Transform list to host buffer.
toHostBuffer :: ( Storable c, Pf Host a ~ c
                , Buffer b
                , BufferPlatform b ~ Host
                , BufferShape    b ~ Int
                , BufferType     b ~ a)
             => [c]                 -- ^ List
             -> IO b  -- ^ Host buffer
toHostBuffer ls = do
  bf <- fst <$> newBufferIO (length ls) nullHostContext
  withBuffer bf $ \ptr ->
    pokeArray ptr ls
  return bf

-- | Unsafe peek
unsafePeekHostBuffer :: ( Storable c, Pf Host a ~ c
                        , Shape sh, Buffer b
                        , BufferPlatform b ~ Host
                        , BufferShape    b ~ sh
                        , BufferType     b ~ a)
                     => b
                     -> [c]
unsafePeekHostBuffer = unsafePerformIO . peekHostBuffer

-- | Unsafe poke
unsafeToHostBuffer :: ( Storable c, Pf Host a ~ c
                      , Buffer b
                      , BufferPlatform b ~ Host
                      , BufferShape    b ~ Int
                      , BufferType     b ~ a)
                   => [c]
                   -> b
unsafeToHostBuffer = unsafePerformIO . toHostBuffer
