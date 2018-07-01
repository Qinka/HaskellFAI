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

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Foreign.FAI.Types
  ( Pf(..)
  , Buffer(..)
  , Context(..)
  , Accelerate(..)
  , FAI(..)
  , FAICopy(..)
  , newBuffer
  , liftIO
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import Control.Monad

type family Pf p t :: *

data Buffer p a = Buffer
  { buffPtr :: ForeignPtr (Pf p a) -- ^ pointer
  , bufSize :: Int                 -- ^ number of size
  }
  deriving (Show, Eq)

data Context p = Context
  { unContextPtr :: ForeignPtr (Context p)
  }
  deriving (Show, Eq)

newtype Accelerate p a = Accelerate
  { doAccelerate :: Context p -> IO (a, Context p)
  }

type PfPtr p a = Ptr (Pf p a)

class FAI p where
  faiMemAllocate :: Int         -- ^ size
                 -> Accelerate p (Ptr a)  -- ^ Pointer
  faiMemRelease  :: Ptr a       -- ^ Pointer
                 -> Accelerate p ()
  faiMemReleaseP :: Accelerate p (FinalizerPtr a) -- ^ pointer of the function of release the pointer

class (FAI p1, FAI p2) =>  FAICopy p1 p2 where
  faiMemCopy :: (Storable b, (Pf p1 a) ~ b, Storable c, (Pf p2 a) ~ c)
             => Buffer p1 a       -- ^ Destination
             -> Buffer p2 a       -- ^ Source
             -> Accelerate p2 ()

instance Functor (Accelerate p) where
  fmap f (Accelerate a) = Accelerate $ \c -> do
    (r,c') <- a c
    return (f r, c')

instance Applicative (Accelerate p) where
  pure x = Accelerate $ \c -> return (x, c)
  (<*>) (Accelerate ff) (Accelerate fa) = Accelerate $ \c1 -> do
    (r1,c2) <- fa c1
    (r2,c3) <- ff c2
    return (r2 r1, c3)

instance Monad (Accelerate p) where
  (>>=) (Accelerate a) mf = Accelerate $ \c1 -> do
    (r1, c2) <- a c1
    let (Accelerate m) = mf r1
    m c2

instance MonadIO (Accelerate p) where
  liftIO m = Accelerate $ \c -> (\r -> (r,c)) <$> m

newBuffer :: (FAI p, Storable b, (Pf p a) ~ b)
          => Int -- ^ number of items
          -> Accelerate p (Buffer p a)
newBuffer n = do
  fin <- faiMemReleaseP
  ptr <- alloc undefined
  when (nullPtr == ptr) $ error "Can not allocate memory."
  fptr <- liftIO $ newForeignPtr_ ptr
  liftIO $ addForeignPtrFinalizer fin fptr
  return $ Buffer fptr n
  where alloc :: (FAI p, Storable b) => b -> Accelerate p (Ptr b)
        alloc u = faiMemAllocate (n * sizeOf u)
