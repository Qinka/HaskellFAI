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
  ( Pf
  , Buffer(..)
  , Context(..)
  , Accelerate(..)
  , FAI(..)
  , FAICopy(..)
  , FinalizerContextPtr
  , Storable(..)
  , Ptr
  , ForeignPtr
  , liftIO
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

-- | Platform types
type family Pf p t :: *

-- | buffer hosted pointer and size
data Buffer p a = Buffer
  { bufPtr  :: {-# UNPACK #-} !(ForeignPtr (Pf p a)) -- ^ pointer
  , bufSize :: {-# UNPACK #-} !Int                 -- ^ number of size
  }
  deriving (Show, Eq)

-- | Context of platform
--
-- The Haskell GC can not guarantee that @Context p@ will be released after
-- all the @Buffer p a@ is released.
-- So the C implement at lower level need to make sure it.
newtype Context p = Context
  { unContextPtr :: ForeignPtr (Context p)
  }
  deriving (Show, Eq)

-- | Accelearate type.
newtype Accelerate p a = Accelerate
  { doAccelerate :: Context p -> IO (a, Context p)
  }

-- | Context concened finalizer
type FinalizerContextPtr p a =
  Either (FinalizerEnvPtr (Context p) a) (FinalizerPtr a)

-- | FAI interface
class FAI p where
  faiMemAllocate :: Context p   -- ^ Context
                 -> Int         -- ^ size
                 -> IO (Ptr a)  -- ^ Pointer
  faiMemRelease  :: Context p   -- ^ Context
                 -> Ptr a       -- ^ Pointer
                 -> IO ()
  faiMemReleaseP :: Context p   -- ^ Context
                 -> IO (FinalizerContextPtr p a)
                      -- ^ pointer of the function
                      --   of release the pointer

-- | Copy data from platform @p1@ to platform @p2@.
class (FAI p1, FAI p2) =>  FAICopy p1 p2 where
  faiMemCopy :: (Storable b, (Pf p1 a) ~ b, Storable c, (Pf p2 a) ~ c)
             => Buffer p2 a       -- ^ Destination
             -> Buffer p1 a       -- ^ Source
             -> IO ()

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
