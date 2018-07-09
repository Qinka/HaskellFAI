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
  , dupBuffer
  , dupBufferD
  , liftIO
  , accelerate
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

type family Pf p t :: *

data Buffer p a = Buffer
  { bufPtr  :: ForeignPtr (Pf p a) -- ^ pointer
  , bufSize :: Int                 -- ^ number of size
  }
  deriving (Show, Eq)

newtype Context p = Context
  { unContextPtr :: Ptr (Context p)
  }
  deriving (Show, Eq)

newtype Accelerate p a = Accelerate
  { doAccelerate :: Context p -> IO (a, Context p)
  }

accelerate :: Context p -> Accelerate p a -> IO a
accelerate cc = (fst <$>) . flip doAccelerate cc

class FAI p where
  faiMemAllocate :: Context p
                 -> Int         -- ^ size
                 -> IO (Ptr a)  -- ^ Pointer
  faiMemRelease  :: Context p
                 -> Ptr a       -- ^ Pointer
                 -> IO ()
  faiMemReleaseP :: Context p
                 -> IO (Either
                         (FinalizerEnvPtr (Context p) a)
                         (FinalizerPtr                a))
                      -- ^ pointer of the function
                      --   of release the pointer

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

autoNewForeignPtr :: Either
                        (FinalizerEnvPtr (Context p) (Pf p a))
                        (FinalizerPtr                (Pf p a))
                  -> Context p
                  -> Ptr (Pf p a)
                  -> Int
                  -> IO (Buffer p a)
autoNewForeignPtr fin cc ptr size = fmap (`Buffer` size) $ case fin of
  Left  f -> newForeignPtrEnv f (unContextPtr cc) ptr
  Right f -> newForeignPtr    f                   ptr

newBuffer :: (FAI p, Storable b, (Pf p a) ~ b)
          => Int -- ^ number of items
          -> Accelerate p (Buffer p a)
newBuffer n = Accelerate $ \cc -> do
  fin <- faiMemReleaseP cc
  (ptr, size) <- alloc cc undefined
  when (nullPtr == ptr) $ error "Can not allocate memory."
  buf <- autoNewForeignPtr fin cc ptr size
  return (buf, cc)
  where alloc :: (FAI p, Storable b) => Context p ->  b -> IO (Ptr b, Int)
        alloc cc u =
          let size = n * sizeOf u
          in (\p -> (p, size)) <$> faiMemAllocate cc (n * sizeOf u)

-- | without copy things
dupBuffer :: ( FAICopy p1 p2, FAI p1, FAI p2
             , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
          => Bool
           -> Buffer p1 a
          -> Accelerate p2 (Buffer p2 a)
dupBuffer is buf = Accelerate $ \cc -> dup cc is buf

dupBufferD :: ( FAICopy p2 p1, FAI p1, FAI p2
              , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
           => Bool
           -> Buffer p2 a
           -> Accelerate p2 (Buffer p1 a)
dupBufferD is buf = Accelerate $ \cc -> replaceContext cc <$>  dup undefined is buf

replaceContext :: Context p2 -> (a, Context p1) -> (a, Context p2)
replaceContext cc (a, _) = (a, cc)

dup :: ( FAICopy p1 p2, FAI p1, FAI p2
       , Storable b, Pf p2 a ~ b, Pf p1 a ~ b)
       => Context p2
       -> Bool
       -> Buffer p1 a
       -> IO (Buffer p2 a, Context p2)
dup cc is buf = do
  fin <- faiMemReleaseP cc
  let size = bufSize buf
  ptr  <- faiMemAllocate cc size
  bDst <- autoNewForeignPtr fin cc ptr size
  when is $ faiMemCopy bDst buf
  return (bDst, cc)

