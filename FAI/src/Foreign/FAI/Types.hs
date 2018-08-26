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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

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
  , Shape(..)
  , Z(..)
  , (:.)(..)
  , bufSize
  , bufByte
  , DIM0
  , DIM1
  , DIM2
  , DIM3
  , DIM4
  , DIM5
  , ignoreLogger
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger
import Control.Monad.Catch
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

-- | Platform types
type family Pf p t :: *

-- | buffer hosted pointer and size
data Buffer sh p a = Buffer
  { bufPtr   :: {-# UNPACK #-} !(ForeignPtr (Pf p a)) -- ^ pointer
  , bufShape ::                !sh                    -- ^ number of size
  }
  deriving (Show, Eq)

-- | Context of platform
--
-- The Haskell GC can not guarantee that @Context p@ will be released after
-- all the @Buffer p a@ is released.
-- So the C implement at lower level need to make sure it.
data Context p = Context
  { unContextPtr    :: !(ForeignPtr (Context p))
  , unContextLogger :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  }

instance Show p => Show (Context p) where
  show = show . unContextPtr

instance Eq p => Eq (Context p) where
  (==) a b = unContextPtr a == unContextPtr b

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
  faiDefaultContextIO :: IO (Context p)

-- | Copy data from platform @p1@ to platform @p2@.
class (FAI p1, FAI p2) =>  FAICopy p1 p2 where
  faiMemCopy :: (Storable b, (Pf p1 a) ~ b, Storable c, (Pf p2 a) ~ c, Shape sh)
             => Buffer sh p2 a       -- ^ Destination
             -> Buffer sh p1 a       -- ^ Source
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

instance MonadLogger (Accelerate p) where
  monadLoggerLog l ls ll msg = Accelerate $ \c -> (\rt -> (rt, c)) <$> unContextLogger c l ls ll (toLogStr msg)

ignoreLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
ignoreLogger _ _ _ _ = return ()

instance MonadThrow (Accelerate p) where
  throwM e = Accelerate $ \c -> (\r -> (r,c)) <$> throwM e

instance MonadCatch (Accelerate p) where
  catch (Accelerate m) h = Accelerate $ \c -> catch (m c) (`hIO` c)
    where hIO e = let (Accelerate hm) = h e in hm

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

class Shape sh where
  shLen :: sh -> Int

bufSize :: Shape sh => Buffer sh p a -> Int
bufSize = shLen . bufShape

bufByte :: (Storable b, b ~ Pf p a, Shape sh)
        => Buffer sh p a
        -> Int
bufByte (Buffer fp sh) = size fp undefined * shLen sh
  where size :: Storable a => ForeignPtr a -> a -> Int
        size _ = sizeOf

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
