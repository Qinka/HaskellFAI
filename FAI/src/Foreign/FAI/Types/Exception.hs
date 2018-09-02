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
Module: Foreign.FAI.Types.Exception
Description: The defined exceptions of FAI.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The defined exceptions of FAI.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}

module Foreign.FAI.Types.Exception
  ( module Control.Monad.Catch
  , throw
  , getExceptionCallStack
  , cleanExceptionCallStack
  , NullPtrAllocated(..)
  , HasCallStack
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef             (IORef, modifyIORef, newIORef,
                                         readIORef, writeIORef)
import           GHC.Stack              (CallStack, HasCallStack,
                                         freezeCallStack)
import           System.IO.Unsafe       (unsafePerformIO)

{-# NOINLINE ref #-}
ref :: IORef [CallStack]
ref = unsafePerformIO $ newIORef []

-- | Throw the exception with the call stack[Thread unsafe]
throw :: (HasCallStack, MonadThrow m, MonadIO m, Exception e) => e -> m a
throw = (liftIO (modifyIORef ref (freezeCallStack ?callStack :)) >>) . throwM

-- | Clean the Exception call stack
cleanExceptionCallStack :: MonadIO m => m ()
cleanExceptionCallStack = liftIO $ writeIORef ref []

-- | Get the exception call stack
getExceptionCallStack   :: MonadIO m => m [CallStack]
getExceptionCallStack   = liftIO $ readIORef ref

-- | Buffer allocated null pointer
data NullPtrAllocated = NullPtrAllocated
    deriving(Eq)

instance Show NullPtrAllocated where
  show _ = "Error: The buffer allocator allocated a null pointer for the buffer."

instance Exception NullPtrAllocated
