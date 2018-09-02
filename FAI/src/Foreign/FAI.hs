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
Module: Foreign.FAI
Description: The Haskell Foreign Accelerate Interace.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The Haskell Foreign Accelerate Interace.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foreign.FAI
  ( module Foreign.FAI.Types
  , accelerate
  , accelerateEither
  , withLogger
  , exitHandler
  , exitFailureHandler
  , handleException
  , newBuffer
  , newBufferIO
  , dupBuffer
  , dupBufferD
  , dupBufferIO
  , logger
  , withBuffer
  , withBuffer'
  ) where

import           Control.Monad
import           Control.Monad.Logger        (LoggingT (..), logError, logInfo)
import           Data.Text                   (pack)
import           Foreign.FAI.Internal
import           Foreign.FAI.Types
import           Foreign.FAI.Types.Exception
import           Foreign.ForeignPtr          (castForeignPtr, withForeignPtr)
import           Foreign.Ptr                 (Ptr, nullPtr)
import           Foreign.Storable            (Storable (..))
import           GHC.Stack                   (HasCallStack, prettyCallStack)
import           System.Exit                 (ExitCode (..), exitWith)

-- | With logger do action
withLogger :: (HasCallStack, FAI p, ContextLogger p)
           => (LoggingT (Accelerate p) () -> Accelerate p ())
           -> Accelerate p a
           -> Accelerate p a
withLogger logging = (logging logger >>) . ($logInfo "Enable logger" >>)

-- | Catch the exception, display, and then exit with exit-code
exitHandler :: (HasCallStack, FAI p, ContextLogger p)
            => ExitCode -> SomeException -> Accelerate p a
exitHandler ec (SomeException e) = do
  exc <- getExceptionCallStack
  $logError $ pack (displayException e) `mappend` "\n" `mappend` pack (unlines $ map prettyCallStack exc)
  liftIO $ exitWith ec

  
-- | Catch the exception, display, and then exit with exit-code: 1
exitFailureHandler :: (HasCallStack, FAI p, ContextLogger p)
                   => SomeException -> Accelerate p a
exitFailureHandler = exitHandler (ExitFailure 1)

-- | Handle the exception(with handler).
handleException :: (HasCallStack, ContextLogger p)
                => (SomeException -> Accelerate p a)
                -> Accelerate p a
                -> Accelerate p a
handleException = flip catchAll

infix 3 `accelerate`
-- | run the @Accelerate@.
accelerate  :: p
            -> Accelerate p a
            -> IO a
accelerate cc = (cleanExceptionCallStack >>) . (fst <$>) . flip doAccelerate cc

accelerateEither :: (Exception e)
                 => p -> Accelerate p a -> IO (Either e a)
accelerateEither cc = try . (fst <$>) . flip doAccelerate cc

-- | Allocate new buffer (IO)
newBufferIO :: ( FAI p, Buffer b, ContextPointer p
               , Storable c, (Pf p a) ~ c
               , BufferShape    b ~ sh, Shape sh
               , BufferType     b ~ a
               , BufferPlatform b ~ p
               )
            => sh         -- ^ shape
            -> p          -- ^ platform context
            -> IO (b, p)  -- ^ Buffer and (new) context
newBufferIO sh cc =  do
  fin <- faiMemReleaseP cc
  ptr <- alloc cc undefined
  when (nullPtr == ptr) $ throwM NullPtrAllocated
  buf <- autoNewForeignPtr fin cc ptr sh
  return (buf, cc)
  where alloc :: (FAI p, Storable b) => p ->  b -> IO (Ptr b)
        alloc c' u =
          faiMemAllocate c' $ shLen sh * sizeOf u

-- | Allocate buffer
newBuffer :: ( FAI p, Buffer b, ContextPointer p
             , Storable c, (Pf p a) ~ c
             , BufferShape    b ~ sh, Shape sh
             , BufferType     b ~ a
             , BufferPlatform b ~ p
             )
          => sh              -- ^ shape
          -> Accelerate p b  -- ^ buffer
newBuffer = Accelerate . newBufferIO

-- | Duplicate buffer (IO)
dupBufferIO :: ( FAICopy p1 p2, FAI p1, FAI p2
               , Storable c, Pf p2 a ~ c, Pf p1 a ~ c
               , Buffer b1, Buffer b2
               , BufferPlatform b1 ~ p1
               , BufferPlatform b2 ~ p2
               , BufferShape    b1 ~ sh
               , BufferShape    b2 ~ sh
               , BufferType     b1 ~ a
               , BufferType     b2 ~ a
               , Shape sh)
            => Bool          -- ^ Whether copy data
            -> b1            -- ^ buffer (src)
            -> p2            -- ^ platform context
            -> IO (b2, p2)   -- ^ buffer (dst) and context
dupBufferIO is buf cc = dup cc is buf

-- | Duplicate buffer
dupBuffer :: ( FAICopy p1 p2, FAI p1, FAI p2
             , Storable c, Pf p2 a ~ c, Pf p1 a ~ c
             , Buffer b1, Buffer b2
             , BufferPlatform b1 ~ p1
             , BufferPlatform b2 ~ p2
             , BufferShape    b1 ~ sh
             , BufferShape    b2 ~ sh
             , BufferType     b1 ~ a
             , BufferType     b2 ~ a
             , Shape sh)
          => Bool                -- ^ Whether copy data
          -> b1                  -- ^ buffer (src)
          -> Accelerate p2 b2    -- ^ buffer (dst)
dupBuffer is buf = Accelerate (dupBufferIO is buf)

-- | Duplicate buffer (for debug)
dupBufferD :: ( FAICopy p1 p2, FAI p1, FAI p2
             , Storable c, Pf p2 a ~ c, Pf p1 a ~ c
             , Buffer b1, Buffer b2
             , BufferPlatform b1 ~ p1
             , BufferPlatform b2 ~ p2
             , BufferShape    b1 ~ sh
             , BufferShape    b2 ~ sh
             , BufferType     b1 ~ a
             , BufferType     b2 ~ a
             , Shape sh)
           => Bool                -- ^ Whether copy data
           -> b1                  -- ^ buffer (src)
           -> Accelerate p1 b2    -- ^ buffer (dst)
dupBufferD is buf = Accelerate $ \cc -> replaceContext cc <$>  dup undefined is buf

-- | With buffer for a pointer
withBuffer :: ( FAI p, Buffer b
              , BufferType     b ~ a
              , BufferPlatform b ~ p
              )
           => b -> (Ptr (Pf p a) -> IO c) -> IO c
withBuffer buf = withForeignPtr (getBufferPtr buf)

-- | With buffer for a pointer(casted)
withBuffer' :: ( FAI p, Buffer b
               , BufferType     b ~ a
               , BufferPlatform b ~ p
               )
            => b -> (Ptr a -> IO c) -> IO c
withBuffer' buf = withForeignPtr (castForeignPtr $ getBufferPtr buf)

-- | Operator for set logger.
logger :: (FAI p, ContextLogger p) => LoggingT (Accelerate p) ()
logger = LoggingT $ \contextLogger -> Accelerate $ \c -> return ((), setContextLogger c contextLogger)
