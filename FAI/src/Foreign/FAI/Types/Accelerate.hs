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
Description: The functions and types about ``Accelerate''.
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: LGPL3
Maintainer: me@qinka.pro
Stability: experimental
Portability: unknown

The functions and types about ``Accelerate''.
-}

module Foreign.FAI.Types.Accelerate
  ( Accelerate(..)
  , ignoreLogger
  , liftIO
  ) where

import           Control.Monad.Catch       (MonadCatch (..), MonadThrow (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Logger      (Loc, LogLevel, LogSource, LogStr,
                                            MonadLogger (..), toLogStr)
import           Foreign.FAI.Types.Context (ContextLogger (..))

-- | Accelearate type.
newtype Accelerate p a = Accelerate
  { doAccelerate :: p -> IO (a, p) -- ^ accelerate kernel
  }

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

instance ContextLogger p => MonadLogger (Accelerate p) where
  monadLoggerLog l ls ll msg = Accelerate $ \c -> (\rt -> (rt, c)) <$> getContextLogger c l ls ll (toLogStr msg)

-- | The logger which will ignore all the logs.
ignoreLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
ignoreLogger _ _ _ _ = return ()

instance MonadThrow (Accelerate p) where
  throwM e = Accelerate $ \c -> (\r -> (r,c)) <$> throwM e

instance MonadCatch (Accelerate p) where
  catch (Accelerate m) h = Accelerate $ \c -> catch (m c) (`hIO` c)
    where hIO e = let (Accelerate hm) = h e in hm
