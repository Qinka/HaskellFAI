{-# LANGUAGE TypeFamilies #-}

module Accelerate.ExampleBLAS.Fill
    ( scale
    , random
    , eye2D
    )where

import           Accelerate.ExampleBLAS.Binding.Fill
import           Foreign.FAI
import           Foreign.Ptr
import           Foreign.Storable

scale :: (FAI p, Shape sh, Storable b, b ~ Pf p Float)
      => sh -> Float -> Accelerate p (Buffer sh p Float)
scale sh s = do
  buf <- newBuffer sh
  liftIO $ withBuffer buf $ \p ->
    fill_N_scalar (castPtr p) s $ shLen sh
  return buf

eye2D :: (FAI p, Storable b, b ~ Pf p Float)
        => Int -> Accelerate p (Buffer (Int, Int) p Float)
eye2D i = do
  buf <- newBuffer (i, i)
  liftIO $ withBuffer buf $ \p ->
    fill_HW_eye (castPtr p) i
  return buf

random :: (FAI p, Shape sh, Storable b, b ~ Pf p Float)
       => sh -> Accelerate p (Buffer sh p Float)
random sh = do
  buf <- newBuffer sh
  liftIO $ withBuffer buf $ \p ->
    fill_N_random (castPtr p) $ shLen sh
  return buf
