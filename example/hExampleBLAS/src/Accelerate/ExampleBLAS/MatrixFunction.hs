{-# LANGUAGE TypeFamilies #-}

module Accelerate.ExampleBLAS.MatrixFunction where

import           Accelerate.ExampleBLAS.Binding.MatrixFunction
import           Control.Monad
import           Foreign.FAI
import           Foreign.Ptr
import           Foreign.Storable

hwForwardMul :: ( FAI p, a ~ Pf p Float
                , Storable a)
             => Buffer (Int, Int) p Float
             -> Buffer (Int, Int) p Float
             -> Context p
             -> IO (Buffer (Int, Int) p Float, Context p)
hwForwardMul b1 b2 cc = do
  let (m,  n) = bufShape b1
      (n', s) = bufShape b2
  when (n /= n') $ error "shape error"
  (outB, cc') <- newBufferIO (m,s) cc
  withBuffer b1 $ \p1 -> withBuffer b2 $ \p2 -> withBuffer outB $ \po ->
    forward_HW_mul2D (castPtr po) (castPtr p1) (castPtr p2) m n s
  return (outB, cc')


hwBackwardMulA :: ( FAI p, a ~ Pf p Float
                  , Storable a)
               => Buffer (Int, Int) p Float
               -> Buffer (Int, Int) p Float
               -> Context p
               -> IO (Buffer (Int, Int) p Float, Context p)
hwBackwardMulA b1 b2 cc = do
  let (m,  n) = bufShape b1
      (n', s) = bufShape b2
  when (n /= n') $ error "shape error"
  (outB, cc') <- newBufferIO (m,s) cc
  withBuffer b1 $ \p1 -> withBuffer b2 $ \p2 -> withBuffer outB $ \po ->
    backward_HW_mul2D_A (castPtr po) (castPtr p1) (castPtr p2) m n s
  return (outB, cc')

hwBackwardMulB :: ( FAI p, a ~ Pf p Float
                  , Storable a)
               => Buffer (Int, Int) p Float
               -> Buffer (Int, Int) p Float
               -> Context p
               -> IO (Buffer (Int, Int) p Float, Context p)
hwBackwardMulB b1 b2 cc = do
  let (m,  n) = bufShape b1
      (n', s) = bufShape b2
  when (n /= n') $ error "shape error"
  (outB, cc') <- newBufferIO (m,s) cc
  withBuffer b1 $ \p1 -> withBuffer b2 $ \p2 -> withBuffer outB $ \po ->
    backward_HW_mul2D_B (castPtr po) (castPtr p1) (castPtr p2) m n s
  return (outB, cc')
