{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.MatrixFunction
  ( forward_HW_mul2D
  , backward_HW_mul2D_A
  , backward_HW_mul2D_B
  ) where

#include <example-blas/matrix_function.h>

import Foreign.Ptr
import Foreign.C

{# fun forward_HW_mul2D
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  , `Int'
  , `Int'
  } -> `()' #}

{# fun backward_HW_mul2D_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  , `Int'
  , `Int'
  } -> `()' #}

{# fun backward_HW_mul2D_B
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  , `Int'
  , `Int'
  } -> `()' #}