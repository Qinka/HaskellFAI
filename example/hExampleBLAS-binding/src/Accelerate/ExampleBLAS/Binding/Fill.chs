{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.Fill
  ( fill_N_scalar
  , fill_HW_eye
  , fill_N_random
  ) where

#include <example-blas/fill.h>

import Foreign.Ptr
import Foreign.C

-- 
{#fun fill_N_scalar
  { castPtr `Ptr Float'
  ,             `Float'
  , `Int'} -> `()' #}

{#fun fill_HW_eye
  { castPtr `Ptr Float'
  , `Int'} -> `()' #}

  
{#fun fill_N_random
  { castPtr `Ptr Float'
  , `Int'} -> `()' #}