{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.TrigonometricFunction
  ( forward_N_cos
  , backward_N_cos_A
  , forward_N_sin
  , backward_N_sin_A
  , forward_N_tan
  , backward_N_tan_A
  , forward_N_atan
  , backward_N_atan_A
  , forward_N_acos
  , backward_N_acos_A
  , forward_N_asin
  , backward_N_asin_A
  ) where

#include <example-blas/trigonometric_function.h>

import Foreign.Ptr
import Foreign.C

{# fun forward_N_cos
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_cos_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_sin
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_sin_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_tan
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_tan_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_atan
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_atan_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_acos
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_acos_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_asin
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_asin_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}