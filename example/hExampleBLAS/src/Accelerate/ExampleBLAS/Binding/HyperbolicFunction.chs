{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.HyperbolicFunction
  ( forward_N_cosh
  , backward_N_cosh_A
  , forward_N_sinh
  , backward_N_sinh_A
  , forward_N_tanh
  , backward_N_tanh_A
  , forward_N_atanh
  , backward_N_atanh_A
  , forward_N_acosh
  , backward_N_acosh_A
  , forward_N_asinh
  , backward_N_asinh_A
  ) where

#include <example-blas/hyperbolic_function.h>

import Foreign.Ptr
import Foreign.C

{# fun forward_N_cosh
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_cosh_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_sinh
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_sinh_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_tanh
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_tanh_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_atanh
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_atanh_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_acosh
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_acosh_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun forward_N_asinh
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}
{# fun backward_N_asinh_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}