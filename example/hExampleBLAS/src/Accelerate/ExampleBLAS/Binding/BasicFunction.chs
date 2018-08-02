{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.BasicFunction
  ( forward_N_add
  , backward_N_add_A
  , backward_N_add_B
  , forward_N_sub
  , backward_N_sub_A
  , backward_N_sub_B
  , forward_N_dot_prd
  , backward_N_dot_prd_A
  , backward_N_dot_prd_B
  , forward_N_dot_div
  , backward_N_dot_div_A
  , backward_N_dot_div_B
  , forward_N_scale
  , backward_N_scale_A
  , backward_N_scale_s
  , forward_N_abs
  , backward_N_abs_A
  , forward_N_sign
  , backward_N_sign_A
  , forward_N_exp
  , backward_N_exp_A
  , forward_N_expm1
  , backward_N_expm1_A
  , forward_N_log
  , backward_N_log_A
  , forward_N_log1p
  , backward_N_log1p_A
  , forward_N_sqrt
  , backward_N_sqrt_A
  , forward_N_pow
  , backward_N_pow_A
  , backward_N_pow_w
  , forward_N_ceil
  , backward_N_ceil_A
  , forward_N_floor
  , backward_N_floor_A
  , forward_N_erf
  , backward_N_erf_A
  , forward_N_erfc
  , backward_N_erfc_A
  ) where

#include <example-blas/basic_function.h>

import Foreign.Ptr
import Foreign.C

-- Add
{#fun forward_N_add
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_add_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_add_B
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

  -- Sub
{#fun forward_N_sub
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_sub_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_sub_B
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

-- Prd
{#fun forward_N_dot_prd
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_dot_prd_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_dot_prd_B
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

-- Div
{#fun forward_N_dot_div
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_dot_div_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_dot_div_B
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  ,             `Float'
  , `Int'} -> `()' #}

-- scale
{#fun forward_N_scale
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  ,             `Float'
  , `Int'} -> `()' #}

{#fun backward_N_scale_A
  { castPtr `Ptr Float'
  ,             `Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_scale_s
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

-- abs
{# fun forward_N_abs
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_abs_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- sign
{# fun forward_N_sign
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_sign_A
  { castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- exp
{# fun forward_N_exp
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_exp_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- expm1
{# fun forward_N_expm1
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_expm1_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- log
{# fun forward_N_log
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_log_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- log1p
{# fun forward_N_log1p
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_log1p_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- sqrt
{# fun forward_N_sqrt
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_sqrt_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- pow
{# fun forward_N_pow
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  ,             `Float'
  , `Int'} -> `()' #}

{# fun backward_N_pow_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  ,             `Float'
  , `Int'} -> `()' #}

{# fun backward_N_pow_w
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  ,             `Float'
  , `Int'} -> `()' #}

-- ceil
{# fun forward_N_ceil
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_ceil_A
  { castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- floor
{# fun forward_N_floor
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_floor_A
  { castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- erf
{# fun forward_N_erf
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_erf_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

-- erfc
{# fun forward_N_erfc
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}

{# fun backward_N_erfc_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int' } -> `()' #}