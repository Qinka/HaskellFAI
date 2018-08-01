{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.ActivationFunction where

#include <example-blas/activation_function.h>

import Foreign.Ptr
import Foreign.C


-- | forward for sigmoid \(B = sigmoid(A)\)
{#fun forward_N_sigmoid
  { castPtr `Ptr Float' -- ^ \(B\)
  , castPtr `Ptr Float' -- ^ \(A\)
  , `Int'               -- ^ \(n\)
  } -> `()' #}

{#fun backward_N_sigmoid_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun forward_N_ReLU
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}

{#fun backward_N_ReLU_A
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'} -> `()' #}