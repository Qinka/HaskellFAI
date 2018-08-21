{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.ComputerVision
  ( color_background_mask
  , color_background_mask_rgb_rang
  , color_rgb_to_hsv
  , color_hsv_to_rgb
  ) where

#include <example-blas/computer_vision.h>

import Foreign.Ptr
import Foreign.C

{#fun color_background_mask
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  , `Int'
  } -> `()' #}
  
{#fun color_background_mask_rgb_rang
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  , `Float'
  , `Float'
  , `Float'
  , `Float'
  , `Float'
  , `Float'
  } -> `()' #}


{#fun color_rgb_to_hsv
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  } -> `()' #}


{#fun color_hsv_to_rgb
  { castPtr `Ptr Float'
  , castPtr `Ptr Float'
  , `Int'
  } -> `()' #}