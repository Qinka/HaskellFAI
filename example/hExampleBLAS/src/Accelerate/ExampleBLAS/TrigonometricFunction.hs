{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Accelerate.ExampleBLAS.TrigonometricFunction where

import           Accelerate.ExampleBLAS.Binding.GenTH
import           Accelerate.ExampleBLAS.Binding.TrigonometricFunction
import           Foreign.FAI
import           Foreign.Ptr

mkBindingFAI 'forward_N_cos              True []
mkBindingFAI 'backward_N_cos_A           True []
mkBindingFAI 'forward_N_sin              True []
mkBindingFAI 'backward_N_sin_A           True []
mkBindingFAI 'forward_N_tan              True []
mkBindingFAI 'backward_N_tan_A           True []
mkBindingFAI 'forward_N_atan             True []
mkBindingFAI 'backward_N_atan_A          True []
mkBindingFAI 'forward_N_acos             True []
mkBindingFAI 'backward_N_acos_A          True []
mkBindingFAI 'forward_N_asin             True []
mkBindingFAI 'backward_N_asin_A          True []
