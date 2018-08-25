{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Accelerate.ExampleBLAS.ActivationFunction where

import           Accelerate.ExampleBLAS.Binding.ActivationFunction
import           Accelerate.ExampleBLAS.TH
import           Foreign.FAI
import           Foreign.Ptr

mkBindingFAI 'forward_N_sigmoid             True  []
mkBindingFAI 'backward_N_sigmoid_A          True  []
mkBindingFAI 'forward_N_ReLU                True  []
mkBindingFAI 'backward_N_ReLU_A             True  []
