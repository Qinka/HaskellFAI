{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Accelerate.ExampleBLAS.BasicFunction where

import           Accelerate.ExampleBLAS.Binding.BasicFunction
import           Accelerate.ExampleBLAS.Binding.GenTH
import           Foreign.FAI

mkBindingFAI 'forward_N_add                     True  []
mkBindingFAI 'backward_N_add_A                  True  []
mkBindingFAI 'backward_N_add_B                  True  []
mkBindingFAI 'forward_N_sub                     True  []
mkBindingFAI 'backward_N_sub_A                  True  []
mkBindingFAI 'backward_N_sub_B                  True  []
mkBindingFAI 'forward_N_dot_prd                 True  []
mkBindingFAI 'backward_N_dot_prd_A              True  []
mkBindingFAI 'backward_N_dot_prd_B              True  []
mkBindingFAI 'forward_N_dot_div                 True  []
mkBindingFAI 'backward_N_dot_div_A              True  []
mkBindingFAI 'backward_N_dot_div_B              True  []
mkBindingFAI 'forward_N_scale                   True  [True,False]
mkBindingFAI 'backward_N_scale_A                True  [True,False]
mkBindingFAI 'backward_N_scale_s                False []
mkBindingFAI 'forward_N_abs                     True  []
mkBindingFAI 'backward_N_abs_A                  True  []
mkBindingFAI 'forward_N_sign                    True  []
mkBindingFAI 'backward_N_sign_A                 True  []
mkBindingFAI 'forward_N_exp                     True  []
mkBindingFAI 'backward_N_exp_A                  True  []
mkBindingFAI 'forward_N_expm1                   True  []
mkBindingFAI 'backward_N_expm1_A                True  []
mkBindingFAI 'forward_N_log                     True  []
mkBindingFAI 'backward_N_log_A                  True  []
mkBindingFAI 'forward_N_log1p                   True  []
mkBindingFAI 'backward_N_log1p_A                True  []
mkBindingFAI 'forward_N_sqrt                    True  []
mkBindingFAI 'backward_N_sqrt_A                 True  []
mkBindingFAI 'forward_N_pow                     True  [True, False]
mkBindingFAI 'backward_N_pow_A                  True  [True, True, True, False]
mkBindingFAI 'backward_N_pow_w                  False [True, True, True, False]
mkBindingFAI 'forward_N_ceil                    True  []
mkBindingFAI 'backward_N_ceil_A                 True  []
mkBindingFAI 'forward_N_floor                   True  []
mkBindingFAI 'backward_N_floor_A                True  []
mkBindingFAI 'forward_N_erf                     True  []
mkBindingFAI 'backward_N_erf_A                  True  []
mkBindingFAI 'forward_N_erfc                    True  []
mkBindingFAI 'backward_N_erfc_A                 True  []
