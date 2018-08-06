module Accelerate.ExampleBLAS.ExampleBLAS where

import           Accelerate.ExampleBLAS.Binding.ExampleBLAS

exampleBLASVersionStr :: String
exampleBLASVersionStr = example_blas_version_str

exampleBLASVersion :: (Int, Int, Int, Int)
exampleBLASVersion = ( example_blas_version_major
                     , example_blas_version_minor
                     , example_blas_version_patch
                     , example_blas_version_diff
                     )

exampleBLASBackend   :: String
exampleBLASBackend   = example_blas_backend_type_str
exampleBLASBackendId :: Int
exampleBLASBackendId = example_blas_backend_type_id
