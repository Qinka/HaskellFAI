{-# LANGUAGE ForeignFunctionInterface #-}

module Accelerate.ExampleBLAS.Binding.ExampleBLAS
  ( example_blas_version_str
  , example_blas_version_major
  , example_blas_version_minor
  , example_blas_version_patch
  , example_blas_version_diff
  , example_blas_backend_type_str
  , example_blas_backend_type_id
  ) where

#include <example-blas/example_blas.h>

import Foreign.Ptr
import Foreign.C

{#fun pure example_blas_version_str
  {} -> `String' #}
{#fun pure example_blas_version_major
  {} -> `Int' #}
{#fun pure example_blas_version_minor
  {} -> `Int' #}
{#fun pure example_blas_version_patch
  {} -> `Int' #}
{#fun pure example_blas_version_diff
  {} -> `Int' #}

{#fun pure example_blas_backend_type_str
  {} -> `String' #}
{#fun pure example_blas_backend_type_id
  {} -> `Int' #}