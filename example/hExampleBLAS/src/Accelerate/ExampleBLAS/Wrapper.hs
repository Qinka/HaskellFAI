{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Accelerate.ExampleBLAS.Wrapper
    ( bestContextIO
    , MayContext(..)
    , Wrappered(..)
    , module X
    , CUDA(..)
    , nullCUDAContextIO
    ) where

import           Foreign.FAI                                  as X
import           Foreign.FAI.Platform.Host                    as X
import           Foreign.FAI.Platform.Host.Debug              as X

import           Accelerate.ExampleBLAS.ActivationFunction    as X
import           Accelerate.ExampleBLAS.BasicFunction         as X
import           Accelerate.ExampleBLAS.ComputerVision        as X
import           Accelerate.ExampleBLAS.ExampleBLAS           as X
import           Accelerate.ExampleBLAS.Fill                  as X
import           Accelerate.ExampleBLAS.HyperbolicFunction    as X
import           Accelerate.ExampleBLAS.LoadImg               as X
import           Accelerate.ExampleBLAS.MatrixFunction        as X
import           Accelerate.ExampleBLAS.TrigonometricFunction as X

import           Control.Monad                                as X
import           Foreign.Storable                             as X
import           System.Environment                           as X


#ifdef __CUDA_ENABLE
import           Accelerate.ExampleBLAS.LoadImg.CUDA          as X
import           Foreign.FAI.Platform.CUDA                    as X
import           Foreign.FAI.Platform.CUDA.Debug              as X
#else
data CUDA = CUDA
nullCUDAContextIO = undefined
#endif

data MayContext = H' (Context Host)
                | C' (Context CUDA)


--bestContextIO :: IO MayContext
bestContextIO = case exampleBLASBackend of
#ifdef __CUDA_ENABLE
  "OACC_DRVPTR" -> C' <$> nullCUDAContextIO
#endif
  "OMP_TARGET"  -> H' <$> nullHostContextIO
  "OMP_ONLY"    -> H' <$> nullHostContextIO
  "OACC_ONLY"   -> H' <$> nullHostContextIO
  "NO_ACC"      -> H' <$> nullHostContextIO
  _             -> error "Unsupported backend."


class (LoadSaveImg p, FAI p, Storable (Pf p Float)) => Wrappered p

instance (LoadSaveImg p, FAI p, Storable (Pf p Float)) => Wrappered p
