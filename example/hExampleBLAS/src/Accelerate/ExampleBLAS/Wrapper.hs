{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Accelerate.ExampleBLAS.Wrapper
    ( bestContextIO
    , bestContext
    , Wrappered(..)
    , module X

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
import           Control.Monad.Catch                          as X
import           Control.Monad.Logger                         as X
import           Foreign.Storable                             as X
import           System.Environment                           as X


#ifdef __CUDA_ENABLE
import           Accelerate.ExampleBLAS.LoadImg.CUDA          as X
import           Foreign.FAI.Platform.CUDA                    as X
import           Foreign.FAI.Platform.CUDA.Debug              as X
#endif

import           Accelerate.ExampleBLAS.TH                    (mkBestContext)

mkBestContext


class (LoadSaveImg p, FAI p, Storable (Pf p Float)) => Wrappered p

instance (LoadSaveImg p, FAI p, Storable (Pf p Float)) => Wrappered p
