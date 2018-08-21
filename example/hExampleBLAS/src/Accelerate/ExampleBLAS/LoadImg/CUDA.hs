module Accelerate.ExampleBLAS.LoadImg.CUDA
  (
  ) where

import           Accelerate.ExampleBLAS.LoadImg

import           Foreign.FAI
import           Foreign.FAI.Platform.CUDA
import           Foreign.FAI.Platform.Host

instance LoadSaveImg CUDA where
  readImageToBufferFloat is fp = do
    host <- readImageToBufferFloatHost is fp
    dupBuffer True host
  writeImageFromBufferFloat is fp f cu = do
    host <- liftIO $ dupBufferIO True cu nullHostContext
    writeImageFromBufferFloatHost is fp f host
