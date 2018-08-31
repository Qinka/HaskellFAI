{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Foreign.FAI.Platform.CUDASpec
  ( spec
  ) where

import           Foreign.FAI
import           Foreign.FAI.Platform.CUDA
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Platform.Host.Debug
import           Foreign.ForeignPtr
import           Test.Hspec

data Ten a = Ten !(ForeignPtr Float) Int
  deriving(Eq, Show)

instance Buffer (Ten CUDA) where
  type BufferPlatform (Ten CUDA) = CUDA
  type BufferShape    (Ten CUDA) = Int
  type BufferType     (Ten CUDA) = Float
  getBufferPtr   (Ten fp _) = fp
  setBufferPtr   (Ten _  l) = (`Ten` l)
  getBufferShape (Ten _  l) = l
  setBufferShape (Ten fp _) = Ten fp
  makeBuffer                = Ten


instance Buffer (Ten Host) where
  type BufferPlatform (Ten Host) = Host
  type BufferShape    (Ten Host) = Int
  type BufferType     (Ten Host) = Float
  getBufferPtr   (Ten fp _) = fp
  setBufferPtr   (Ten _  l) = (`Ten` l)
  getBufferShape (Ten _  l) = l
  setBufferShape (Ten fp _) = Ten fp
  makeBuffer                = Ten


spec :: Spec
spec = do
  describe "Test CUDA" $ do
    it "copy and same" $ do
      let acc = accelerate nullCUDAContext $ do
            let arr1 = [1..100] :: [Float]
                b1  = unsafeToHostBuffer arr1 :: Ten Host
            liftIO $ print b1
            b2 <- dupBuffer True b1 :: Accelerate CUDA (Ten CUDA)
            liftIO $ print b2
            b3 <- dupBufferD True b2 :: Accelerate CUDA (Ten Host)
            liftIO $ print b3
            let arr2 = unsafePeekHostBuffer b3
            return (arr1, arr2)
      (arr1, arr2) <- acc
      arr2 `shouldBe` arr1
