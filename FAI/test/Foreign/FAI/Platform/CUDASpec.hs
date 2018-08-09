{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Foreign.FAI.Platform.CUDASpec
  ( spec
  ) where

import           Test.Hspec

#ifdef ENABLE_CUDA

import           Foreign.FAI
import           Foreign.FAI.Platform.CUDA
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Platform.Host.Debug
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe

spec :: Spec
spec = do
  describe "Test CUDA" $ do
    it "copy and same" $ do
      let acc = accelerate cc $ do
            let arr1 = [1..100] :: [Float]
                b1  = unsafeToHostBuffer arr1 :: Buffer Int Host Float
            liftIO $ print b1
            b2 <- dupBuffer True b1 :: Accelerate CUDA (Buffer Int CUDA Float)
            liftIO $ print b2
            b3 <- dupBufferD True b2 :: Accelerate CUDA (Buffer Int Host Float)
            liftIO $ print b3
            let arr2 = unsafePeekHostBuffer b3
            return (arr1, arr2)
      (arr1, arr2) <- acc
      arr2 `shouldBe` arr1

cc = unsafePerformIO nullCUDAContext

#else
spec = describe "Skip CUDA Test" $ it "Do nothing" $ putStrLn "Skip."
-- ENABLE_CUDA
#endif
