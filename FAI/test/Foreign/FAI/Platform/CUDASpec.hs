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

peekBuffer :: (Storable b, Pf Host a ~ b) => Buffer Host a -> IO [b]
peekBuffer (Buffer fp len) = withForeignPtr fp $ \p -> peekArray len p

peekBufferA :: (Storable b, Pf Host a ~ b, Pf CUDA a ~ b) => Buffer CUDA a -> Accelerate CUDA [b]
peekBufferA b = do
  hostBuffer <- dupBufferD True b
  liftIO $ peekBuffer hostBuffer


spec :: Spec
spec = do
  describe "Test CUDA" $ do
    it "copy and same" $ do
      let acc = accelerate cc $ do
            let arr1 = [1..100] :: [Float]
                b1  = unsafeToHostBuffer arr1 :: Buffer Host Float
            liftIO $ print b1
            b2 <- dupBuffer True b1 :: Accelerate CUDA (Buffer CUDA Float)
            liftIO $ print b2
            b3 <- dupBufferD True b2 :: Accelerate CUDA (Buffer Host Float)
            liftIO $ print b3
            let arr2 = unsafePeekHostBuffer b3
            return (arr1, arr2)
      (arr1, arr2) <- acc
      arr1 `shouldBe` arr2

cc = Context nullPtr

#else
spec = describe "Skip CUDA Test" $ it "Do nothing" $ putStrLn "Skip."
-- ENABLE_CUDA
#endif
