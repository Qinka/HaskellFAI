{-# LANGUAGE TypeFamilies #-}

module Foreign.FAI.Platform.HostSpec
  ( spec
  ) where


import Foreign.FAI.Types
import Foreign.FAI.Platform.Host
import           Test.Hspec
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable

peekBuffer :: (Storable b, Pf Host a ~ b) => Buffer Host a -> IO [b]
peekBuffer (Buffer fp len) = withForeignPtr fp $ \p -> peek undefined p len
  where peek :: (Storable b) => b -> Ptr b-> Int -> IO [b]
        peek u ptr i = peekArray (i `div` sizeOf u) ptr

peekBufferA :: (Storable b, Pf Host a ~ b) => Buffer Host a -> Accelerate Host [b]
peekBufferA b = liftIO $ peekBuffer b


spec :: Spec
spec = do
  describe "Allocate buffer" $ do
    it "newBuffer" $ do
      let acc = accelerate cc $ do
            b <- newBuffer 20 :: Accelerate Host (Buffer Host Float)
            liftIO $ print b
            peekBufferA b >>= liftIO. print
            return ()
      acc `shouldReturn` ()
    it "dupBuffer" $ do
      let acc = accelerate cc $ do
            b1 <- newBuffer 20 :: Accelerate Host (Buffer Host Float)
            peekBufferA b1 >>= liftIO . print
            b2 <- dupBuffer False b1
            peekBufferA b2 >>= liftIO . print
            return ()
      acc `shouldReturn` ()
  describe "Test host" $ do
    it "copy and same" $ do
      let acc = accelerate cc $ do
            let arr1 = [1..100] :: [Float]
                b1  = bufFromList arr1 :: Buffer Host Float
            b2 <- dupBuffer True b1 :: Accelerate Host (Buffer Host Float)
            let arr2 = bufToList b2
            return (arr1, arr2)
      (arr1, arr2) <- acc 
      arr1 `shouldBe` arr2
    
cc = Context nullPtr
