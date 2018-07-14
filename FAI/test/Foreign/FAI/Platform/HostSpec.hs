{-# LANGUAGE TypeFamilies #-}

module Foreign.FAI.Platform.HostSpec
  ( spec
  ) where

import           Foreign.FAI
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Platform.Host.Debug
import           Foreign.Ptr
import           Foreign.Storable
import           Test.Hspec

peekBufferA :: (Storable b, Pf Host a ~ b) => Buffer Host a -> Accelerate Host [b]
peekBufferA = liftIO . peekHostBuffer


spec :: Spec
spec = do
  describe "Allocate buffer" $ do
    it "newBuffer" $ do
      let acc = accelerate cc $ do
            b <- newBuffer 20 :: Accelerate Host (Buffer Host Float)
            liftIO $ print b
            peekBufferA b >>= liftIO . print
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
  describe "Debug" $ do
    it "peek and poke" $ do
      ls <- accelerate cc $ do
        bf <- newBuffer 10 :: Accelerate Host (Buffer Host Float)
        liftIO (peekHostBuffer bf) >>= liftIO . print
        liftIO $ pokeHostBuffer bf [0..9]
        ls <- liftIO (peekHostBuffer bf)
        liftIO $ print ls
        return ls
      ls `shouldBe` [0..9]
  describe "Test host" $ do
    it "copy and same" $ do
      let acc = accelerate cc $ do
            let arr1 = [1..100] :: [Float]
                b1  = unsafeToHostBuffer arr1 :: Buffer Host Float
            b2 <- dupBuffer True b1 :: Accelerate Host (Buffer Host Float)
            let arr2 = unsafePeekHostBuffer b2
            return (arr1, arr2)
      (arr1, arr2) <- acc
      arr1 `shouldBe` arr2

cc :: Context Host
cc = Context nullPtr
