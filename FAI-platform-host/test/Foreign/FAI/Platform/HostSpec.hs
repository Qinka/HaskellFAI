{-# LANGUAGE TypeFamilies #-}

module Foreign.FAI.Platform.HostSpec
  ( spec
  ) where

import           Foreign.FAI
import           Foreign.FAI.Platform.Host
import           Foreign.FAI.Platform.Host.Debug
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe
import           Test.Hspec

data Ten = Ten !(ForeignPtr Float) Int
  deriving(Eq, Show)

instance Buffer Ten where
  type BufferPlatform Ten = Host
  type BufferShape    Ten = Int
  type BufferType     Ten = Float
  getBufferPtr   (Ten fp _) = fp
  setBufferPtr   (Ten _  l) = (`Ten` l)
  getBufferShape (Ten _  l) = l
  setBufferShape (Ten fp _) = Ten fp
  makeBuffer                = Ten

peekBufferA :: Ten -> Accelerate Host [Float]
peekBufferA = liftIO . peekHostBuffer

spec :: Spec
spec = do
  describe "Allocate buffer" $ do
    it "newBuffer" $ do
      let acc = accelerate nullHostContext $ do
            b <- newBuffer 20 :: Accelerate Host Ten
            liftIO $ print b
            peekBufferA b >>= liftIO . print
            return ()
      acc `shouldReturn` ()
    it "dupBuffer" $ do
      let acc = accelerate nullHostContext $ do
            b1 <- newBuffer 20 :: Accelerate Host Ten
            peekBufferA b1 >>= liftIO . print
            b2 <- dupBuffer False b1
            peekBufferA b2 >>= liftIO . print
            return ()
      acc `shouldReturn` ()
  describe "Debug" $ do
    it "peek and poke" $ do
      ls <- accelerate nullHostContext $ do
        bf <- newBuffer 10 :: Accelerate Host Ten
        liftIO (peekHostBuffer bf) >>= liftIO . print
        liftIO $ pokeHostBuffer bf [0..9]
        ls <- liftIO (peekHostBuffer bf)
        liftIO $ print ls
        return ls
      ls `shouldBe` [0..9]
  describe "Test host" $ do
    it "copy and same" $ do
      let acc = accelerate nullHostContext $ do
            let arr1 = [1..100] :: [Float]
                b1  = unsafeToHostBuffer arr1 :: Ten
            b2 <- dupBuffer True b1 :: Accelerate Host Ten
            let arr2 = unsafePeekHostBuffer b2
            return (arr1, arr2)
      (arr1, arr2) <- acc
      arr2 `shouldBe` arr1
