{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Accelerate.ExampleBLAS.LoadImg
  ( readImageToBufferFloatHost
  , writeImageFromBufferFloatHost
  , loadImageToBufferFloat
  , saveImageFromBufferFloat
  , ImgFormat(..)
  , LoadSaveImg(..)
  ) where

import           Codec.Picture
import           Control.Monad.Logger
import qualified Data.Text                 as T
import           Data.Vector.Storable      (Vector, fromList, toList,
                                            unsafeToForeignPtr0)
import           Foreign.FAI
import           Foreign.FAI.Platform.Host
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Storable

data ImgFormat = Y8 | Y16 | Y32 | YF |  RGB8 | RGB16 | RGBF | RGBA8
    deriving (Eq, Show)

loadImageToBufferFloat :: (LoadSaveImg p, Storable b, b ~ Pf p Float)
                       => Bool -- ^ to [0,1]
                       -> FilePath -- ^ Image
                       -> Accelerate p (Buffer (Int, Int, Int) p Float, ImgFormat)
loadImageToBufferFloat is fp = do
  (b, f) <- readImageToBufferFloat is fp
  $(logInfo) $ mconcat ["load image from ", T.pack fp]
  $(logInfo) $ mconcat ["image in format of ", T.pack (show f), " with shape", T.pack (show $ bufShape b)]
  return (b, f)

saveImageFromBufferFloat :: (LoadSaveImg p, Storable b, b ~ Pf p Float)
                         => Bool
                         -> FilePath
                         -> ImgFormat
                         -> Buffer (Int, Int, Int) p Float
                         -> Accelerate p ()
saveImageFromBufferFloat is fp f b = do
  $(logInfo) $ mconcat ["save image to ", T.pack fp]
  $(logInfo) $ mconcat ["image in format of ", T.pack (show f), " with shape", T.pack (show $ bufShape b)]
  writeImageFromBufferFloat is fp f b

class FAI p => LoadSaveImg p where
  readImageToBufferFloat :: (Storable b, b ~ Pf p Float) => Bool -> FilePath -> Accelerate p (Buffer (Int, Int, Int) p Float, ImgFormat)
  writeImageFromBufferFloat :: (Storable b, b ~ Pf p Float) => Bool -> FilePath -> ImgFormat -> Buffer (Int, Int, Int) p Float -> Accelerate p ()

instance LoadSaveImg Host where
  readImageToBufferFloat = readImageToBufferFloatHost
  writeImageFromBufferFloat  = writeImageFromBufferFloatHost

toColorFloat :: (Integral a, Bounded a) => a -> Float
toColorFloat x = fromIntegral x / mf x maxBound
    where mf :: (Integral a, Bounded a) => a -> a -> Float
          mf _ = fromIntegral

fromColorFloat :: (Integral a, Bounded a) => Float -> a
fromColorFloat = mf maxBound
    where mf :: (Integral a, Bounded a) => a -> Float -> a
          mf m f = round $ f * fromIntegral m

readImageToBufferFloatHost :: FAI p
                           => Bool -- ^ whether transform from integer to (0, 1) :: [Float]
                           -> FilePath
                           -> Accelerate p (Buffer (Int, Int, Int) Host Float, ImgFormat)
readImageToBufferFloatHost is fp = Accelerate $ \cc -> do
  (fs, h, w, c, f) <- readImage fp >>= \case
    Left err -> error err
    Right di -> fromDynImg di
  buf <- fst <$> newBufferIO (h, w, c) nullHostContext
  withBuffer buf $ \p -> pokeArray p fs
  return ((buf,f), cc)
  where trans :: Storable a => (a -> b) -> Vector a -> [b]
        trans f = map f . toList
        fI :: (Integral a, Bounded a) => a -> Float
        fI = if is then toColorFloat else fromIntegral
        fromDynImg :: DynamicImage -> IO ([Float], Int, Int, Int, ImgFormat)
        fromDynImg (ImageY8    (Image w h d)) = return (trans fI d, h, w, 1, Y8)
        fromDynImg (ImageY16   (Image w h d)) = return (trans fI d, h, w, 1, Y16)
        fromDynImg (ImageRGB8  (Image w h d)) = return (trans fI d, h, w, 3, RGB8)
        fromDynImg (ImageRGB16 (Image w h d)) = return (trans fI d, h, w, 3, RGB16)
        fromDynImg (ImageRGBA8 (Image w h d)) = return (trans fI d, h, w, 4, RGBA8)
        fromDynImg (ImageYF    (Image w h d)) = return (trans id d, h, w, 1, YF)
        fromDynImg (ImageRGBF  (Image w h d)) = return (trans id d, h, w, 3, RGBF)
        fromDynImg _ = error "Not support!"

writeImageFromBufferFloatHost :: FAI p
                              => Bool  -- ^ whether transform  to integer from (0, 1) :: [Float]
                              -> FilePath
                              -> ImgFormat
                              -> Buffer (Int, Int, Int) Host Float
                              -> Accelerate p ()
writeImageFromBufferFloatHost is fp f (Buffer p sh) = Accelerate $ \cc -> do
  di <- toDynImg f p sh
  savePngImage fp di
  return ((), cc)
  where trans :: (Storable a, Storable b) => (a -> b) -> ForeignPtr a -> Int -> IO (Vector b)
        trans f fp len = fromList . map f <$> withForeignPtr fp (peekArray len)
        rI :: (Integral a, Bounded a) => Float -> a
        rI = if is then fromColorFloat else round
        toDynImg :: ImgFormat -> ForeignPtr Float -> (Int, Int, Int) -> IO DynamicImage
        toDynImg Y8    fp (h, w, c) = ImageY8    . Image w h <$> trans rI fp (h * w * c)
        toDynImg Y16   fp (h, w, c) = ImageY16   . Image w h <$> trans rI fp (h * w * c)
        toDynImg RGB8  fp (h, w, c) = ImageRGB8  . Image w h <$> trans rI fp (h * w * c)
        toDynImg RGB16 fp (h, w, c) = ImageRGB16 . Image w h <$> trans rI fp (h * w * c)
        toDynImg RGBA8 fp (h, w, c) = ImageRGBA8 . Image w h <$> trans rI fp (h * w * c)
        toDynImg YF    fp (h, w, c) = ImageYF    . Image w h <$> trans id fp (h * w * c)
        toDynImg RGBF  fp (h, w, c) = ImageRGBF  . Image w h <$> trans id fp (h * w * c)
        toDynImg _ _ _ = error "Not support!"
