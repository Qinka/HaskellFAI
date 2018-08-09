{-# LANGUAGE LambdaCase #-}

module Accelerate.ExampleBLAS.LoadImg
  ( readImageToBufferFloat
  , writeImageFromBufferFloat
  , ImgFormat(..)
  ) where

import           Codec.Picture
import           Data.Vector.Storable      (Vector, fromList, toList,
                                            unsafeToForeignPtr0)
import           Foreign.FAI
import           Foreign.FAI.Platform.Host
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Storable


data ImgFormat = Y8 | Y16 | Y32 | YF |  RGB8 | RGB16 | RGBF | RGBA8
    deriving (Show)

readImageToBufferFloat :: FAI p
                       => FilePath
                       -> Accelerate p (Buffer (Int, Int, Int) Host Float, ImgFormat)
readImageToBufferFloat fp = Accelerate $ \cc -> do
  (fs, h, w, c, f) <- readImage fp >>= \case
    Left err -> error err
    Right di -> fromDynImg di
  buf <- fst <$> newBufferIO (h, w, c) nullHostContext
  withBuffer buf $ \p -> pokeArray p fs
  return ((buf,f), cc)
  where trans :: Storable a => (a -> b) -> Vector a -> [b]
        trans f = map f . toList
        fromDynImg :: DynamicImage -> IO ([Float], Int, Int, Int, ImgFormat)
        fromDynImg (ImageY8    (Image w h d)) = return (trans fromIntegral d, h, w, 1, Y8)
        fromDynImg (ImageY16   (Image w h d)) = return (trans fromIntegral d, h, w, 1, Y16)
        fromDynImg (ImageRGB8  (Image w h d)) = return (trans fromIntegral d, h, w, 3, RGB8)
        fromDynImg (ImageRGB16 (Image w h d)) = return (trans fromIntegral d, h, w, 3, RGB16)
        fromDynImg (ImageRGBA8 (Image w h d)) = return (trans fromIntegral d, h, w, 4, RGBA8)
        fromDynImg (ImageYF    (Image w h d)) = return (trans           id d, h, w, 1, YF)
        fromDynImg (ImageRGBF  (Image w h d)) = return (trans           id d, h, w, 3, RGBF)
        fromDynImg _ = error "Not support!"

writeImageFromBufferFloat :: FAI p
                          => FilePath
                          -> ImgFormat
                          -> Buffer (Int, Int, Int) Host Float
                          -> Accelerate p ()
writeImageFromBufferFloat fp f (Buffer p sh) = Accelerate $ \cc -> do
  di <- toDynImg f p sh
  savePngImage fp di
  return ((), cc)
  where trans :: (Storable a, Storable b) => (a -> b) -> ForeignPtr a -> Int -> IO (Vector b)
        trans f fp len = fromList . map f <$> withForeignPtr fp (peekArray len)
        toDynImg :: ImgFormat -> ForeignPtr Float -> (Int, Int, Int) -> IO DynamicImage
        toDynImg Y8    fp (h, w, c) = ImageY8    . Image w h <$> trans round fp (h * w * c)
        toDynImg Y16   fp (h, w, c) = ImageY16   . Image w h <$> trans round fp (h * w * c)
        toDynImg RGB8  fp (h, w, c) = ImageRGB8  . Image w h <$> trans round fp (h * w * c)
        toDynImg RGB16 fp (h, w, c) = ImageRGB16 . Image w h <$> trans round fp (h * w * c)
        toDynImg RGBA8 fp (h, w, c) = ImageRGBA8 . Image w h <$> trans round fp (h * w * c)
        toDynImg YF    fp (h, w, c) = ImageYF    . Image w h <$> trans    id fp (h * w * c)
        toDynImg RGBF  fp (h, w, c) = ImageRGBF  . Image w h <$> trans    id fp (h * w * c)
        toDynImg _ _ _ = error "Not support!"
