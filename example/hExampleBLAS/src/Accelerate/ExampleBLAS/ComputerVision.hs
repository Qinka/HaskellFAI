{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Accelerate.ExampleBLAS.ComputerVision where

import           Accelerate.ExampleBLAS.Binding.ComputerVision
import           Control.Monad
import           Foreign.FAI
import           Foreign.Ptr
import           Foreign.Storable

cvColorBackgroundMask :: (FAI p, Shape sh, Storable b, b ~ Pf p Float)
                      => Buffer (Z  :. Int) p Float -- ^ color
                      -> Buffer (sh :. Int) p Float -- ^ image
                      -> Accelerate p (Buffer (sh :. Int) p Float)
cvColorBackgroundMask color image = do
  when (colorSh' /= colorSh) $ error $ "not same shape " ++ show colorSh ++ " " ++ show colorSh'
  maskBuf <- newBuffer $ bufShape image
  liftIO $ withBuffer color $ \pColor ->
    withBuffer image $ \pImage ->
    withBuffer maskBuf $ \pMask ->
    color_background_mask (castPtr pMask) (castPtr pImage) (castPtr pColor)
                          (shLen pixelSh) (shLen colorSh)
  return maskBuf
  where (pixelSh :. colorSh ) = bufShape image
        (Z       :. colorSh') = bufShape color

cvColorBackgroundMaskRGBRang :: (FAI p, Shape sh, Storable b, b ~ Pf p Float)
                             => Buffer (sh :. Int) p Float -- ^ image
                             -> (Float, Float, Float)
                             -> (Float, Float, Float)
                             -> Accelerate p (Buffer (sh :. Int) p Float)
cvColorBackgroundMaskRGBRang image (aR, aG, aB) (bR, bG, bB) = do
  when (shLen colorSh /= 3) $ error "Color, not RGB"
  maskBuf <- newBuffer $ bufShape image
  liftIO $ withBuffer image $ \pImage ->
    withBuffer maskBuf $ \pMask ->
    color_background_mask_rgb_rang (castPtr pMask) (castPtr pImage) (shLen pixelSh) aR aG aB bR bG bB
  return maskBuf
  where (pixelSh :. colorSh ) = bufShape image

cvColorRGB2HSV :: (FAI p, Shape sh, Storable b, b ~ Pf p Float)
               => Buffer (sh :. Int) p Float -- ^ image
               -> Accelerate p (Buffer (sh :. Int) p Float)
cvColorRGB2HSV image = do
  when (colorSh /= 3) $ error $ "not same shape " ++ show colorSh
  hsvBuf <- newBuffer $ bufShape image
  liftIO $ withBuffer image $ \pImage ->
    withBuffer hsvBuf $ \pHSV ->
    color_rgb_to_hsv (castPtr pHSV) (castPtr pImage) (shLen pixelSh)
  return hsvBuf
  where (pixelSh :. colorSh ) = bufShape image


cvColorHSV2RGB :: (FAI p, Shape sh, Storable b, b ~ Pf p Float)
               => Buffer (sh :. Int) p Float -- ^ image
               -> Accelerate p (Buffer (sh :. Int) p Float)
cvColorHSV2RGB image = do
  when (colorSh /= 3) $ error $ "not same shape " ++ show colorSh
  rgbBuf <- newBuffer $ bufShape image
  liftIO $ withBuffer image $ \pImage ->
    withBuffer rgbBuf $ \pRGB ->
    color_hsv_to_rgb (castPtr pRGB) (castPtr pImage) (shLen pixelSh)
  return rgbBuf
  where (pixelSh :. colorSh ) = bufShape image