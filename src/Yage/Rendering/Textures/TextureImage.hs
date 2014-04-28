{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Yage.Rendering.Textures.TextureImage where

import              Yage.Prelude

import              Data.Data
import              Graphics.GLUtil.Textures            (IsPixelData(..))
import              Graphics.GLUtil.TypeMapping         (HasGLType, glType)
import qualified    Graphics.Rendering.OpenGL           as GL

import              Codec.Picture
import              JuicySRGB
import              Linear                              (V2(..))

import              Yage.Rendering.Textures.Instances

-- | A tag for opengl-related instances
newtype GLTexture pixel = GLTexture { unGLTexture :: Image pixel }

deriving instance Typeable GLTexture
deriving instance (TyPixel pixel) => Data (GLTexture pixel)

data TextureImage =
       -- | A greyscale image.
       TexY8    (GLTexture Pixel8)
       -- | A greyscale HDR image
     | TexYF    (GLTexture PixelF)
       -- | An image in true color.
     | TexRGB8  (GLTexture PixelRGB8)
       -- | An image with HDR pixels
     | TexRGBF  (GLTexture PixelRGBF)
       -- | An image in true color and an alpha channel.
     | TexRGBA8 (GLTexture PixelRGBA8)
     --  An image in the colorspace used by Jpeg images.
     -- we does not support YCbrCr as an internal texture format 
     -- (converting will be supported outside the rendering core)
     -- | TexYCbCr8 (Image PixelYCbCr8)
       -- | An Image in the sRGB color space
     | TexSRGB8  (GLTexture PixelSRGB8)
     deriving ( Typeable, Data )

data TextureImageSpec = TextureImageSpec
    { texSpecDimension :: !(V2 Int)
    , texSpecPixelSpec :: !PixelSpec
    } deriving ( Show, Eq, Data, Typeable )

--type PixelType = (GL.DataType, GL.PixelFormat)
data PixelSpec = PixelSpec
    { pxSpecType        :: GL.DataType
    , pxSpecComponents  :: GL.PixelFormat
    , pxSpecGLFormat    :: GL.PixelInternalFormat
    } deriving ( Show, Eq, Data, Typeable )


-- |deriving version of texture loading. derives the PixelInternalFormat an PixelType of the image
-- loads into the currently bound texture object in the context
loadTextureImage' :: GL.TwoDimensionalTextureTarget t => 
                  t -> TextureImage -> IO ()
loadTextureImage' target textureImg = 
    loadTextureImage target textureImg ( pixelSpec textureImg ) 


-- |explicit texture loading. the types and internalFormat
-- loads into the currently bound texture object in the context
loadTextureImage :: GL.TwoDimensionalTextureTarget t => 
                 t -> TextureImage -> PixelSpec -> IO ()
loadTextureImage target textureImg spec = aux textureImg
    where
    aux (TexY8     (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexYF     (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexRGB8   (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexRGBF   (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexRGBA8  (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexSRGB8  (GLTexture (Image w h p))) = loadTex p (texSize w h)
    
    loadTex :: IsPixelData p => p -> GL.TextureSize2D -> IO () 
    loadTex p sz = texImage2D target p sz spec
    
    texSize w h = GL.TextureSize2D (fromIntegral w) (fromIntegral h)



texImage2D :: (GL.TwoDimensionalTextureTarget t, IsPixelData d) => 
            t -> d -> GL.TextureSize2D -> PixelSpec -> IO ()
texImage2D target pxdata texSize (PixelSpec dataType components internalFormat) = 
    withPixels pxdata $
        GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 . GL.PixelData components dataType


-- | derives the pixel-spec of a TextureImage
pixelSpec :: TextureImage -> PixelSpec
pixelSpec = aux
    where
    aux (TexY8     img) = PixelSpec ( pixelDataType img ) GL.Red  GL.R8
    aux (TexYF     img) = PixelSpec ( pixelDataType img ) GL.Red  GL.R32F
    aux (TexRGB8   img) = PixelSpec ( pixelDataType img ) GL.RGB  GL.RGB8
    aux (TexRGBF   img) = PixelSpec ( pixelDataType img ) GL.RGB  GL.RGB32F
    aux (TexRGBA8  img) = PixelSpec ( pixelDataType img ) GL.RGBA GL.RGBA8
    aux (TexSRGB8  img) = PixelSpec ( pixelDataType img ) GL.RGB  GL.SRGB8


pixelDataType :: forall pixel. (Pixel pixel, HasGLType (PixelBaseComponent pixel)) => GLTexture pixel -> GL.DataType
pixelDataType _ = glType ( error "pixelDataType: invalid access" :: PixelBaseComponent pixel)


textureWidth :: GLTexture pixel -> Int
textureWidth (GLTexture (Image w _ _)) = w


textureHeight :: GLTexture pixel -> Int
textureHeight (GLTexture (Image _ h _)) = h


textureImageMap :: (forall pixel. Pixel pixel => Image pixel -> a) -> TextureImage -> a
textureImageMap f = aux
    where
    aux (TexY8     (GLTexture img)) = f img
    aux (TexYF     (GLTexture img)) = f img
    aux (TexRGB8   (GLTexture img)) = f img
    aux (TexRGBF   (GLTexture img)) = f img
    aux (TexRGBA8  (GLTexture img)) = f img
    aux (TexSRGB8  (GLTexture img)) = f img


textureSpec :: TextureImage -> TextureImageSpec
textureSpec tex =
    let dimension = V2 (textureImageMap imageWidth tex) (textureImageMap imageHeight tex)
        pxSpec    = pixelSpec tex
    in TextureImageSpec dimension pxSpec


debugString :: TextureImage -> String
debugString img = format "{0}: {{1}}" [show $ toConstr img, show $ textureSpec img]

instance Show TextureImage where
    show = debugString


