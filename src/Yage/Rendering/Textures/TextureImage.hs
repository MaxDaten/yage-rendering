{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Yage.Rendering.Textures.TextureImage where

import              Yage.Prelude
import              Yage.Lens

import              Data.Data
import              Graphics.GLUtil.Textures            ( IsPixelData(..) )
import              Graphics.GLUtil.TypeMapping         ( HasGLType, glType )
import qualified    Graphics.Rendering.OpenGL           as GL

import              Codec.Picture
import              Codec.Picture.Types                 ( convertImage )
import              JuicySRGB
import              Linear                              ( V2(..) )

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



data PixelSpec = PixelSpec
    { pxSpecType        :: !GL.DataType
    , pxSpecComponents  :: !GL.PixelFormat
    , pxSpecGLFormat    :: !GL.PixelInternalFormat
    } deriving ( Show, Eq, Data, Typeable )

data TextureImageSpec = TextureImageSpec
    { _texSpecDimension :: !(V2 Int)
    , _texSpecPixelSpec :: !PixelSpec
    } deriving ( Show, Eq, Data, Typeable )

makeLenses ''TextureImageSpec


type TextureCtr p = (GLTexture p -> TextureImage)

mkTextureImg :: TextureCtr pixel -> Image pixel -> TextureImage
mkTextureImg ctr = ctr . GLTexture


fromDynamic :: DynamicImage -> Either String TextureImage
fromDynamic = aux
    where
    aux (ImageY8        img) = Right $ mkTextureImg TexY8 img
    aux (ImageYF        img) = Right $ mkTextureImg TexYF img
    aux (ImageRGB8      img) = Right $ mkTextureImg TexRGB8 img
    aux (ImageRGBF      img) = Right $ mkTextureImg TexRGBF img
    aux (ImageRGBA8     img) = Right $ mkTextureImg TexRGBA8 img
    aux (ImageYCbCr8    img) = Right $ mkTextureImg TexRGB8 (convertImage img)
    -- not supported formats
    aux (ImageYA8       _img) = Left "not supported format: YA8"
    aux (ImageY16       _img) = Left "not supported format: Y16"
    aux (ImageYA16      _img) = Left "not supported format: YA16"
    aux (ImageRGB16     _img) = Left "not supported format: RGB16"
    aux (ImageRGBA16    _img) = Left "not supported format: RGBA16"
    aux (ImageCMYK8     _img) = Left "not supported format: CMYK8"
    aux (ImageCMYK16    _img) = Left "not supported format: CMYK16"

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


textureDimension :: Getter TextureImage (V2 Int)
textureDimension = textureImageSpec.texSpecDimension


textureImageMap :: (forall pixel. Pixel pixel => Image pixel -> a) -> TextureImage -> a
textureImageMap f = aux
    where
    aux (TexY8     (GLTexture img)) = f img
    aux (TexYF     (GLTexture img)) = f img
    aux (TexRGB8   (GLTexture img)) = f img
    aux (TexRGBF   (GLTexture img)) = f img
    aux (TexRGBA8  (GLTexture img)) = f img
    aux (TexSRGB8  (GLTexture img)) = f img
    {-# INLINE aux #-}
{-# INLINE textureImageMap #-}

textureImageSpec :: Getter TextureImage TextureImageSpec
textureImageSpec = to getter where
    getter tex =
        let dimension = V2 (textureImageMap imageWidth tex) (textureImageMap imageHeight tex)
            pxSpec    = pixelSpec tex
        in TextureImageSpec dimension pxSpec
    {-# INLINE getter #-}
{-# INLINE textureImageSpec #-}


mkTextureSpec :: V2 Int -> GL.DataType -> GL.PixelFormat -> GL.PixelInternalFormat -> TextureImageSpec
mkTextureSpec dim dataType components internalFormat = TextureImageSpec dim $ PixelSpec dataType components internalFormat
{-# INLINE mkTextureSpec #-}


-- | derives complete 'TextureImageSpec' from the components
-- The component type will be UnsignedByte (Word8)
mkTextureSpec' :: V2 Int -> GL.PixelFormat -> TextureImageSpec
mkTextureSpec' dim components = TextureImageSpec dim $ PixelSpec GL.UnsignedByte components (compsToInternal components)
{-# INLINE mkTextureSpec' #-}


-- | simple pixel component to internalFormat mapping, assumes UnsignedByte (Word8) components
compsToInternal :: GL.PixelFormat -> GL.PixelInternalFormat
compsToInternal GL.DepthComponent   = GL.DepthComponent'
compsToInternal GL.DepthStencil     = GL.Depth32fStencil8
compsToInternal GL.Red              = GL.R8
compsToInternal GL.RGB              = GL.RGB8
compsToInternal GL.BGR              = GL.RGB8
compsToInternal GL.RGBA             = GL.RGBA8
compsToInternal GL.BGRA             = GL.RGBA8
compsToInternal GL.Luminance        = GL.Luminance8
compsToInternal GL.LuminanceAlpha   = GL.Luminance8Alpha8
-- TODO complete mapping
compsToInternal pf = error $ unpack $ format "unsupported format: {}" (Only $ Shown pf )
{-# INLINE compsToInternal #-}


debugString :: TextureImage -> String
debugString img = unpack $ format "{}: {}" (Shown $ toConstr img, Shown $ img^.textureImageSpec)
{-# INLINE debugString #-}

instance Show TextureImage where
    show = debugString


