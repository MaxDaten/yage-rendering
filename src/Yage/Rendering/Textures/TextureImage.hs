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
import              Control.Monad                       ( zipWithM_ )
import              Graphics.GLUtil.Textures            ( IsPixelData(..) )
import              Graphics.GLUtil.TypeMapping         ( HasGLType, glType )
import qualified    Yage.Core.OpenGL                    as GL

import              Codec.Picture
import              Codec.Picture.Types                 ( convertImage )
import              JuicySRGB
import              Linear                              ( V2(..) )

import              Yage.Rendering.Textures.Instances
import              Yage.Rendering.Textures.MipMapChain

-- | A tag for opengl-related instances
newtype GLTexture pixel = GLTexture { unGLTexture :: Image pixel }

deriving instance Typeable GLTexture
deriving instance (TyPixel pixel) => Data (GLTexture pixel)

data TextureImage =
       TexY8    (GLTexture Pixel8)
        -- ^ A greyscale image.
     | TexYF    (GLTexture PixelF)
        -- ^ A greyscale HDR image
     | TexRGB8  (GLTexture PixelRGB8)
        -- ^ An image in true color.
     | TexRGBF  (GLTexture PixelRGBF)
        -- ^ An image with HDR pixels
     | TexRGBA8 (GLTexture PixelRGBA8)
        -- ^ An image in true color and an alpha channel.

        -- | TexYCbCr8 (Image PixelYCbCr8)
        --  An image in the colorspace used by Jpeg images.
        -- we does not support YCbrCr as an internal texture format
        -- (converting will be supported outside the rendering core)
     | TexSRGB8  (GLTexture PixelSRGB8)
       -- ^ An Image in the sRGB color space
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
{-# INLINE fromDynamic #-}

toDynamic :: TextureImage -> DynamicImage
toDynamic = aux
    where
    aux ( TexY8    ( GLTexture img ) )   = ImageY8 img
    aux ( TexYF    ( GLTexture img ) )   = ImageYF img
    aux ( TexRGB8  ( GLTexture img ) )   = ImageRGB8 img
    aux ( TexRGBF  ( GLTexture img ) )   = ImageRGBF img
    aux ( TexRGBA8 ( GLTexture img ) )   = ImageRGBA8 img
    aux ( TexSRGB8 ( GLTexture img ) )   = ImageRGB8 (convertImage img)
{-# INLINE toDynamic #-}

-- | deriving version of texture loading. derives the PixelInternalFormat an PixelType of the image
-- loads into the currently bound texture object in the context
uploadTextureImages' :: (GL.TwoDimensionalTextureTarget t, Show t) =>
                  t -> MipMapChain TextureImage -> IO ()
uploadTextureImages' target textureImgs =
    uploadTextureImages target $ fmap (\img -> (img, pixelSpec img)) textureImgs


-- | explicit texture loading. the types and internalFormat
-- loads into the currently bound texture object in the context
uploadTextureImages :: (GL.TwoDimensionalTextureTarget t, Show t) =>
                 t -> MipMapChain (TextureImage, PixelSpec) -> IO ()
uploadTextureImages target textureImgs = zipWithM_ (uncurry aux) (toList textureImgs) [0..]
    where
    aux (TexY8     (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexYF     (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexRGB8   (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexRGBF   (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexRGBA8  (GLTexture (Image w h p))) = loadTex p (texSize w h)
    aux (TexSRGB8  (GLTexture (Image w h p))) = loadTex p (texSize w h)

    loadTex :: IsPixelData p => p -> GL.TextureSize2D -> PixelSpec -> GL.GLint -> IO ()
    loadTex p sz spec lvl = texImage2D target lvl p sz spec

    texSize w h = GL.TextureSize2D (fromIntegral w) (fromIntegral h)



texImage2D :: (GL.TwoDimensionalTextureTarget t, Show t, IsPixelData d) =>
            t -> GL.Level -> d -> GL.TextureSize2D -> PixelSpec -> IO ()
texImage2D target mipLevel pxdata texSize (PixelSpec dataType components internalFormat) =
    GL.checkErrorOf errFmt $ withPixels pxdata $
            GL.texImage2D target GL.NoProxy mipLevel internalFormat texSize 0 . GL.PixelData components dataType
    where
    errFmt = unpack $ format
        "texImage2D: target: {}, mipLevel: {}, texSize: {}, dataType: {}, components: {}, internalFormat: {}"
        (Shown target, Shown mipLevel, Shown texSize, Shown dataType, Shown components, Shown internalFormat )

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


textureImageDimension :: Getter TextureImage (V2 Int)
textureImageDimension = textureImageSpec.texSpecDimension

withTextureImage :: TextureImage -> (forall pixel. Pixel pixel => Image pixel -> a) -> a
withTextureImage texImg f = withTextureImageCtr texImg (uncurry $ const f)

withTextureImageCtr :: TextureImage -> (forall pixel. Pixel pixel => (TextureCtr pixel, Image pixel) -> a) -> a
withTextureImageCtr texImg f = aux texImg where
    aux (TexY8     (GLTexture img)) = f (TexY8, img)
    aux (TexYF     (GLTexture img)) = f (TexYF, img)
    aux (TexRGB8   (GLTexture img)) = f (TexRGB8, img)
    aux (TexRGBF   (GLTexture img)) = f (TexRGBF, img)
    aux (TexRGBA8  (GLTexture img)) = f (TexRGBA8, img)
    aux (TexSRGB8  (GLTexture img)) = f (TexSRGB8, img)
    {-# INLINE aux #-}
{-# INLINE withTextureImage #-}

textureImageSpec :: Getter TextureImage TextureImageSpec
textureImageSpec = to getter where
    getter tex =
        let dimension = V2 (withTextureImage tex imageWidth) (withTextureImage tex imageHeight)
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
mkTextureSpec' dim components = TextureImageSpec dim $ PixelSpec GL.UnsignedByte components (defaultInternal components)
{-# INLINE mkTextureSpec' #-}


-- | simple pixel component to internalFormat mapping, assumes UnsignedByte (Word8) components
defaultInternal :: GL.PixelFormat -> GL.PixelInternalFormat
defaultInternal GL.DepthComponent   = GL.DepthComponent'
defaultInternal GL.DepthStencil     = GL.Depth32fStencil8 -- | FIXME : find 24/8 DepthStencil?
defaultInternal GL.Red              = GL.R8
defaultInternal GL.RGB              = GL.RGB8
defaultInternal GL.BGR              = GL.RGB8
defaultInternal GL.RGBA             = GL.RGBA8
defaultInternal GL.BGRA             = GL.RGBA8
defaultInternal GL.Luminance        = GL.Luminance8
defaultInternal GL.LuminanceAlpha   = GL.Luminance8Alpha8
-- TODO complete mapping
defaultInternal pf = error $ unpack $ format "unsupported format: {}" (Only $ Shown pf )
{-# INLINE defaultInternal #-}


debugString :: TextureImage -> String
debugString img = unpack $ format "{}: {}" (Shown $ toConstr img, Shown $ img^.textureImageSpec)
{-# INLINE debugString #-}

instance Show TextureImage where
    show = debugString


class IsTextureImage a where
    toTextureImage :: a -> TextureImage

instance IsTextureImage (Image Pixel8) where
    toTextureImage = mkTextureImg TexY8

instance IsTextureImage (Image PixelF) where
    toTextureImage = mkTextureImg TexYF

instance IsTextureImage (Image PixelRGB8) where
    toTextureImage = mkTextureImg TexRGB8

instance IsTextureImage (Image PixelRGBF) where
    toTextureImage = mkTextureImg TexRGBF

instance IsTextureImage (Image PixelRGBA8) where
    toTextureImage = mkTextureImg TexRGBA8

instance IsTextureImage (Image PixelSRGB8) where
    toTextureImage = mkTextureImg TexSRGB8
