{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes #-}
module Yage.Rendering.Texture (
      TexColor(), TexInfo()
    , readTextureImg, readTexInfoImgWith
    , module JT
    ) where

import Yage.Prelude

import Graphics.GLUtil.JuicyTextures as JT
import Codec.Picture as JT
import Codec.Picture.Types as JT
import Graphics.GLUtil.Textures
import Graphics.Rendering.OpenGL (TextureObject)

deriving instance Show TexColor

instance Show (TexInfo a) where
    show TexInfo{..} = format "TexInfo: {0} {1} - {2}" [show texWidth, show texHeight, show texColor]


-- http://hackage.haskell.org/package/GLUtil-0.7/docs/src/Graphics-GLUtil-JuicyTextures.html#readTexture
readTexInfoImgWith :: DynamicImage -> (forall a. IsPixelData a => TexInfo a -> IO b) -> IO (Either String b)
readTexInfoImgWith img k = getTexInfo img
    where
        getTexInfo (ImageY8    (Image w h p)) = Right <$> k (texInfo w h TexMono p)
        getTexInfo (ImageYF    (Image w h p)) = Right <$> k (texInfo w h TexMono p)
        getTexInfo (ImageRGB8  (Image w h p)) = Right <$> k (texInfo w h TexRGB p)
        getTexInfo (ImageRGBF  (Image w h p)) = Right <$> k (texInfo w h TexRGB p)
        getTexInfo (ImageRGBA8 (Image w h p)) = Right <$> k (texInfo w h TexRGBA p)
        getTexInfo (ImageYCbCr8 img'        ) = getTexInfo . ImageRGB8 $ convertImage img'
        getTexInfo (ImageYA8    _           ) = return $ Left "YA format not supported"
        getTexInfo _                          = return $ Left "Unsupported image format"


readTextureImg :: DynamicImage -> IO (Either String TextureObject)
readTextureImg res = readTexInfoImgWith res loadTexture

