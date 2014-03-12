{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Rendering.Texture (
      TexColor(), TexInfo(), CubeMap(..)

    , readTextureImg, readCubeTextures, readTexInfoImgWith, loadCubeTexture
    , module JT
    , module GLTex
    ) where

import Yage.Prelude hiding (Vector, sequence, any)

import Data.Foldable
import Data.Traversable

import Graphics.GLUtil.JuicyTextures as JT
import Codec.Picture as JT
import Codec.Picture.Types as JT
import Graphics.GLUtil.Textures
import Graphics.GLUtil.TypeMapping
import Graphics.GLUtil.GLError
import Graphics.Rendering.OpenGL (genObjectNames, ($=))
import Graphics.Rendering.OpenGL.GL.Texturing as GLTex
import qualified Graphics.Rendering.OpenGL as GL

deriving instance Show TexColor

instance Show (TexInfo a) where
    show TexInfo{..} = format "TexInfo: {0} {1} - {2}" [show texWidth, show texHeight, show texColor]

-- | In OpenGL Order
data CubeMap a = CubeMap
    { cubeFaceRight :: !a -- | TextureCubeMapPositiveX   
    , cubeFaceLeft  :: !a -- | TextureCubeMapNegativeX  
    , cubeFaceTop   :: !a -- | TextureCubeMapPositiveY  
    , cubeFaceBottom:: !a -- | TextureCubeMapNegativeY  
    , cubeFaceFront :: !a -- | TextureCubeMapPositiveZ  
    , cubeFaceBack  :: !a -- | TextureCubeMapNegativeZ
    } deriving ( Show, Functor, Foldable, Traversable )

data Tex = forall a. IsPixelData a => Tex (TexInfo a)


instance Applicative CubeMap where
  pure a = CubeMap a a a a a a
  {-# INLINE pure #-}
  CubeMap a b c d e f <*> CubeMap g h i j k l = CubeMap (a g) (b h) (c i) (d j) (e k) (f l)
  {-# INLINE (<*>) #-}



glCubeMapFaces :: CubeMap TextureTargetCubeMapFace
glCubeMapFaces =
    CubeMap TextureCubeMapPositiveX
            TextureCubeMapNegativeX
            TextureCubeMapPositiveY
            TextureCubeMapNegativeY
            TextureCubeMapPositiveZ
            TextureCubeMapNegativeZ


-- http://hackage.haskell.org/package/GLUtil-0.7/docs/src/Graphics-GLUtil-JuicyTextures.html#readTexture
readTexInfoImgWith :: DynamicImage -> (forall a. IsPixelData a => TexInfo a -> IO b) -> IO (Either String b)
readTexInfoImgWith img k =
    case getTexInfo img of
        Right (Tex tex) -> Right <$> k tex
        Left err  -> return $ Left err


getTexInfo :: DynamicImage -> Either String Tex
getTexInfo (ImageY8    (Image w h p)) = Right . Tex $ texInfo w h TexMono p
getTexInfo (ImageYF    (Image w h p)) = Right . Tex $ texInfo w h TexMono p
getTexInfo (ImageRGB8  (Image w h p)) = Right . Tex $ texInfo w h TexRGB p
getTexInfo (ImageRGBF  (Image w h p)) = Right . Tex $ texInfo w h TexRGB p
getTexInfo (ImageRGBA8 (Image w h p)) = Right . Tex $ texInfo w h TexRGBA p
getTexInfo (ImageYCbCr8 img'        ) = getTexInfo . ImageRGB8 $ convertImage img'
getTexInfo (ImageYA8    _           ) = Left "YA format not supported"
getTexInfo _                          = Left "Unsupported image format"


readTextureImg :: DynamicImage -> IO (Either String TextureObject)
readTextureImg res = readTexInfoImgWith res loadTexture


readCubeTextures :: CubeMap DynamicImage -> IO (Either String TextureObject)
readCubeTextures cube = 
    let eCubeTextures = sequence $ getTexInfo <$> cube :: Either String (CubeMap Tex)
    in case eCubeTextures of
        Left err  -> return $ Left err
        Right c   -> Right <$> loadCubeTexture c

-- | to GLUtil
-- |Create a new 2D texture with data from a 'TexInfo'.
loadCubeTexture :: CubeMap Tex -> IO TextureObject
loadCubeTexture cubeTexs = do 
    printErrorMsg $ "loadCubeTexture"
    [obj] <- genObjectNames 1
    textureBinding TextureCubeMap $= Just obj
    sequenceA_ $ (\(Tex i) -> reloadTextureTarget i) <$> cubeTexs <*> glCubeMapFaces
    textureWrapMode TextureCubeMap GL.R $= (Mirrored, ClampToEdge)
    textureWrapMode TextureCubeMap GL.S $= (Mirrored, ClampToEdge)
    textureWrapMode TextureCubeMap GL.T $= (Mirrored, ClampToEdge)
    textureFilter TextureCubeMap        $= ((GLTex.Linear', Just GLTex.Linear'), GLTex.Linear')
    textureBinding TextureCubeMap $= Nothing
    printErrorMsg $ "loadCubeTexture-post"
    return obj

-- |Replace a 2D texture's pixel data with data from a 'TexInfo'.
reloadTextureTarget :: forall a t. (IsPixelData a, TwoDimensionalTextureTarget t, Show t) => 
                    TexInfo a -> t -> IO ()
reloadTextureTarget tex target = (print target) >> (loadTex $ texColor tex)
    where
        loadTex TexMono = case pixelType of
                            GL.UnsignedShort -> loadAux GL.Luminance16 GL.Luminance
                            GL.Float         -> loadAux GL.R32F GL.Red
                            GL.UnsignedByte  -> loadAux GL.R8 GL.Red
                            _                -> loadAux GL.Luminance' GL.Luminance
        loadTex TexRG = case pixelType of
                          GL.UnsignedShort   -> loadAux GL.RG16 GL.RGInteger
                          GL.Float           -> loadAux GL.RG32F GL.RG
                          GL.UnsignedByte    -> loadAux GL.RG8UI GL.RGInteger
                          GL.Byte            -> loadAux GL.RG8I GL.RGInteger
                          GL.Int             -> loadAux GL.RG32I GL.RGInteger
                          GL.UnsignedInt     -> loadAux GL.RG32UI GL.RGInteger
                          _ -> error "Unknown pixelType for TexRG"
        loadTex TexRGB  = loadAux GL.RGBA' GL.RGB
        loadTex TexBGR  = loadAux GL.RGBA' GL.BGR
        loadTex TexRGBA = loadAux GL.RGBA' GL.RGBA
        sz = TextureSize2D (texWidth tex) (texHeight tex)
        pixelType = glType (undefined::Elem a)
        loadAux i e = withPixels (texData tex) $ 
                      (texImage2D target GL.NoProxy 0 i sz 0 .
                       GL.PixelData e pixelType)
