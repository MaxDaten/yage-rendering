{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Rendering.Textures.TextureCube (
      TextureCube(..)

    , readCubeTextures, loadCubeTexture
    ) where

import Yage.Prelude                                                 hiding ( Vector, sequence, any )

import Data.Foldable
import Data.Traversable

import Graphics.GLUtil.GLError
import Graphics.Rendering.OpenGL                                    ( genObjectNames, ($=) )
import qualified Graphics.Rendering.OpenGL                          as GL

import Yage.Rendering.Textures.TextureUtils
import Codec.Picture

-- | In OpenGL Order
data TextureCube a = TextureCube
    { cubeFaceRight :: !a -- | TextureCubeMapPositiveX   
    , cubeFaceLeft  :: !a -- | TextureCubeMapNegativeX  
    , cubeFaceTop   :: !a -- | TextureCubeMapPositiveY  
    , cubeFaceBottom:: !a -- | TextureCubeMapNegativeY  
    , cubeFaceFront :: !a -- | TextureCubeMapPositiveZ  
    , cubeFaceBack  :: !a -- | TextureCubeMapNegativeZ
    } deriving ( Show, Functor, Foldable, Traversable )



instance Applicative TextureCube where
  pure a = TextureCube a a a a a a
  {-# INLINE pure #-}
  TextureCube a b c d e f <*> TextureCube g h i j k l = TextureCube (a g) (b h) (c i) (d j) (e k) (f l)
  {-# INLINE (<*>) #-}



glCubeMapFaces :: TextureCube TextureTargetCubeMapFace
glCubeMapFaces =
    TextureCube TextureCubeMapPositiveX
                TextureCubeMapNegativeX
                TextureCubeMapPositiveY
                TextureCubeMapNegativeY
                TextureCubeMapPositiveZ
                TextureCubeMapNegativeZ


readCubeTextures :: TextureCube DynamicImage -> IO (Either String TextureObject)
readCubeTextures cube = 
    let eCubeTextures = sequence $ getTexInfo <$> cube :: Either String (TextureCube Tex)
    in case eCubeTextures of
        Left err  -> return $ Left err
        Right c   -> Right <$> loadCubeTexture c

-- | to GLUtil
-- |Create a new 2D texture with data from a 'TexInfo'.
loadCubeTexture :: TextureCube Tex -> IO TextureObject
loadCubeTexture cubeTexs = do 
    printErrorMsg $ "loadCubeTexture"
    
    [obj] <- genObjectNames 1
    withTextureBoundAt TextureCubeMap obj $ do
        sequenceA_ $ (\(Tex i) -> reloadTextureTarget i) <$> cubeTexs <*> glCubeMapFaces
    
        textureWrapMode TextureCubeMap GL.R $= (Mirrored, ClampToEdge)
        textureWrapMode TextureCubeMap GL.S $= (Mirrored, ClampToEdge)
        textureWrapMode TextureCubeMap GL.T $= (Mirrored, ClampToEdge)
        textureFilter TextureCubeMap        $= ((GL.Linear', Just GL.Linear'), GL.Linear')
    
    printErrorMsg $ "loadCubeTexture-post"
    return obj
