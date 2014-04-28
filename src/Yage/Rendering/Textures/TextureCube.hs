{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
module Yage.Rendering.Textures.TextureCube where

import Yage.Prelude                                                 hiding ( Vector, sequence, any )

import Data.Data
import Data.Foldable
import qualified Graphics.Rendering.OpenGL                          as GL

import Yage.Rendering.Textures.TextureImage

-- | In OpenGL Order
data Cube a = Cube
    { cubeFaceRight :: !a -- | TextureCubeMapPositiveX   
    , cubeFaceLeft  :: !a -- | TextureCubeMapNegativeX  
    , cubeFaceTop   :: !a -- | TextureCubeMapPositiveY  
    , cubeFaceBottom:: !a -- | TextureCubeMapNegativeY  
    , cubeFaceFront :: !a -- | TextureCubeMapPositiveZ  
    , cubeFaceBack  :: !a -- | TextureCubeMapNegativeZ
    } deriving ( Show, Functor, Foldable, Traversable, Data, Typeable )



instance Applicative Cube where
  pure a = Cube a a a a a a
  {-# INLINE pure #-}
  Cube a b c d e f <*> Cube g h i j k l = Cube (a g) (b h) (c i) (d j) (e k) (f l)
  {-# INLINE (<*>) #-}


type TextureCube = Cube TextureImage
type GLCubeFaces = Cube GL.TextureTargetCubeMapFace

glCubeFaces :: GLCubeFaces
glCubeFaces =
    Cube
        GL.TextureCubeMapPositiveX
        GL.TextureCubeMapNegativeX
        GL.TextureCubeMapPositiveY
        GL.TextureCubeMapNegativeY
        GL.TextureCubeMapPositiveZ
        GL.TextureCubeMapNegativeZ


loadCubeTextureImage :: TextureCube -> IO ()
loadCubeTextureImage cubeTexs = 
    sequenceA_ $ loadTex <$> glCubeFaces <*> cubeTexs

    where

    loadTex :: GL.TextureTargetCubeMapFace -> TextureImage -> IO ()
    loadTex = loadTextureImage'

{--

readTexturesCubeImage :: TextureCube  -> IO (Either String TextureObject)
readTexturesCubeImage cube = 
    let eCubeTextures = sequence $ getTexInfo <$> cube :: Either String (TextureCube Tex)
    in case eCubeTextures of
        Left err  -> return $ Left err
        Right c   -> Right <$> loadCubeTexture c

-- | to GLUtil
-- |Create a new 2D texture with data from a 'TexInfo'.
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
--}
