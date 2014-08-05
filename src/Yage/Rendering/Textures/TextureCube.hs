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
    { cubeFaceRight :: !a
    -- ^ TextureCubeMapPositiveX
    , cubeFaceLeft  :: !a
    -- ^ TextureCubeMapNegativeX
    , cubeFaceTop   :: !a
    -- ^ TextureCubeMapPositiveY
    , cubeFaceBottom:: !a
    -- ^ TextureCubeMapNegativeY
    , cubeFaceFront :: !a
    -- ^ TextureCubeMapPositiveZ
    , cubeFaceBack  :: !a
    -- ^ TextureCubeMapNegativeZ
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


uploadCubeTextureImage :: TextureCube -> IO ()
uploadCubeTextureImage cubeTexs =
    sequenceA_ $ loadTex <$> glCubeFaces <*> cubeTexs

    where

    loadTex :: GL.TextureTargetCubeMapFace -> TextureImage -> IO ()
    loadTex = uploadTextureImage'

