{-# LANGUAGE ConstraintKinds                    #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude

import qualified Graphics.Rendering.OpenGL as GL

import           Yage.Rendering.Texture

import           Linear

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject

---------------------------------------------------------------------------------------------------

data GLBufferSpec = GLBufferSpec 
    { _glBufferFormat    :: !GL.PixelInternalFormat 
    , _glBufferSize      :: V2 Int
    }

data RenderbufferResource = RenderbufferResource String GLBufferSpec

data TextureResource =
      TextureFile FilePath
    | TextureImage String DynamicImage
    | TextureBuffer String GL.TextureTarget2D GLBufferSpec -- only backed by opengl
    deriving (Typeable)


data VertexDataResource =
    OBJFile FilePath
    deriving ( Show )

-- TODO
instance Ord TextureResource where
    compare (TextureFile pathA   ) (TextureFile pathB   )    = compare pathA pathB
    compare (TextureImage nameA _) (TextureImage nameB _)    = compare nameA nameB
    compare (TextureBuffer nameA _ _) (TextureBuffer nameB _ _)  = compare nameA nameB
    compare (TextureFile _) _ = GT
    compare _ _               = LT

instance Show TextureResource where
    show (TextureFile path)     = show path
    show (TextureImage name _ ) = show name
    show (TextureBuffer name _ _)  = show name

instance Eq TextureResource where
    (==) (TextureFile name1) (TextureFile name2)           = name1 == name2
    (==) (TextureImage name1 _ ) (TextureImage name2 _ )   = name1 == name2
    (==) (TextureBuffer name1 _ _) (TextureBuffer name2 _ _)   = name1 == name2
    (==) _ _ = False

instance Show RenderbufferResource where
    show (RenderbufferResource name _) = show name

instance Eq RenderbufferResource where
    (==) (RenderbufferResource nameA _) (RenderbufferResource nameB _) = nameA == nameB

instance Ord RenderbufferResource where
    compare (RenderbufferResource nameA _) (RenderbufferResource nameB _) = compare nameA nameB



data ShaderResource = ShaderResource
    { _srVertSrc :: FilePath
    , _srFragSrc :: FilePath
    } deriving (Show, Eq, Ord)


