{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude hiding (Text, unpack)
import           Yage.Text as TF

import           Data.Hashable                       ()

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
      TextureImage String DynamicImage
    | TextureImageCube String (CubeMap DynamicImage)
    | TextureBuffer String GL.TextureTarget2D GLBufferSpec -- only backed by opengl
    deriving (Typeable)


getTextureID :: TextureResource -> Text
getTextureID (TextureImage     name _  )  = TF.format "TextureImage: ident={}"     (Only name)
getTextureID (TextureImageCube name _  )  = TF.format "TextureImageCube: ident={}" (Only name)
getTextureID (TextureBuffer    name _ _)  = TF.format "TextureBuffer: ident={}"     (Only name)

-- TODO
instance Ord TextureResource where
    compare a b = compare (getTextureID a) (getTextureID b)

instance Show TextureResource where
    show = unpack . getTextureID

instance Eq TextureResource where
    (==) a b = getTextureID a == getTextureID b

instance Show RenderbufferResource where
    show (RenderbufferResource name _) = show name

instance Eq RenderbufferResource where
    (==) (RenderbufferResource nameA _) (RenderbufferResource nameB _) = nameA == nameB

instance Ord RenderbufferResource where
    compare (RenderbufferResource nameA _) (RenderbufferResource nameB _) = compare nameA nameB


instance Hashable TextureResource where
    hashWithSalt salt tex =
        salt `hashWithSalt` (getTextureID tex)



data ShaderResource = ShaderResource
    { _srVertSrc :: FilePath
    , _srFragSrc :: FilePath
    } deriving (Show, Eq, Ord)


