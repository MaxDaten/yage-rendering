{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude hiding (Text, unpack)
import           Yage.Text as TF

import           Data.Hashable                       ()

import qualified Graphics.Rendering.OpenGL as GL

import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.Textures
import           Linear

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject

---------------------------------------------------------------------------------------------------

data GLBufferSpec = GLBufferSpec 
    { _glBufferFormat    :: !GL.PixelInternalFormat 
    , _glBufferSize      :: V2 Int
    }


data Renderbuffer = Renderbuffer String GLBufferSpec

data Texture =
      TextureImage String DynamicImage
    | TextureImageCube String (TextureCube DynamicImage)
    | TextureBuffer String GL.TextureTarget2D GLBufferSpec
    deriving (Typeable)

type RenderTargets = AttachmentTypes Texture Renderbuffer

getTextureID :: Texture -> Text
getTextureID (TextureImage     name _  )  = TF.format "TextureImage: ident={}"     (Only name)
getTextureID (TextureImageCube name _  )  = TF.format "TextureImageCube: ident={}" (Only name)
getTextureID (TextureBuffer    name _ _)  = TF.format "TextureBuffer: ident={}"     (Only name)


instance Ord Texture where
    compare a b = compare (getTextureID a) (getTextureID b)

instance Show Texture where
    show = unpack . getTextureID

instance Eq Texture where
    (==) a b = getTextureID a == getTextureID b

instance Show Renderbuffer where
    show (Renderbuffer name _) = show name

instance Eq Renderbuffer where
    (==) (Renderbuffer nameA _) (Renderbuffer nameB _) = nameA == nameB

instance Ord Renderbuffer where
    compare (Renderbuffer nameA _) (Renderbuffer nameB _) = compare nameA nameB


instance Hashable Texture where
    hashWithSalt salt tex =
        salt `hashWithSalt` (getTextureID tex)



data ShaderResource = ShaderResource
    { _srVertSrc :: FilePath
    , _srFragSrc :: FilePath
    } deriving (Show, Eq, Ord)


