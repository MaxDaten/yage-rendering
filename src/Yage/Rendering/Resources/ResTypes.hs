{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude hiding (Text, unpack)
import           Yage.Text as TF

import           Data.Data
import           Data.Hashable                       ()

import qualified Graphics.Rendering.OpenGL           as GL

import           Yage.Rendering.Backend.Framebuffer
import qualified Yage.Rendering.Textures             as Tex
import           Yage.Rendering.Textures.Instances   ()

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject

---------------------------------------------------------------------------------------------------

type BufferSpec = Tex.TextureImageSpec
data Renderbuffer = Renderbuffer String BufferSpec

data Texture =
      Texture2D        String Tex.TextureImage
    | TextureCube      String Tex.TextureCube
    | TextureBuffer    String GL.TextureTarget2D BufferSpec
    deriving ( Typeable, Data )

type RenderTargets = AttachmentTypes Texture Renderbuffer


textureName :: Texture -> String
textureName = aux
    where
    aux (Texture2D        name _  ) = name
    aux (TextureCube      name _  ) = name
    aux (TextureBuffer    name _ _) = name


textureID :: Texture -> Text
textureID tex = TF.format "{}.{}" ( Shown $ toConstr tex, Shown $ textureName tex )


textureSpec :: Texture -> BufferSpec
textureSpec = aux
    where
    aux ( Texture2D _ img )                             = Tex.textureSpec img
    aux ( TextureCube _ Tex.Cube{cubeFaceRight = img} ) = Tex.textureSpec img
    aux ( TextureBuffer _ _ spec )                      = spec


instance Ord Texture where
    compare a b = compare (textureID a) (textureID b)

instance Show Texture where
    show tex = 
        unpack $ TF.format "{} spec={}" ( textureID tex, Shown $ textureSpec tex )

instance Eq Texture where
    (==) a b = textureID a == textureID b

instance Show Renderbuffer where
    show ( Renderbuffer name spec ) =
        unpack $ TF.format "Renderbuffer.{} spec={}" ( name, Shown spec )

instance Eq Renderbuffer where
    (==) (Renderbuffer nameA _) (Renderbuffer nameB _) = nameA == nameB

instance Ord Renderbuffer where
    compare (Renderbuffer nameA _) (Renderbuffer nameB _) = compare nameA nameB


instance Hashable Texture where
    hashWithSalt salt tex =
        salt `hashWithSalt` (textureID tex)


data ShaderResource = ShaderResource
    { _srVertSrc :: FilePath
    , _srFragSrc :: FilePath
    } deriving (Show, Eq, Ord)


