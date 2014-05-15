{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude                        hiding ( Text, unpack )
import           Yage.Lens                           ( makeLenses )
import           Yage.Text                           as TF

import           Data.Data
import           Data.Hashable                       ()

import qualified Graphics.Rendering.OpenGL           as GL
import           Filesystem.Path.CurrentOS           ( encode )

import           Yage.Rendering.Backend.Framebuffer
import qualified Yage.Rendering.Textures             as TexImg
import           Yage.Rendering.Textures.Instances   ()

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject

---------------------------------------------------------------------------------------------------

type BufferSpec = TexImg.TextureImageSpec
data Renderbuffer = Renderbuffer String BufferSpec

type TextureId = Text

data Texture = Texture TextureId TextureData
    deriving ( Typeable, Data )

data TextureData =
      Texture2D        TexImg.TextureImage
    | TextureCube      TexImg.TextureCube
    | TextureBuffer    GL.TextureTarget2D BufferSpec
    deriving ( Typeable, Data )

type RenderTargets = AttachmentTypes Texture Renderbuffer


textureId :: Texture -> Text
textureId (Texture tid texData) = TF.format "{}.{}" ( Shown $ toConstr texData, Shown tid )


textureData :: Texture -> TextureData
textureData (Texture _ texData) = texData


textureSpec :: Texture -> BufferSpec
textureSpec (Texture _ texData) = aux texData
    where
    aux ( Texture2D img )                                = TexImg.textureSpec img
    aux ( TextureCube TexImg.Cube{cubeFaceRight = img} ) = TexImg.textureSpec img
    aux ( TextureBuffer _ spec )                         = spec


instance Ord Texture where
    compare a b = compare (textureId a) (textureId b)


instance Show Texture where
    show tex = 
        unpack $ TF.format "{} spec={}" ( textureId tex, Shown $ textureSpec tex )


instance Eq Texture where
    (==) a b = textureId a == textureId b


instance Show Renderbuffer where
    show ( Renderbuffer name spec ) =
        unpack $ TF.format "Renderbuffer.{} spec={}" ( name, Shown spec )


instance Eq Renderbuffer where
    (==) (Renderbuffer nameA _) (Renderbuffer nameB _) = nameA == nameB


instance Ord Renderbuffer where
    compare (Renderbuffer nameA _) (Renderbuffer nameB _) = compare nameA nameB


instance Hashable Texture where
    hashWithSalt salt tex =
        salt `hashWithSalt` (textureId tex)


data ShaderResource = ShaderResource
    { _srVertSrc :: FilePath
    , _srFragSrc :: FilePath
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderResource


instance Hashable ShaderResource where
    hashWithSalt salt ShaderResource{..} =
        salt       `hashWithSalt`
        _srVertSrc `hashWithSalt` _srFragSrc


instance Hashable FilePath where
    hashWithSalt salt = hashWithSalt salt . encode



