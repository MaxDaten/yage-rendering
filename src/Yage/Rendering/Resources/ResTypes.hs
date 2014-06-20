{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE LambdaCase                         #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude                        hiding ( Text )
import           Yage.Lens                           ( makeLenses )

import           Data.Data
import           Data.Hashable                       ()

import qualified Yage.Core.OpenGL                    as GL
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

type TextureId = LText

data Texture = Texture TextureId TextureConfig TextureData
    deriving ( Typeable, Data )

data TextureData =
      Texture2D        TexImg.TextureImage
    | TextureCube      TexImg.TextureCube
    | TextureBuffer    GL.TextureTarget2D BufferSpec
    deriving ( Typeable, Data )

data TextureConfig = TextureConfig
    { _texConfFiltering :: !TextureFiltering
    , _texConfWrapping  :: !TextureWrapping
    } deriving ( Show, Eq, Typeable, Data )

data TextureFiltering = TextureFiltering
    { _texMinFilter    :: !GL.TextureFilter
    , _texMipmapFilter :: !(Maybe GL.TextureFilter)
    , _texMagFilter    :: !GL.TextureFilter
    } deriving ( Show, Eq, Ord, Typeable, Data )

data TextureWrapping = TextureWrapping
    { _texWrapRepetition :: GL.Repetition 
    , _texWrapClamping   :: GL.Clamping
    } deriving ( Show, Eq, Ord, Typeable, Data )

makeLenses ''TextureConfig
makeLenses ''TextureFiltering
makeLenses ''TextureWrapping

type RenderTargets = AttachmentTypes Texture Renderbuffer

textureId :: Texture -> LText
textureId (Texture tid _ texData) = format "{}.{}" ( Shown $ toConstr texData, Shown tid )


textureData :: Texture -> TextureData
textureData (Texture _ _ texData) = texData

textureConfig :: Texture -> TextureConfig
textureConfig (Texture _ conf _) = conf


textureSpec :: Texture -> BufferSpec
textureSpec tex = 
    case textureData tex of
    Texture2D img        -> TexImg.textureImageSpec img
    TextureBuffer _ spec -> spec
    TextureCube TexImg.Cube{TexImg.cubeFaceRight = img} -> TexImg.textureImageSpec img


mkTexture :: TextureId -> TextureData -> Texture
mkTexture texid texdata = Texture texid def texdata 


instance Ord Texture where
    compare a b = compare (textureId a) (textureId b)


instance Show Texture where
    show tex = 
        unpack $ format "{} spec={}, conf={}" ( textureId tex, Shown $ textureSpec tex, Shown $ textureConfig tex )


instance Eq Texture where
    (==) a b = textureId a == textureId b


instance Show Renderbuffer where
    show ( Renderbuffer name spec ) =
        unpack $ format "Renderbuffer.{} spec={}" ( name, Shown spec )


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

instance Default TextureConfig where
    def = TextureConfig def def

instance Default TextureWrapping where
    def = TextureWrapping GL.Repeated GL.Repeat

instance Default TextureFiltering where
    def = TextureFiltering GL.Linear' (Just GL.Linear') GL.Linear'

instance GL.MipMappable TextureData where
    generateMipmap' = \case
         Texture2D _       -> GL.generateMipmap' GL.Texture2D
         TextureCube _     -> GL.generateMipmap' GL.TextureCubeMap
         TextureBuffer t _ -> GL.generateMipmap' t
