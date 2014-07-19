{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE LambdaCase                         #-}
module Yage.Rendering.Resources.ResTypes where

import           Yage.Prelude                        hiding ( Text )
import           Yage.Lens

import           Data.Data
import           Data.Hashable                       ()

import qualified Yage.Core.OpenGL                    as GL
import           Filesystem.Path.CurrentOS           ( encode )

import           Yage.Rendering.Backend.Framebuffer
import qualified Yage.Rendering.Textures             as TexImg
import           Yage.Rendering.Textures.Instances   ()

import           Yage.Geometry.D2.Rectangle

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject

---------------------------------------------------------------------------------------------------

type BufferSpec = TexImg.TextureImageSpec
data Renderbuffer = Renderbuffer String BufferSpec

type TextureId = ByteString

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
    } deriving ( Show, Eq, Ord, Typeable, Data )

data TextureFiltering = TextureFiltering
    { _texMinFilter    :: !GL.TextureFilter
    -- ^ GL.Nearest | GL.Linear'
    , _texMipmapFilter :: !(Maybe GL.TextureFilter)
    -- ^ GL.Nearest | GL.Linear'
    , _texMagFilter    :: !GL.TextureFilter
    -- ^ GL.Nearest | GL.Linear'
    } deriving ( Show, Eq, Ord, Typeable, Data )

data TextureWrapping = TextureWrapping
    { _texWrapRepetition :: GL.Repetition 
    -- ^ GL.Repeated | GL.Mirrored
    , _texWrapClamping   :: GL.Clamping
    -- ^ GL.Clamp | GL.Repeat | GL.ClampToEdge | GL.ClampToBorder
    } deriving ( Show, Eq, Ord, Typeable, Data )

makeLenses ''TextureConfig
makeLenses ''TextureFiltering
makeLenses ''TextureWrapping

type RenderTargets = AttachmentTypes Texture Renderbuffer

textureId :: Lens' Texture ByteString
textureId = lens getter setter where
    getter (Texture tid _ _) = tid
    setter (Texture _ conf tdata) tid = Texture tid conf tdata  


textureData :: Lens' Texture TextureData
textureData = lens getter setter where
    getter (Texture _ _ texData)      = texData
    setter (Texture tid conf _) texData = Texture tid conf texData

textureConfig :: Lens' Texture TextureConfig
textureConfig = lens getter setter where
    getter (Texture _ conf _) = conf
    setter (Texture tid _ texData) conf = Texture tid conf texData


mkTexture :: TextureId -> TextureData -> Texture
mkTexture texid texdata = Texture texid def texdata



isTextureBuffer :: Texture -> Bool
isTextureBuffer tex = 
    case tex^.textureData of
        TextureBuffer _ _ -> True
        _ -> False


class HasTextureSpec t where
    textureSpec :: Getter t BufferSpec

instance HasTextureSpec Texture where
    textureSpec = to getter where
        getter tex =
            case tex^.textureData of
            Texture2D img        -> img^.TexImg.textureImageSpec
            TextureBuffer _ spec -> spec
            TextureCube TexImg.Cube{TexImg.cubeFaceRight = img} -> img^.TexImg.textureImageSpec
    {-# INLINE textureSpec #-}


instance HasTextureSpec Renderbuffer where
    textureSpec = to getter where
        getter (Renderbuffer _ spec) = spec
    {-# INLINE textureSpec #-}


instance GetRectangle Renderbuffer Int where
    asRectangle = to getter where
        getter tex = Rectangle 0 (tex^.textureSpec.TexImg.texSpecDimension)
    {-# INLINE asRectangle #-}

instance GetRectangle Texture Int where
    asRectangle = to getter where
        getter tex = Rectangle 0 (tex^.textureSpec.TexImg.texSpecDimension)
    {-# INLINE asRectangle #-}



--instance (FramebufferSpec target RenderTargets) => HasRectangle target Int where
--    rectangle = lens getter setter where
--        getter fboSpec =
--            let ((Attachment _ target):_) = allAttachments fboSpec
--            in target^.textureSpec.rectangle

--        setter = error "FramebufferSpec: no settable rectangle"




instance Ord Texture where
    compare a b = compare (a^.textureId) (b^.textureId)


instance Show Texture where
    show tex = 
        unpack $ format "{}.{} spec={}, conf={}" 
            ( Shown $ tex^.textureData.to toConstr
            , Shown $ tex^.textureId
            , Shown $ tex^.textureSpec
            , Shown $ tex^.textureConfig
            )


instance Eq Texture where
    (==) a b = a^.textureId == b^.textureId


instance Show Renderbuffer where
    show ( Renderbuffer name spec ) =
        unpack $ format "Renderbuffer.{} spec={}" ( name, Shown spec )


instance Eq Renderbuffer where
    (==) (Renderbuffer nameA _) (Renderbuffer nameB _) = nameA == nameB


instance Ord Renderbuffer where
    compare (Renderbuffer nameA _) (Renderbuffer nameB _) = compare nameA nameB


instance Hashable Texture where
    hashWithSalt salt tex =
        salt `hashWithSalt` (tex^.textureId)


instance Hashable FilePath where
    hashWithSalt salt = hashWithSalt salt . encode

instance Default TextureConfig where
    def = TextureConfig def def

instance Default TextureWrapping where
    def = TextureWrapping GL.Repeated GL.Repeat

instance Default TextureFiltering where
    def = TextureFiltering GL.Linear' (Just GL.Linear') GL.Linear'

