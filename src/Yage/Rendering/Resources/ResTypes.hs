{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE LambdaCase                         #-}
{-# LANGUAGE RankNTypes                         #-}
module Yage.Rendering.Resources.ResTypes where
    -- ( mkTexture2D, mkTexture2DMip, mkTextureBuffer
    -- , Texture
    -- , TextureConfig(..), texConfFiltering, texConfWrapping
    -- , TextureFiltering(..), texMinFilter, texMipmapFilter, texMagFilter
    -- , TextureWrapping(..), texWrapRepetition, texWrapClamping
    -- , textureId, textureData, textureConfig, defaultTextureConfig
    -- , HasTextureSpec(..)

    -- , isTextureBuffer, textureDimension
    -- , VBO, EBO, FBO, RenderTargets, BufferSpec, Renderbuffer, TextureId
    -- )
    -- where

import           Yage.Prelude                        hiding ( Text )
import           Yage.Lens
import           Yage.Math                           (V2)

import           Data.Data
import           Data.Hashable                       ()

import qualified Yage.Core.OpenGL                    as GL
import           Filesystem.Path.CurrentOS           ( encode )

import           Yage.Rendering.Backend.Framebuffer
import qualified Yage.Rendering.Textures             as TexImg
import           Yage.Rendering.Textures.Instances   ()
import           Yage.Rendering.Textures.MipMapChain

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

-- | The relevant `TextureData` contains the binary pixel data for a
--   simple 2D texture or a cube texture (e.g.  a environemnt map).
--   For `Texture2D` or `TextureCube` a optional custom mipmap chain is supported.
--   There is no mipmap generation with a given custom mipmap chain.
data TextureData =
      Texture2D          (MipMapChain TexImg.TextureImage)
      -- ^ a simple 2D texture with an optional custom mipmap chain.
    | TextureCube        (MipMapChain TexImg.TextureCube)
    -- ^ a cube texture with an optional custom mipmap chain
    | TextureBuffer      GL.TextureTarget2D BufferSpec
    -- ^ a texture buffer, filled by and reside on the render hardware.
    --   No direct access currently supported
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


hasCustomMipMaps :: Getter Texture Bool
hasCustomMipMaps = to get where
    get tex = case tex^.textureData of
        TextureBuffer _ _ -> False
        Texture2D mips    -> hasMipMaps mips
        TextureCube mips  -> hasMipMaps mips


isTextureBuffer :: Getter Texture Bool
isTextureBuffer = to get where
    get tex = case tex^.textureData of
        TextureBuffer _ _ -> True
        _ -> False

withTextureTarget :: Texture -> (forall t. GL.ParameterizedTextureTarget t => t -> IO ()) -> IO ()
withTextureTarget texture fm =
    case texture^.textureData of
        Texture2D   _     -> fm GL.Texture2D
        TextureCube _     -> fm GL.TextureCubeMap
        TextureBuffer t _ -> fm t


-- | creates a simple 2D texture container with given id and content
--   without custom mipmap
mkTexture2D :: TextureId -> TexImg.TextureImage -> Texture
mkTexture2D texId img = Texture texId def $ Texture2D $ mkMipMapChain img []

-- | creates a simple 2D texture container with given id and content
--   with a custom mipmap
mkTexture2DMip :: TextureId -> MipMapChain TexImg.TextureImage -> Texture
mkTexture2DMip texId mipchainImgs = Texture texId def $ Texture2D mipchainImgs

-- | creates a cube texture container with given id and content
--   without custom mipmap
mkTextureCube :: TextureId -> TexImg.TextureCube -> Texture
mkTextureCube texId img = Texture texId def $ TextureCube $ mkMipMapChain img []

-- | creates a cube texture container with given id and content
--   with a custom mipmap
mkTextureCubeMip :: TextureId -> MipMapChain TexImg.TextureCube -> Texture
mkTextureCubeMip texId mipchainImgs = Texture texId def $ TextureCube mipchainImgs

-- | creates a `Texture` as a buffer texture reside on the render hardware
mkTextureBuffer :: TextureId -> GL.TextureTarget2D -> BufferSpec -> Texture
mkTextureBuffer texId target spec = Texture texId def (TextureBuffer target spec)



class HasTextureSpec t where
    textureSpec :: Getter t BufferSpec

-- | Getting the `TextureSpec` of the `Texture`.
--   For `Texture`s with a custom mipmap chain the value is
--   the `TextureSpec` of the base (0th) mipmap
instance HasTextureSpec Texture where
    textureSpec = to getter where
        getter tex =
            case tex^.textureData of
            Texture2D imgs        -> imgs^.to mipMapBase.TexImg.textureImageSpec
            TextureCube imgs      -> imgs^.to mipMapBase.to TexImg.cubeFaceRight.TexImg.textureImageSpec
            TextureBuffer _ spec  -> spec
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



textureDimension :: Getter Texture (V2 Int)
textureDimension = textureSpec.TexImg.texSpecDimension


defaultTextureConfig :: TextureConfig
defaultTextureConfig = TextureConfig def def


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
    def = defaultTextureConfig

instance Default TextureWrapping where
    def = TextureWrapping GL.Repeated GL.Repeat

instance Default TextureFiltering where
    def = TextureFiltering GL.Linear' (Just GL.Linear') GL.Linear'

