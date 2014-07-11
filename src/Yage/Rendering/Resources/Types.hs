{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE LambdaCase          #-}
module Yage.Rendering.Resources.Types where

import           Yage.Prelude
import           Yage.Lens


import           Control.Monad.RWS

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.GLUtil



import           Yage.Rendering.Mesh
import           Yage.Rendering.Resources.ResTypes


-- | RHI RenderHardwareInterface
type VertexBufferRHI       = (MeshHash, GL.BufferObject)
type IndexBufferRHI        = (MeshHash, GL.BufferObject)
type VertexArrayRHI        = VAO
type ShaderRHI             = ShaderProgram
type TextureRHI            = (BufferSpec, TextureConfig, GL.TextureObject)
type RenderbufferRHI       = (BufferSpec, GL.RenderbufferObject)
type FramebufferRHI        = GL.FramebufferObject


data GLResources = GLResources
    { _loadedShaders       :: (Map ShaderResource           ShaderRHI            )
    , _loadedVertexBuffer  :: (Map MeshId                   VertexBufferRHI      )
    , _loadedVertexArrays  :: (Map (MeshId, ShaderResource) VertexArrayRHI       )
    , _loadedIndexBuffers  :: (Map MeshId                   IndexBufferRHI       )
    , _loadedTextures      :: (Map Texture                  TextureRHI           )
    , _loadedRenderbuffers :: (Map Renderbuffer             RenderbufferRHI      )
    , _compiledFBOs        :: (Map String                   FramebufferRHI       )
    }

makeLenses ''GLResources


type ResourceManager = RWST () [String] GLResources IO

data UpdateTag a = Clean a | Dirty a
    deriving ( Show, Eq, Ord, Functor, Foldable, Traversable )


instance Applicative UpdateTag where
    pure = return 
    (<*>) = ap 


instance Monad UpdateTag where
    return = Clean
    Clean a >>= f = f a
    Dirty a >>= f = case f a of
        Dirty b -> Dirty b
        Clean b -> Dirty b

tagDirty :: a -> UpdateTag a
tagDirty = Dirty

updateTag :: (a -> c) -> (a -> c) -> UpdateTag a -> c
updateTag clean dirty = \case
    Clean a -> clean a
    Dirty a -> dirty a


isDirty :: UpdateTag a -> Bool
isDirty (Dirty _) = True
isDirty _         = False

isClean :: UpdateTag a -> Bool
isClean (Clean _) = True
isClean _         = False

