{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE ExistentialQuantification          #-}
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


