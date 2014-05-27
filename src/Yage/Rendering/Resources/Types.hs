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


type VertexBufferRHI       = (MeshHash, GL.BufferObject)
--type GLIndexbuffer         = (MeshHash, GL.BufferObject)
type VertexArrayRHI        = VAO
type ShaderRHI             = ShaderProgram
type TextureRHI            = (BufferSpec, GL.TextureObject)
type RenderbufferRHI       = (BufferSpec, GL.RenderbufferObject)
type FramebufferRHI        = GL.FramebufferObject


data GLResources = GLResources
    { _loadedShaders       :: (Map ShaderResource           ShaderRHI            )
    , _loadedVertexBuffer  :: (Map MeshId                   VertexBufferRHI      )
    , _loadedVertexArrays  :: (Map (MeshId, ShaderResource) VertexArrayRHI       )
    --, _loadedIndexBuffer   :: (Map MeshId                   GLElementbuffer     )
    , _loadedTextures      :: (Map Texture                  TextureRHI           )
    , _loadedRenderbuffers :: (Map Renderbuffer             RenderbufferRHI      )
    , _compiledFBOs        :: (Map String                   FramebufferRHI       )
    }

makeLenses ''GLResources


type ResourceManager = RWST () [String] GLResources IO


