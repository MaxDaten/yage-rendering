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


type GLVertexbuffer        = (MeshHash, GL.BufferObject)
--type GLIndexbuffer         = (MeshHash, GL.BufferObject)
type GLVertexArray         = VAO
type GLShader              = ShaderProgram
type GLTexture             = (BufferSpec, GL.TextureObject)
type GLRenderbuffer        = (BufferSpec, GL.RenderbufferObject)
type GLFramebuffer         = GL.FramebufferObject


data GLResources = GLResources
    { _loadedShaders       :: (Map ShaderResource           GLShader            )
    , _loadedVertexBuffer  :: (Map MeshId                   GLVertexbuffer      )
    , _loadedVertexArrays  :: (Map (MeshId, ShaderResource) GLVertexArray       )
    --, _loadedIndexBuffer   :: (Map MeshId                   GLElementbuffer     )
    , _loadedTextures      :: (Map Texture                  GLTexture           )
    , _loadedRenderbuffers :: (Map Renderbuffer             GLRenderbuffer      )
    , _compiledFBOs        :: (Map String                   GLFramebuffer       )
    }

makeLenses ''GLResources


type ResourceManager = RWST () [String] GLResources IO


