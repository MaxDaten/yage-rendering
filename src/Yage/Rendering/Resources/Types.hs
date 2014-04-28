{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE ExistentialQuantification          #-}
module Yage.Rendering.Resources.Types where

import           Yage.Prelude
import           Yage.Lens


import           Control.Monad.RWS

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.GLUtil



--import           Yage.Rendering.Types
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


--instance Monoid GLRenderResources where
--    mappend (GLRenderResources sA vA aA tA fA rA) (GLRenderResources sB vB aB tB fB rB)
--           = GLRenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB) (fA `union` fB) (rA `union` rB)
--    mempty = GLRenderResources mempty mempty mempty mempty mempty mempty


type ResourceManager = RWST () [String] GLResources IO


