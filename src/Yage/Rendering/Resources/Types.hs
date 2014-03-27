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

-- | container for all possible entities to render


--class (Monad rm, Ord def) => ResourceManaging rm def res | rm -> res where
--    requestResource :: def -> rm res
--    --requestFramebuffer   :: (Ord fbId)   => (fbId, a) -> rm fbhandler
--    --requestTexture       :: (Ord texId)  => texId  -> rm texhandler
--    --requestRenderbuffer  :: (Ord buffId) => buffId -> rm buffhandler
--    --requestShader        :: (Ord shId)   => shId   -> rm shhandler
--    --requestGeometry      :: (Ord geoId)  => geoId  -> rm geohandler
--    ---- | RenderItem is like the combound of a render-program and render-data
--    ----  e.g. shader and geometry 
--    --requestRenderItem    :: (Ord geoId, Ord shId) => (geoId, shId) -> rm iid

type GLVertexbuffer        = (MeshHash, GL.BufferObject)
type GLElementbuffer       = (MeshHash, GL.BufferObject)
type GLVertexArray         = VAO
type GLShader              = ShaderProgram
type GLTexture             = (GLBufferSpec, GL.TextureObject)
type GLRenderbuffer        = (GLBufferSpec, GL.RenderbufferObject)
type GLFramebuffer         = GL.FramebufferObject


data GLResources = GLResources
    { _loadedShaders       :: (Map ShaderResource           GLShader            )
    , _loadedVertexBuffer  :: (Map MeshId                   GLVertexbuffer      )
    , _loadedVertexArrays  :: (Map (MeshId, ShaderResource) GLVertexArray       )
    , _loadedElementBuffer :: (Map MeshId                   GLElementbuffer     )
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


