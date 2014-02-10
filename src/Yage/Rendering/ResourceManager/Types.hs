{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE FunctionalDependencies             #-}
module Yage.Rendering.ResourceManager.Types where

import           Yage.Prelude

import           Control.Monad.RWS

import qualified Graphics.Rendering.OpenGL as GL

import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec (VertexBufferObject)
import           Yage.Rendering.Backend.Framebuffer
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


type GLVertexbuffer        = (ModificationToken, VertexBufferObject, EBO)
type GLVertexArray         = VAO
type GLShader              = ShaderProgram
type GLTexture             = (GLBufferSpec, GL.TextureObject)
type GLRenderbuffer        = (GLBufferSpec, GL.RenderbufferObject)
type GLFramebuffer         = Framebuffer TextureResource RenderbufferResource


data GLResources = GLResources
    { _loadedShaders       :: (Map ShaderResource         GLShader       )
    , _loadedVertexBuffer  :: (Map Mesh                   GLVertexbuffer )
    , _loadedVertexArrays  :: (Map (Mesh, ShaderResource) GLVertexArray  )
    , _loadedTextures      :: (Map TextureResource        GLTexture      )
    , _loadedRenderbuffers :: (Map RenderbufferResource   GLRenderbuffer )
    , _compiledFBOs        :: (Map String                 GLFramebuffer  )
    }

makeLenses ''GLResources



--instance Monoid GLRenderResources where
--    mappend (GLRenderResources sA vA aA tA fA rA) (GLRenderResources sB vB aB tB fB rB)
--           = GLRenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB) (fA `union` fB) (rA `union` rB)
--    mempty = GLRenderResources mempty mempty mempty mempty mempty mempty


type ResourceManager = RWST () [String] GLResources IO


