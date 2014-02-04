{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
module Yage.Rendering.ResourceManager.Types where

import           Yage.Prelude

import           Control.Monad.State (MonadState)
import           Control.Monad.Trans.State.Strict
import           Data.Map.Strict
import           Control.Concurrent.STM (TVar)

import qualified Graphics.Rendering.OpenGL as GL

import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec (VertexBufferObject)
import           Yage.Rendering.Backend.Framebuffer
-- | container for all possible entities to render


class (Monad rm) => ResourceManaging rm where
    requestFramebuffer   :: (Ord fbId)   => fbId   -> rm fbhandler
    requestTexture       :: (Ord texId)  => texId  -> rm texhandler
    requestShader        :: (Ord shId)   => shId   -> rm shhandler
    requestGeometry      :: (Ord geoId)  => geoId  -> rm geohandler
    -- | RenderItem is like the combound of a render-program and render-data
    --  e.g. shader and geometry 
    requestRenderItem    :: (Ord geoId, Ord shId) => (geoId, shId) -> rm iid


type VBO_RHandle           = TVar (ModificationToken, VertexBufferObject, EBO)
type VAO_RHandle           = TVar VAO
type Shader_RHandle        = TVar ShaderProgram
type Texture_RHandle       = TVar GL.TextureObject
type Renderbuffer_RHandle  = TVar GL.RenderbufferObject
type Framebuffer_RHandle   = TVar Framebuffer

data GLRenderResources = GLRenderResources
    { _loadedShaders      :: (Map ShaderResource         Shader_RHandle)
    , _loadedVertexBuffer :: (Map Mesh                   VBO_RHandle)
    , _loadedVertexArrays :: (Map (Mesh, ShaderResource) VAO_RHandle)
    , _loadedTextures     :: (Map TextureResource        Texture_RHandle)
    , _loadedRenderbuffers:: (Map RenderbufferResource   Renderbuffer_RHandle)
    , _compiledFBOs       :: (Map String                 Framebuffer_RHandle)
    }

makeLenses ''GLRenderResources



instance Monoid GLRenderResources where
    mappend (GLRenderResources sA vA aA tA fA rA) (GLRenderResources sB vB aB tB fB rB)
           = GLRenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB) (fA `union` fB) (rA `union` rB)
    mempty = GLRenderResources mempty mempty mempty mempty mempty mempty


type ResourceManager res a = StateT res IO a

newtype GLResourceManager a = GLResourceManager { unGLRM :: ResourceManager GLRenderResources a }
    deriving ( Monad, Functor, Applicative, MonadIO
             , MonadState GLRenderResources
             )


