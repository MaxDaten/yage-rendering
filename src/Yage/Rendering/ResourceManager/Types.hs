{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
module Yage.Rendering.ResourceManager.Types where

import           Yage.Prelude

import           Control.Monad.RWS
import           Data.Map.Strict

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
    requestRenderItem    :: (Ord iid)    => iid    -> rm iid


data RenderResources = RenderResources
    { _loadedShaders      :: (Map ShaderResource ShaderProgram)
    , _loadedVertexBuffer :: (Map Mesh (ModificationToken, VertexBufferObject, EBO))
    , _loadedVertexArrays :: (Map (Mesh, ShaderResource) VAO)
    , _loadedTextures     :: (Map TextureResource GL.TextureObject)
    , _loadedRenderbuffers:: (Map RenderbufferResource GL.RenderbufferObject)
    , _compiledFBOs       :: (Map String Framebuffer)
    }

data Resourceables = Resourceables
    { _entities      :: [RenderEntity]
    , _fbufferSpec   :: Maybe (String, FramebufferSpec TextureResource RenderbufferResource)
    -- , _worldLights   :: [RenderLight] 
    }


makeLenses ''Resourceables
makeLenses ''RenderResources



instance Monoid RenderResources where
    mappend (RenderResources sA vA aA tA fA rA) (RenderResources sB vB aB tB fB rB)
           = RenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB) (fA `union` fB) (rA `union` rB)
    mempty = RenderResources mempty mempty mempty mempty mempty mempty


newtype ResourceManager a = ResourceManager {unResourceManager :: RWST Resourceables () RenderResources IO a}
    deriving (MonadWriter (), MonadReader Resourceables, MonadState RenderResources, MonadRWS Resourceables () RenderResources, MonadIO, Monad)



