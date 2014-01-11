{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.ResourceManager.Types where

import           Yage.Prelude

import           Control.Monad.RWS.Strict
import           Data.Map.Strict

import qualified Graphics.Rendering.OpenGL as GL

import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec (VertexBufferObject)
import           Yage.Rendering.Backend.Framebuffer
-- | container for all possible entities to render

data RenderResources = RenderResources
    { _loadedShaders      :: !(Map ShaderResource ShaderProgram)
    , _loadedVertexBuffer :: !(Map Mesh (ModificationToken, VertexBufferObject, EBO))
    , _loadedVertexArrays :: !(Map (Mesh, ShaderResource) VAO)
    , _loadedTextures     :: !(Map TextureResource GL.TextureObject)
    , _framebuffers       :: !(Map String Framebuffer)
    }

data Resourceables = Resourceables
    { _entities :: [RenderEntity]
    -- , _worldLights   :: [RenderLight] 
    }


makeLenses ''Resourceables
makeLenses ''RenderResources


type ResourceManager = RWST Resourceables () RenderResources IO

instance Monoid RenderResources where
    mappend (RenderResources sA vA aA tA fA) (RenderResources sB vB aB tB fB)
           = RenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB) (fA `union` fB)
    mempty = RenderResources mempty mempty mempty mempty mempty

