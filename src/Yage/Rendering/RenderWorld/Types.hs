{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.RenderWorld.Types where

import           Yage.Prelude

import           Control.Monad.RWS.Strict
import           Data.Map.Strict

import qualified Graphics.Rendering.OpenGL as GL

import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec (VertexBufferObject)
-- | container for all possible entities to render

data RenderResources = RenderResources
    { _loadedShaders      :: !(Map ShaderResource ShaderProgram)
    , _loadedVertexBuffer :: !(Map Mesh (ModificationToken,VertexBufferObject,EBO))
    , _loadedVertexArrays :: !(Map (Mesh, ShaderResource) VAO)
    , _loadedTextures     :: !(Map TextureResource GL.TextureObject)
    }

data RenderWorldEnv = RenderWorldEnv
    { _worldEntities :: [RenderEntity] }


makeLenses ''RenderWorldEnv
makeLenses ''RenderResources


type RenderWorld = RWST RenderWorldEnv () RenderResources IO

instance Monoid RenderResources where
    mappend (RenderResources sA vA aA tA) (RenderResources sB vB aB tB)
           = RenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB)
    mempty = RenderResources mempty mempty mempty mempty

