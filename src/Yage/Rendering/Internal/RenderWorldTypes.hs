{-# LANGUAGE TemplateHaskell         #-}
module Yage.Rendering.Internal.RenderWorldTypes where

import Yage.Prelude
import Yage.Math

import Control.Monad.RWS.Strict
import Data.Map.Strict
import Control.Lens

import Linear

import qualified   Graphics.Rendering.OpenGL       as GL

import Yage.Rendering.Types

-- | container for all possible entities to render

data RenderWorldResources = RenderWorldResources
    { _loadedShaders                         :: !(Map ShaderResource ShaderProgram)
    , _loadedVertexBuffer                    :: !(Map (Mesh, ShaderResource) VAO)
    , _loadedTextures                        :: !(Map TextureResource GL.TextureObject)
    }

data RenderWorldEnv = RenderWorldEnv
    { _worldEntities :: [RenderEntity] }

data RenderWorldState = RenderWorldState
    { _renderResources :: RenderWorldResources }


makeLenses ''RenderWorldEnv
makeLenses ''RenderWorldState
makeLenses ''RenderWorldResources



type RenderWorld = RWST RenderWorldEnv () RenderWorldState IO

instance Monoid RenderWorldResources where
    mappend (RenderWorldResources sA vA tA) (RenderWorldResources sB vB tB) 
           = RenderWorldResources (union sA sB) (union vA vB) (union tA tB)
    mempty = RenderWorldResources mempty mempty mempty

initialRenderWorldState = RenderWorldState mempty
