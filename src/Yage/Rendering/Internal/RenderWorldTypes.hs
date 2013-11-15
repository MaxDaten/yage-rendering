{-# LANGUAGE TemplateHaskell         #-}
module Yage.Rendering.Internal.RenderWorldTypes where

import Yage.Prelude
import Yage.Math

import Control.Monad.RWS.Strict
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
    { _worldEntities :: Set RenderEntity }

data RenderWorldState = RenderWorldState
    { _renderResources :: RenderWorldResources }


data RenderView = RenderView
    { _rvViewMatrix        :: !(M44 Float)
    , _rvProjectionMatrix  :: !(M44 Float)
    }


data ViewDefinition = ViewDefinition
    { _vdViewMatrix            :: !(M44 Float)
    , _vdProjectionMatrix      :: !(M44 Float)
    , _vdModelMatrix           :: !(M44 Float)
    , _vdNormalMatrix          :: !(M33 Float)
    , _vdRenderData            :: !RenderData
    , _vdUniformDef            :: !ShaderDefinition
    }

makeLenses ''RenderView
makeLenses ''RenderWorldEnv
makeLenses ''RenderWorldState
makeLenses ''RenderWorldResources
makeLenses ''ViewDefinition


type RenderWorld = RWST RenderWorldEnv () RenderWorldState IO
