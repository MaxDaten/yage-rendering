{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ImpredicativeTypes      #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ExistentialQuantification         #-}
module Yage.Rendering.RenderWorld where

import Yage.Prelude
import Yage.Math

import Data.Map as M hiding (map)
import Data.Set as S hiding (map)
import Data.List
import Data.Maybe (fromJust)

import Control.Monad.State
import Control.Lens

import Linear

import qualified   Graphics.Rendering.OpenGL       as GL

import Yage.Rendering.Types


---------------------------------------------------------------------------------------------------

-- | container for all possible entities to render

data RenderResources = RenderResources
    { _loadedShaders                         :: !(Map ShaderResource ShaderProgram)
    , _loadedVertexBuffer                    :: !(Map Mesh VAO)
    , _loadedTextures                        :: !(Map TextureDefinition GL.TextureObject)
    }


data RenderWorldState = RenderWorldState
    { _worldEntities   :: Set RenderEntity
    , _renderResources :: RenderResources
    }



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
makeLenses ''RenderWorldState
makeLenses ''RenderResources
makeLenses ''ViewDefinition
type RenderWorld = StateT RenderWorldState IO



processRenderView :: RenderView -> RenderWorld [ViewDefinition]
-- process all entities, load render resources
-- generate list of currently contributing entities in view (intermediates)
-- send them to renderer
processRenderView view = do
    prepareResources
    res <- use renderResources
    ents <- findContributingEntities
    return $ map (toViewDefinition view res) ents



findContributingEntities :: RenderWorld [RenderEntity]
findContributingEntities = uses worldEntities S.toList



toViewDefinition :: RenderView -> RenderResources -> RenderEntity -> ViewDefinition
toViewDefinition RenderView{..} RenderResources{..} RenderEntity{..} = 
    let scaleM       = kronecker . point $ eScale
        transM       = mkTransformation eOrientation ePosition
        modelM       = transM !*! scaleM
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM))^?!_Just 
    in ViewDefinition
        { _vdViewMatrix        = _rvViewMatrix
        , _vdProjectionMatrix  = _rvProjectionMatrix
        , _vdModelMatrix       = modelM
        , _vdNormalMatrix      = normalM
        , _vdRenderData        = getRenderData renderDef
        , _vdUniformDef        = snd . def'program $ renderDef
        }
    where
        getRenderData RenderDefinition{..} = 
            let makeTexObj tex = ((_loadedTextures^.at tex)^?!_Just, (tex^._texChannel & _1 %~ fromIntegral))
            in RenderData
                { vao           = undefined -- fromJust $ loadedVertexBuffer^.at def'data
                , shaderProgram = _loadedShaders^.at (def'program^._1) ^?!_Just
                , texObjs       = map makeTexObj def'textures
                , triangleCount = 0
                }



prepareResources :: RenderWorld ()
prepareResources = use worldEntities >>= mapM_ (loadRenderResourcesFor . renderDef) . S.toList



loadRenderResourcesFor :: RenderDefinition -> RenderWorld ()
loadRenderResourcesFor RenderDefinition{..} = do
    res <- use renderResources
    {--
    --}
    unless (res^.loadedVertexBuffer.contains def'data) $ 
        undefined -- renderResources.loadedVertexBuffer.at def'data ?= loadVertexBuffer def'data
    
    unless (res^.loadedShaders.contains (def'program^._1)) $ 
        renderResources.loadedShaders.at (def'program^._1) ?= loadShader (def'program^._1)
    
    forM_ def'textures $ \tex ->
        unless (res^.loadedTextures.contains tex) $
            renderResources.loadedTextures.at tex ?= loadTexture tex
    where
        loadVertexBuffer :: Mesh -> VAO
        loadVertexBuffer = undefined
        
        loadShader :: ShaderResource -> ShaderProgram
        loadShader = undefined

        loadTexture :: TextureDefinition -> GL.TextureObject
        loadTexture = undefined


