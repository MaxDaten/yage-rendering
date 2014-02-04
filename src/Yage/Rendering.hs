{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}

module Yage.Rendering
    ( (!=), shaderEnv
    , module Types
    , module Yage.Rendering
    , module Lenses
    , module RendererExports
    , module RenderEntity
    , module RenderScene
    , module VertexSpec
    , module Mesh
    , module LinExport
    , module Framebuffer
    , module ResourceManager
    ) where

import           Yage.Prelude
import           Yage.Math

import           Data.List                              (map)

import           Control.Monad.RWS

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.Renderer.Lenses as RendererExports
import           Yage.Rendering.Backend.Renderer.Types  as RendererExports (RenderConfig (..), RenderLog (..), ShaderDefinition)
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.RenderEntity            as RenderEntity
import           Yage.Rendering.RenderScene             as RenderScene
import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.VertexSpec              as VertexSpec ((@=), vPosition, vNormal, vColor, vTexture)
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as ResourceManager (initialGLRenderResources)
import           Yage.Rendering.ResourceManager         as Framebuffer (defaultFramebuffer)
import           Yage.Rendering.Types                   as Types
import           Yage.Rendering.Backend.RenderPipeline
import           Yage.Rendering.Backend.Renderer.DeferredLighting


type RenderSystem = RWST () RenderLog GLResources IO



data DeferredLightingDefinition = DeferredLightingDefinition
    { dfGeoPassDefinition       :: PassDefinition -- see Rendering
    , dfLightingPassDefinitin   :: PassDefinition
    }

 


data PassDefinition = PassDefinition
    { fbSpec           :: (String, FramebufferSpec TextureResource RenderbufferResource)
    , fbShader         :: ShaderResource
    , fbGlobalUniforms :: RenderScene    -> ShaderDefinition ()
    , fbEntityUniforms :: RenderEntity   -> ShaderDefinition () 
    }



runRenderSystem :: (MonadIO m) => RenderSystem () -> GLResources -> m (GLResources, RenderLog)
runRenderSystem sys res = io $ execRWST sys () res


-- TODO individual settings
mkRenderSystem :: (WithSetup s Renderer) => a -> RenderPipeline s Renderer a b -> RenderSystem ()
mkRenderSystem toRender pipeline = do
    (_, rlog)     <- io $ Renderer.runRenderer $ runPipeline pipeline toRender
    tell rlog


createDeferredRenderSystem :: DeferredLightingDefinition -> RenderScene -> RenderSystem ()
createDeferredRenderSystem DeferredLightingDefinition{..} scene = do
    ((geoSetup, lightSetup, pipelineData), res) <- loadPipelineResources =<< get
    put res
    mkRenderSystem pipelineData $ deferredLighting (mkGeoPass geoSetup) (mkLightPass lightSetup)

    where

    loadPipelineResources res = 
        runResourceManager res $ 
            (,,) <$> loadFramebufferSetup dfGeoPassDefinition
                 <*> loadFramebufferSetup dfLightingPassDefinitin
                 <*> loadPipelineData


    loadFramebufferSetup :: PassDefinition
                         -> ResourceManager FramebufferSetup
    loadFramebufferSetup PassDefinition{..} = 
        FramebufferSetup 
            <$> requestFramebuffer fbSpec
            <*> requestShader fbShader
            <*> pure (fbGlobalUniforms scene) -- renderView to this here plz
            -- <*> pure []
            <*> pure (return ())
            <*> pure (return ())

        


    loadPipelineData :: ResourceManager DeferredLightingData
    loadPipelineData = 
        let sceneEnts       = scene^.sceneEntities
            rEntities       = map toRenderEntity sceneEnts  -- TODO Filter out items with forward shader
            geoShader       = fbShader dfGeoPassDefinition
            lightShader     = fbShader dfLightingPassDefinitin
            getGeoUniforms  = fbEntityUniforms dfGeoPassDefinition
        in do
            geos     <- forM rEntities $ loadRenderEntity geoShader getGeoUniforms
            lights   <- return []
            return $ DeferredLightingData geos lights



loadRenderEntity :: ShaderResource 
                -> (RenderEntity -> ShaderDefinition ())
                -> RenderEntity 
                -> ResourceManager RenderData
loadRenderEntity withProgram entityUniforms ent =
    let rdef    = ent^.entityRenderDef
    in
    RenderData <$> requestRenderItem (rdef^.rdefData) withProgram
               <*> pure (entityUniforms ent)
               <*> forM (rdef^.rdefTextures) makeTexAssignment
               <*> pure (rdef^.rdefMode)
               <*> pure (meshTriangleCount (rdef^.rdefData))
    
    where 
    
    makeTexAssignment tex =
        let ch = tex^.texChannel & _1 %~ fromIntegral
        in (,ch) <$> requestTexture (tex^.texResource)


---------------------------------------------------------------------------------------------------

newtype ZOrderedRenderable = ZOrderedRenderable RenderEntity
    deriving (Typeable, Renderable)

instance Eq ZOrderedRenderable where
    a == b =
        let aZ = (renderTransformation a)^.transPosition._z
            bZ = (renderTransformation b)^.transPosition._z
        in aZ == bZ

instance Ord ZOrderedRenderable where
    compare a b =
        let aZ = (renderTransformation a)^.transPosition._z
            bZ = (renderTransformation b)^.transPosition._z
        in compare aZ bZ

newtype PositionOrderedEntity = PositionOrderedEntity { unPositionOrderedEntity :: RenderEntity }
    deriving (Typeable, Renderable)


instance Eq PositionOrderedEntity where
    a == b = (renderTransformation a)^.transPosition == (renderTransformation b)^.transPosition

instance Ord PositionOrderedEntity where
    compare a b = ((renderTransformation a)^.transPosition) `compare` ((renderTransformation b)^.transPosition)
