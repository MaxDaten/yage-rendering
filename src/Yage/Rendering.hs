{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Yage.Rendering
    ( module Types
    , module Yage.Rendering
    , module Lenses
    
    , module Mesh
    , module Vertex
    , module Uniforms
    
    , module LinExport
    , module Framebuffer
    , module ResourceManager
    , module Pass
    ) where

import           Yage.Prelude
import           Yage.Math

import           Control.Monad.RWS

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.RenderPass      as Pass
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as ResourceManager (GLResources, initialGLRenderResources)
import           Yage.Rendering.Types                   as Types

import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.Uniforms                as Uniforms
import           Yage.Rendering.Vertex                  as Vertex


type RenderSystem = RWST () RenderLog GLResources IO



runRenderSystem :: (MonadIO m) => RenderSystem () -> GLResources -> m (GLResources, RenderLog)
runRenderSystem sys res = io $ execRWST sys () res


-- TODO individual settings
mkRenderSystem :: Renderer () -> RenderSystem ()
mkRenderSystem toRender = do
    (_, rlog) <- io $ Renderer.runRenderer toRender
    tell rlog
{--
outerRim = flip runRenderSystem res $ do
    runRenderPass geoDescr geos
    runRenderPass lightPass lights
    runRenderPass geoDescr screen
--}

runRenderPass :: (Renderable ent vr, UniformFields (Uniforms globals), UniformFields (Uniforms locals) ) 
              => PassDescr ent globals locals -> [ent] -> RenderSystem ()
runRenderPass passDescr@PassDescr{..} entities = do
    (setup, rSets) <- managePassResoures
    mkRenderSystem $ mkRenderPass setup rSets

    where
    managePassResoures = do
        res <- get 
        (results, res', reslog) <- runResourceManager res $ 
            (,) <$> (requestFramebufferSetup passDescr)
                <*> (forM entities $ \e -> requestRenderSet passShader (passEntityUniforms e) (renderDefinition e))
        scribe resourceLog reslog
        put res'
        return results


{--

createDeferredRenderSystem :: forall geoPass geo geoGlobal geoLocal lightPass light lightGlobal lightLocal screenPass screen screenGlobal screenLocal dd.
    ( geoPass    ~ PassDescr geo geoGlobal geoLocal
    , lightPass  ~ PassDescr light lightGlobal lightLocal
    , screenPass ~ PassDescr screen screenGlobal screenLocal ) =>
    DeferredLightingDescr geoPass lightPass screenPass -> dd -> RenderSystem ()
createDeferredRenderSystem DeferredLightingDescr{..} dd = do
    ((geoSetup, {-- lightSetup,--} screenSetup, pipelineData), res, resLog) <- loadPipelineResources =<< get
    put res
    scribe resourceLog resLog
    mkRenderSystem $ 
        deferredLighting (mkGeoPass geoSetup) (mkScreenPass screenSetup) pipelineData

    where

    loadPipelineResources res = 
        runResourceManager res $ 
           (,,) <$> loadFramebufferSetup dfGeoPassDescr
                 -- <*> loadFramebufferSetup dfLightingPassDescr
                <*> loadFramebufferSetup dfFinalScreen
                <*> loadPipelineData


    loadPipelineData :: (UniformFields (PlainRec gu'), UniformFields (PlainRec lu'), UniformFields (PlainRec su')) 
                     => ResourceManager (DeferredLightingData gu' lu' su')
    loadPipelineData = 
        let geoEnts         = dd & getGeoEntities    :: [RenderEntity gv]
            lightEnts       = dd & getLightEntities  :: [RenderEntity lv]
            screen          = dd & getScreen         :: RenderEntity sv
            
            geoShader       = passShader dfGeoPassDescr
            getGeoUniforms  = passEntityUniforms dfGeoPassDescr
            
            lightShader     = passShader dfLightingPassDescr
            getLightUniforms= passEntityUniforms dfLightingPassDescr
            
            screenShader    = passShader dfFinalScreen
            getScreenUnis   = passEntityUniforms dfFinalScreen 
            
        in do
            geos   <- forM geoEnts $ loadRenderEntity geoShader getGeoUniforms
            lights <- forM lightEnts  $ loadRenderEntity lightShader getLightUniforms
            scr    <- loadRenderEntity screenShader getScreenUnis screen
            return $ DeferredLightingData geos lights scr


--}        

---------------------------------------------------------------------------------------------------

--newtype ZOrderedRenderable = ZOrderedRenderable RenderEntity
--    deriving (Typeable, Renderable)

--instance Eq ZOrderedRenderable where
--    a == b =
--        let aZ = (renderTransformation a)^.transPosition._z
--            bZ = (renderTransformation b)^.transPosition._z
--        in aZ == bZ

--instance Ord ZOrderedRenderable where
--    compare a b =
--        let aZ = (renderTransformation a)^.transPosition._z
--            bZ = (renderTransformation b)^.transPosition._z
--        in compare aZ bZ

--newtype PositionOrderedEntity = PositionOrderedEntity { unPositionOrderedEntity :: RenderEntity }
--    deriving (Typeable, Renderable)


--instance Eq PositionOrderedEntity where
--    a == b = (renderTransformation a)^.transPosition == (renderTransformation b)^.transPosition

--instance Ord PositionOrderedEntity where
--    compare a b = ((renderTransformation a)^.transPosition) `compare` ((renderTransformation b)^.transPosition)
