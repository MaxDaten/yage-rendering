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
    , module RendererExports
    , module RenderEntity
    
    , module Mesh
    , module Vertex
    , module Uniforms
    
    , module LinExport
    , module Framebuffer
    , module ResourceManager
    ) where

import           Yage.Prelude
import           Yage.Math

import           Control.Monad.RWS

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.Renderer.Lenses as RendererExports
import           Yage.Rendering.Backend.Renderer.Types  as RendererExports (RenderConfig (..), RenderLog (..))
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.RenderEntity            as RenderEntity
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as ResourceManager (GLResources, initialGLRenderResources)
import           Yage.Rendering.ResourceManager         as Framebuffer (defaultFramebuffer)
import           Yage.Rendering.Types                   as Types

import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.Uniforms                as Uniforms
import           Yage.Rendering.Vertex                  as Vertex

import           Yage.Rendering.Backend.Renderer.DeferredLighting


type RenderSystem = RWST () RenderLog GLResources IO



data DeferredLightingDescr gGlo gLoc grec
                           lGlo lLoc lrec
                           sGlo sLoc srec = DeferredLightingDescr
    { dfGeoPassDescr        :: PassDescr gGlo gLoc grec
    , dfLightingPassDescr   :: PassDescr lGlo lLoc lrec
    , dfFinalScreen         :: PassDescr sGlo sLoc srec
    }

 

data TargetFramebuffer =
      DefaultFramebuffer 
    | CustomFramebuffer (String, FramebufferSpec TextureResource RenderbufferResource)

data PassDescr g l v = PassDescr
    { passFBSpec         :: TargetFramebuffer
    , passShader         :: ShaderResource
    , passGlobalUniforms :: Uniforms g
    , passEntityUniforms :: RenderEntity v -> Uniforms l
    , passGlobalTextures :: [TextureDefinition]
    , passPreRendering   :: Renderer ()
    , passPostRendering  :: Renderer ()
    }


class ViableVertex (Vertex grec) => HasGeoData a grec where
    getGeoEntities  :: a -> [RenderEntity grec]
    getGeoEntities _ = []
    
class ViableVertex (Vertex lrec) => HasLightData a lrec where
    getLightEntities :: a -> [RenderEntity lrec]
    getLightEntities _ = []
    
    
class ViableVertex (Vertex srec) => HasScreen a srec where
    getScreen :: a -> RenderEntity srec


type HasDeferredData a geo lit scr = (HasGeoData a geo, HasLightData a lit, HasScreen a scr)

runRenderSystem :: (MonadIO m) => RenderSystem () -> GLResources -> m (GLResources, RenderLog)
runRenderSystem sys res = io $ execRWST sys () res


-- TODO individual settings
mkRenderSystem :: Renderer () -> RenderSystem ()
mkRenderSystem toRender = do
    (_, rlog) <- io $ Renderer.runRenderer toRender
    tell rlog


createDeferredRenderSystem :: forall gu gu' lu lu' su su' dd gv lv sv. 
                            ( HasPassUniforms gu gu', HasPassUniforms lu lu', HasPassUniforms su su'
                            , HasDeferredData dd gv lv sv ) 
                           => DeferredLightingDescr gu gu' gv lu lu' lv su su' sv -> dd -> RenderSystem ()
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


    loadFramebufferSetup PassDescr{..} = 
        FramebufferSetup 
            <$> case passFBSpec of
                CustomFramebuffer s -> requestFramebuffer s
                DefaultFramebuffer  -> pure defaultFramebuffer
            <*> requestShader passShader
            <*> pure passGlobalUniforms
            <*> forM passGlobalTextures makeTexAssignment
            <*> pure passPreRendering
            <*> pure passPostRendering

        


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


loadRenderEntity :: (ViableVertex (Vertex vr), UniformFields (PlainRec u))
                => ShaderResource 
                -> (RenderEntity vr -> PlainRec u)
                -> RenderEntity vr
                -> ResourceManager (RenderSet u)
loadRenderEntity withProgram entityUniforms ent = do
    rdef@RenderDefinition{_rdefData} <- pure $ ent^.entityRenderDef 
    RenderSet  <$> (requestRenderSet (_rdefData) withProgram)
               <*> (pure (entityUniforms ent))
               <*> (forM (_rdefTextures rdef) makeTexAssignment)
               <*> (pure (_rdefMode rdef))
               <*> (pure (fromIntegral $ dataCount _rdefData))


makeTexAssignment :: TextureDefinition -> ResourceManager TextureAssignment
makeTexAssignment tex =
    let ch = tex^.texChannel & _1 %~ fromIntegral
    in (,ch) . snd <$> requestTexture (tex^.texResource)


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
