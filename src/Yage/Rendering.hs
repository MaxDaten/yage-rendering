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
    , module Vertex
    , module Mesh
    , module LinExport
    , module Framebuffer
    , module ResourceManager
    ) where

import           Yage.Prelude
import           Yage.Math
import           Yage.Geometry.Vertex                   as Vertex (Vertex)

import           Data.List                              (map, head)

import           Control.Monad.RWS

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.Renderer.Lenses as RendererExports
import           Yage.Rendering.Backend.Renderer.Types  as RendererExports (RenderConfig (..), RenderLog (..), ShaderDefinition)
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.RenderEntity            as RenderEntity
import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as ResourceManager (GLResources, initialGLRenderResources)
import           Yage.Rendering.ResourceManager         as Framebuffer (defaultFramebuffer)
import           Yage.Rendering.Types                   as Types
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
    , passGlobalUniforms :: PlainRec g
    , passEntityUniforms :: RenderEntity v -> PlainRec l
    , passGlobalTextures :: [TextureDefinition]
    , passPreRendering   :: Renderer ()
    , passPostRendering  :: Renderer ()
    }


class HasRenderEntities a vrec where
    renderEntities :: a -> [RenderEntity vrec]

type HasDeferredData a grec lrec srec 
    = ( ViableVertex (Vertex grec), ViableVertex (Vertex lrec), ViableVertex (Vertex srec)
      , HasRenderEntities a grec, HasRenderEntities a lrec, HasRenderEntities a srec)

runRenderSystem :: (MonadIO m) => RenderSystem () -> GLResources -> m (GLResources, RenderLog)
runRenderSystem sys res = io $ execRWST sys () (traceShow "runRenderSystem" res)


-- TODO individual settings
mkRenderSystem :: Renderer () -> RenderSystem ()
mkRenderSystem toRender = do
    (_, rlog) <- io $ Renderer.runRenderer toRender
    tell rlog


createDeferredRenderSystem :: forall g g' l l' s s' scene gv lv sv. 
                            ( HasPassUniforms g g', HasPassUniforms l l', HasPassUniforms s s'
                            , HasDeferredData scene gv lv sv ) 
                           => DeferredLightingDescr g g' gv l l' lv s s' sv -> scene -> RenderSystem ()
createDeferredRenderSystem DeferredLightingDescr{..} scene = do
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

        


    loadPipelineData :: (UniformFields (PlainRec g'), UniformFields (PlainRec l'), UniformFields (PlainRec s')) 
                     => ResourceManager (DeferredLightingData g' l' s')
    loadPipelineData = 
        let geoEnts         = renderEntities scene :: [RenderEntity gv]
            lightEnts       = renderEntities scene :: [RenderEntity lv]
            screenEnts      = renderEntities scene :: [RenderEntity sv]
            geoRs           = map toRenderEntity geoEnts
            lightRs         = map toRenderEntity lightEnts
            screenRs        = map toRenderEntity screenEnts
            
            geoShader       = passShader dfGeoPassDescr
            getGeoUniforms  = passEntityUniforms dfGeoPassDescr
            
            lightShader     = passShader dfLightingPassDescr
            getLightUniforms= passEntityUniforms dfLightingPassDescr
            
            screenShader    = passShader dfFinalScreen
            getScreenUnis   = passEntityUniforms dfFinalScreen 
            
        in do
            geos     <- forM geoRs    $ loadRenderEntity geoShader getGeoUniforms
            lights   <- forM lightRs  $ loadRenderEntity lightShader getLightUniforms
            screen   <- forM screenRs $ loadRenderEntity screenShader getScreenUnis
            return $ DeferredLightingData geos lights (head screen)



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
