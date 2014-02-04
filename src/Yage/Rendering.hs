{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Yage.Rendering
    ( (!=), shaderEnv, RenderResources
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
    ) where

import           Yage.Prelude
import           Yage.Math

import           Data.List                              (map)

import           Control.Monad.RWS

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.Renderer.Lenses as RendererExports
import           Yage.Rendering.Backend.Renderer.Types  as RendererExports (RenderConfig (..), RenderLog (..), RenderSettings (..), RenderTarget (..), ShaderDefinition)
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.RenderEntity            as RenderEntity
import           Yage.Rendering.RenderScene             as RenderScene
import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.VertexSpec              as VertexSpec ((@=), vPosition, vNormal, vColor, vTexture)
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as Framebuffer (defaultFramebuffer)
import           Yage.Rendering.Types                   as Types
import           Yage.Rendering.Backend.RenderPipeline
import           Yage.Rendering.Backend.Renderer.DeferredLighting


type RenderResources = GLRenderResources
type RenderSystem = RWST RenderSettings RenderLog GLRenderResources IO



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



runRenderSystem :: (MonadIO m) => RenderSystem () -> RenderSettings -> RenderResources -> m (RenderResources, RenderLog)
runRenderSystem sys settings res = io $ execRWST sys settings res


-- TODO individual settings
mkRenderSystem :: (WithSetup s Renderer) => a -> RenderPipeline s Renderer a b -> RenderSystem ()
mkRenderSystem toRender pipeline = do
    settings      <- ask
    (_, rlog)     <- io $ flip Renderer.runRenderer settings $ runPipeline pipeline toRender
    tell rlog


createDeferredRenderSystem :: DeferredLightingDefinition -> RenderScene -> RenderSystem ()
createDeferredRenderSystem DeferredLightingDefinition{..} scene = do
    ((geoSetup, lightSetup, pipelineData), res) <- loadPipelineResources =<< get
    put res
    mkRenderSystem pipelineData $ deferredLighting (mkGeoPass geoSetup) (mkLightPass lightSetup)

    where

    loadPipelineResources res = 
        runResourceManager res . unGLRM $ 
            (,,) <$> loadFramebufferSetup dfGeoPassDefinition
                 <*> loadFramebufferSetup dfLightingPassDefinitin
                 <*> loadPipelineData


    loadFramebufferSetup :: PassDefinition
                         -> GLResourceManager FramebufferSetup
    loadFramebufferSetup PassDefinition{..} = 
        FramebufferSetup 
            <$> requestFramebuffer fbSpec
            <*> requestShader fbShader
            <*> pure (fbGlobalUniforms scene) -- renderView to this here plz
            -- <*> pure []
            <*> pure (return ())
            <*> pure (return ())

        


    loadPipelineData :: GLResourceManager DeferredLightingData
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
                -> GLResourceManager RenderData
loadRenderEntity withProgram entityUniforms ent =
    let rdef    = ent^.entityRenderDef
    in
    RenderData <$> requestRenderItem (rdef^.rdefData, withProgram)
               <*> pure (entityUniforms ent)
               <*> forM (rdef^.rdefTextures) makeTexAssignment
               <*> pure (rdef^.rdefMode)
               <*> pure (meshTriangleCount (rdef^.rdefData))
    
    where 
    
    makeTexAssignment tex =
        let ch = tex^.texChannel & _1 %~ fromIntegral
        in (,ch) <$> requestTexture (tex^.texResource)

{--

        getProjection :: Camera -> RenderTarget -> M44 Float
        getProjection (Camera3D _ fov) target =
            let V2 w h      = fromIntegral <$> target^.targetSize
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
                aspect      = (w/h)
            in projectionMatrix fov aspect n f


    createUniformDef ent = 
        let trans        = ent^.entityTransformation
            scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
            modelM       = transM !*! scaleM
            -- TODO rethink the normal matrix here
            normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
        in do
            "model_matrix"      != modelM
            "normal_matrix"     != normalM

let albedoTex       = TextureBuffer "gbuffer-albedo"       $ GLTextureSpec Texture2D RGB' (800, 600)
            normalTex       = TextureBuffer "gbuffer-normal"       $ GLTextureSpec Texture2D RGB' (800, 600)
            specularTex     = TextureBuffer "gbuffer-specul"       $ GLTextureSpec Texture2D RGB' (800, 600)
            glossyTex       = TextureBuffer "gbuffer-glossy"       $ GLTextureSpec Texture2D RGB' (800, 600)
            depthStencilTex = TextureBuffer "gbuffer-depthStencil" $ GLTextureSpec Texture2D RGB' (800, 600)
            geoProgram      = ShaderResource "geopass.vert" "geopass.frag"

            geoFBODef       = FramebufferResources
                                { fbSpec           = ( "geo-fbo"
                                                     , colorAttachment (TextureTarget Texture2D albedoTex   0)  <>  
                                                       colorAttachment (TextureTarget Texture2D normalTex   0)  <>
                                                       colorAttachment (TextureTarget Texture2D specularTex 0)  <>
                                                       colorAttachment (TextureTarget Texture2D glossyTex   0)  <>
                                                       depthStencilAttachment (TextureTarget Texture2D depthStencilTex 0)
                                                     )
                                , fbShader         = geoProgram
                                }

            lightTex        = TextureBuffer "lbuffer-light" $ GLTextureSpec Texture2D RGB' (800, 600)
            lightingProgram = ShaderResource "lightpass.vert" "lightpass.frag"

            lightFBODef     = FramebufferResources
                                { fbSpec           = ( "light-fbo"
                                                     , colorAttachment        (TextureTarget Texture2D lightTex   0)   <>
                                                       depthStencilAttachment (TextureTarget Texture2D depthStencilTex 0)
                                                     )
                                , fbShader         = lightingProgram
                                }

--}

{--

mkSceneRenderer :: RenderScene -> Maybe (String, FramebufferSpec TextureResource RenderbufferResource) -> RenderSystem (Renderer ())
mkSceneRenderer scene mfbo = do
    (renderView, viewEntities) <- prepareResources
    Just framebuffer           <- case mfbo of
                                    Just (fboId, _) -> use $ compiledFBOs.at fboId
                                    Nothing         -> return $ Just defaultFramebuffer
    return $ renderFrame renderView viewEntities framebuffer

    where

        prepareResources :: RenderSystem (RenderView, [ViewEntity])
        prepareResources = do 
            renderSettings  <- ask
            let renderTarget        = renderSettings^.reRenderTarget

                viewMatrix          = scene^.sceneCamera.cameraHandle.to camMatrix
                projMatrix          = getProjection (scene^.sceneCamera) renderTarget
                renderView          = RenderView viewMatrix projMatrix
                entities            = map toRenderEntity $ scene^.sceneEntities
                resourceables       = Resourceables entities mfbo

--------- SUCKAGE!!!!!!!!!!!!!!!!!------------------
            loadedRes <- runResourceManager resourceables =<< get
            put loadedRes
            return (renderView, map (toViewEntity renderView loadedRes) entities)

        getProjection :: Camera -> RenderTarget -> M44 Float
        getProjection (Camera3D _ fov) target =
            let V2 w h      = fromIntegral <$> target^.targetSize
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
                aspect      = (w/h)
            in projectionMatrix fov aspect n f -- TODO : move zfar/znear

        getProjection (Camera2D _) target =
            let V2 w h      = fromIntegral <$> target^.targetSize
                V2 x y      = fromIntegral <$> target^.targetXY
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
            in orthographicMatrix x w y h n f

---------------------------------------------------------------------------------------------------



toViewEntity :: RenderView -> RenderResources -> RenderEntity -> ViewEntity
toViewEntity rview@RenderView{..} RenderResources{..} ent =
    let scaleM       = kronecker . point $ ent^.entityTransformation.transScale
        transM       = mkTransformation (ent^.entityTransformation.transOrientation) (ent^.entityTransformation.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in ViewEntity
        { _vdMVPMatrix         = _rvProjectionMatrix !*! _rvViewMatrix !*! modelM
        , _vdModelViewMatrix   = _rvViewMatrix !*! modelM
        , _vdModelMatrix       = modelM
        , _vdNormalMatrix      = normalM
        , _vdRenderData        = getRenderData $ ent^.entityRenderDef
        , _vdUniformDef        = (ent^.entityRenderDef.rdefProgram._2, uniformEnv)
        }
    
    where
        
        getRenderData renderDef =
            let rData = renderDef^.rdefData
                rProg = renderDef^.rdefProgram^._1
                rTexs  = renderDef^.rdefTextures
            in RenderData
                { _vao           = _loadedVertexArrays^.at (rData, rProg) ^?!_Just
                , _shaderProgram = _loadedShaders^.at rProg ^?!_Just
                , _texObjs       = map makeTexObj rTexs
                , _elementCount  = meshTriangleCount rData
                , _drawMode      = renderDef^.rdefMode
                }
        
        makeTexObj tex =
            let obj = _loadedTextures^.at (tex^.texResource) ^?!_Just
                ch  = tex^.texChannel & _1 %~ fromIntegral
            in (obj, ch)
        
        uniformEnv = ShaderEnv
            { _seProgram           = _loadedShaders^.at (ent^.entityRenderDef^.rdefProgram._1) ^?!_Just
            , _seViewDef           = undefined -- TODO init is currently in renderer (awkward!!)
            , _seView              = rview
            }

--}

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
