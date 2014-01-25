{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
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
import           GHC.Float                              (double2Float)

import           Control.Monad                          (mapM)
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

data RenderNode = RenderNode
    { _renderSubject :: RenderScene
    , _renderTarget  :: Maybe (String, FramebufferSpec TextureResource RenderbufferResource)
    --, _renderSettings  :: RenderSettings
    }
makeLenses ''RenderNode


type RenderSystem = RWST RenderSettings RenderLog RenderResources IO


class HasPipelineData a | a -> b where
    getPipelineData :: a -> b



runRenderSystem :: (MonadIO m) => RenderSystem () -> RenderSettings -> RenderResources -> m (RenderResources, RenderLog)
runRenderSystem sys settings res = io $ execRWST sys settings res


-- TODO individual settings
mkRenderSystem :: HasPipelineData a => a -> RenderPipeline s Rendering b c -> RenderSystem ()
mkRenderSystem units = do
    settings      <- ask
    renderers     <- mapM (\u -> mkSceneRenderer (u^.renderSubject) (u^.renderTarget)) units
    renderResult  <- io $ mapM (flip Renderer.runRenderer settings) renderers
    mapM_ tell (renderResult^..traverse._3)


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
