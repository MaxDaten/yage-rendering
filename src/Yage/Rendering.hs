{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
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

    ) where

import           Data.List                              (map)
import           GHC.Float                              (double2Float)
import           Yage.Prelude

import           Control.Monad.RWS

import           Linear                                 as Linear

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.Renderer.Lenses as RendererExports
import           Yage.Rendering.Backend.Renderer.Types  as RendererExports (RenderConfig (..), RenderLog (..), RenderSettings (..), RenderTarget (..), ShaderDefinition)
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.RenderEntity            as RenderEntity
import           Yage.Rendering.RenderScene             as RenderScene
import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.VertexSpec              as VertexSpec ((@=), vPosition, vNormal, vColor, vTexture)
import           Yage.Rendering.RenderWorld
import           Yage.Rendering.Types                   as Types

data RenderUnit = RenderUnit
    { _renderSubject :: RenderScene
    --, _renderSettings  :: RenderSettings
    }
makeLenses ''RenderUnit


type RenderSystem = RWST RenderSettings RenderLog RenderResources IO


-- TODO individual settings
runRenderSystem :: (MonadIO m) => [RenderUnit] -> RenderSettings -> RenderResources -> m (RenderResources, RenderLog)
runRenderSystem units settings res = do
    (res', rlog) <- io $ execRWST theSystem settings res
    return (res', rlog)
    where
        theSystem = do
            renderInstructions <- sequence_ <$> mapM prepareSceneRenderer (units^..traverse.renderSubject)
            (_, _, rlog) <- io $ Renderer.runRenderer renderInstructions settings
            tell rlog


prepareSceneRenderer :: RenderScene -> RenderSystem (Renderer ())
prepareSceneRenderer scene = do
    renderSettings <- ask
    renderResources <- get
    let renderTarget        = renderSettings^.reRenderTarget

        viewMatrix          = scene^.sceneCamera.cameraHandle.to camMatrix
        projMatrix          = getProjection (scene^.sceneCamera) renderTarget
        renderView          = RenderView viewMatrix projMatrix

        worldEnv            = RenderWorldEnv $ map toRenderEntity $ scene^.sceneEntities

    (viewDefs, renderRes') <- io $ runRenderWorld renderView worldEnv renderResources
    put renderRes'
    return $ renderFrame renderView viewDefs
    where
        getProjection :: Camera -> RenderTarget -> M44 Float
        getProjection (Camera3D _ fov) target =
            let (w, h)      = fromIntegral <$$> target^.targetSize
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
                aspect      = (w/h)
            in projectionMatrix fov aspect n f -- TODO : move zfar/znear

        getProjection (Camera2D _) target =
            let (w, h)      = fromIntegral <$$> target^.targetSize
                (x, y)      = fromIntegral <$$> target^.targetXY
                (n, f)      = double2Float <$$> (target^.targetZNear, target^.targetZFar)
            in orthographicMatrix x w y h n f

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
