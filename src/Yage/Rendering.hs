{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering
    ( (!=), shaderEnv, RenderResources
    , module Types
    , module Yage.Rendering
    , module Yage.Rendering.Lenses
    , module RendererExports
    , module RenderEntity
    ) where

import           Yage.Prelude

import           Data.List                             (map)

import           Control.Monad.RWS

import           Linear

import           Yage.Rendering.Backend.Renderer       as Renderer
import           Yage.Rendering.Backend.Renderer.Types as RendererExports 
                                                          (RenderConfig (..), RenderTarget (..)
                                                            , RenderLog (..), RenderSettings(..)
                                                            , ShaderDefinition)
import           Yage.Rendering.Backend.Renderer.Lenses as RendererExports
import           Yage.Rendering.Lenses
import           Yage.Rendering.RenderWorld
import           Yage.Rendering.RenderEntity           as RenderEntity
import           Yage.Rendering.RenderScene            as RenderScene
import           Yage.Rendering.Types                  as Types

data RenderUnit = RenderUnit
    { _renderSubject   :: RenderScene
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
                aspect      = (w/h)
            in projectionMatrix fov aspect 0.1 100.0 -- TODO : move zfar/znear
        
        getProjection (Camera2D _) target = 
            let (w, h)      = fromIntegral <$$> target^.targetSize
            in orthographicMatrix 0.0 w 0.0 h 0.5 100.0 -- TODO magic numbers to camera2d

---------------------------------------------------------------------------------------------------

newtype ZOrderedRenderable = ZOrderedRenderable RenderEntity
    deriving (Typeable, Renderable)

instance Eq ZOrderedRenderable where
    a == b =
        let aZ = renderPosition a ^._z
            bZ = renderPosition b ^._z
        in aZ == bZ

instance Ord ZOrderedRenderable where
    compare a b =
        let aZ = renderPosition a ^._z
            bZ = renderPosition b ^._z
        in compare aZ bZ

newtype PositionOrderedEntity = PositionOrderedEntity { unPositionOrderedEntity :: RenderEntity }
    deriving (Typeable, Renderable)


instance Eq PositionOrderedEntity where
    a == b = renderPosition a == renderPosition b

instance Ord PositionOrderedEntity where
    compare a b = renderPosition a `compare` renderPosition b
