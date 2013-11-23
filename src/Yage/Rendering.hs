{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering
    ( (!=), shaderEnv
    , module Types
    , module Yage.Rendering
    , module Yage.Rendering.Lenses
    , module RendererExports
    , module RenderEntity
    ) where

import           Yage.Prelude

import           Data.List                             (map)
import           Control.Monad.RWS.Strict

import           Linear

import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.Backend.Renderer.Types as RendererExports 
                                                          (RenderConfig (..), RenderTarget (..), RenderLog (..))
import           Yage.Rendering.Lenses
import           Yage.Rendering.RenderWorld            hiding (renderResources)
import           Yage.Rendering.RenderEntity           as RenderEntity
import           Yage.Rendering.RenderScene            as RenderScene
import           Yage.Rendering.Types                  as Types


data RenderUnit = RenderUnit
    { _renderResources :: RenderWorldState
    , _renderSettings  :: RenderEnv
    , _lastRenderLog   :: RenderLog
    }

makeLenses ''RenderUnit

type GuiModel = Int

data RenderSubjects = RenderSubjects
    { _rSysRenderScene :: !RenderScene 
    , _rSysGuiModel    :: !GuiModel
    }

makeLenses ''RenderSubjects

type RenderSystem = RWST RenderSubjects () RenderUnit IO



initialRenderUnit :: RenderConfig -> RenderTarget -> RenderUnit
initialRenderUnit rconf rtarget =
    RenderUnit initialRenderWorldState (RenderEnv rconf rtarget) emptyRenderLog


runRenderSystem :: (MonadIO m) => RenderSubjects -> RenderUnit -> m RenderUnit
runRenderSystem subjects unit = do
    (unit', _) <- io $ execRWST theSystem subjects unit
    return unit'

theSystem :: RenderSystem ()
theSystem = do
    scene       <- view rSysRenderScene
    renderEnv   <- use renderSettings
    worldState  <- use renderResources
    let viewMatrix          = scene^.sceneCamera.cameraHandle.to camMatrix
        projMatrix          = projectionMatrix (scene^.sceneCamera.cameraFOV) aspect 0.1 100.0 -- TODO : move zfar/znear
        rView               = RenderView viewMatrix projMatrix
        worldEnv            = RenderWorldEnv $ map toRenderEntity $ scene^.sceneEntities
        (w, h)              = fromIntegral <$$> renderEnv^.reRenderTarget.targetSize
        aspect              = (w/h)
    (viewDefs, worldState') <- io $ runRenderWorld rView worldEnv worldState
    (_, __, rlog)           <- io $ runRenderer (renderView rView viewDefs) renderEnv
    renderResources .= worldState'
    lastRenderLog   .= rlog

{--
    let viewMatrix          = scene^.sceneCamera.cameraHandle.to camMatrix
        projMatrix          = projectionMatrix (scene^.sceneCamera.cameraFOV) aspect 0.1 100.0 -- TODO : move zfar/znear
        rView               = RenderView viewMatrix projMatrix
        worldEnv            = RenderWorldEnv $ map toRenderEntity $ scene^.sceneEntities
        worldState          = rUnit^.renderResources
        renderEnv           = rUnit^.renderSettings
        (w, h)              = fromIntegral <$$> renderEnv^.reRenderTarget.targetSize
        aspect              = (w/h)
    in io $ do
        (viewDefs, worldState')    <- runRenderWorld rView worldEnv worldState
        (_, __, rlog)              <- runRenderer (renderView rView viewDefs) renderEnv
        return $ RenderUnit worldState' renderEnv rlog
--}




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
