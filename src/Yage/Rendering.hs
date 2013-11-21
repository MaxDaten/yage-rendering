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

import           Control.Lens

import           Linear

import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.Backend.Renderer.Types as RendererExports 
                                                          (RenderConfig (..), RenderTarget (..), RenderLog (..))
import           Yage.Rendering.Lenses
import           Yage.Rendering.RenderWorld            hiding (renderResources)
import           Yage.Rendering.RenderEntity           as RenderEntity
import           Yage.Rendering.Types                  as Types


data RenderUnit = RenderUnit
    { _renderResources :: RenderWorldState
    , _renderSettings  :: RenderEnv
    , _lastRenderLog   :: RenderLog
    }

makeLenses ''RenderUnit

initialRenderUnit :: RenderConfig -> RenderTarget -> RenderUnit
initialRenderUnit rconf rtarget =
    RenderUnit initialRenderWorldState (RenderEnv rconf rtarget) emptyRenderLog


renderScene :: (MonadIO m) => RenderScene -> RenderUnit -> m RenderUnit
renderScene scene rUnit =
    let rView       = RenderView (scene^.sceneViewMatrix) (scene^.sceneProjectionMatrix)
        worldEnv    = RenderWorldEnv $ map toRenderEntity $ scene^.sceneEntities
        worldState  = rUnit^.renderResources
        renderEnv   = rUnit^.renderSettings
    in io $ do
        (viewDefs, worldState')    <- runRenderWorld rView worldEnv worldState
        (_, __, rlog)              <- runRenderer (renderView rView viewDefs) renderEnv
        return $ RenderUnit worldState' renderEnv rlog



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
