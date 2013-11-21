module Yage.Rendering.RenderScene where


import           Yage.Prelude

import           Data.List (length)
import           Control.Lens


import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Lenses
import           Yage.Rendering.Types



emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene [] 0.0 (Cam.camMatrix Cam.fpsCamera) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)

addEntity :: (Renderable r) =>  r -> RenderScene -> RenderScene
addEntity r scene = scene & sceneEntities <>~ [SomeRenderable r]

entitiesCount :: RenderScene -> Int
entitiesCount = length . _sceneEntities
