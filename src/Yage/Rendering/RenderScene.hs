module Yage.Rendering.RenderScene
    ( module Yage.Rendering.RenderScene
    , Cam.deg2rad, Cam.rosCamera, Cam.fpsCamera, Cam.projectionMatrix, Cam.camMatrix
    , CameraHandle
    ) where


import           Yage.Prelude

import           Data.List (length)

import           Linear (V3)
import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Lenses
import           Yage.Rendering.Types



emptyRenderScene :: Camera -> RenderScene
emptyRenderScene cam = RenderScene [] 0.0 cam -- ) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)

addEntity :: (Renderable r) =>  r -> RenderScene -> RenderScene
addEntity r scene = scene & sceneEntities <>~ [SomeRenderable r]

entitiesCount :: RenderScene -> Int
entitiesCount = length . _sceneEntities


-- | for chaining like:
-- >>> cam `dolly` movement
-- >>>     `pan`   turn
-- >>>     `tilt`  tilting
-- 
dolly :: CameraHandle -> V3 Float -> CameraHandle
dolly = flip Cam.dolly

panRad :: CameraHandle -> Float -> CameraHandle
panRad = flip Cam.panRad

pan :: CameraHandle -> Float -> CameraHandle
pan = flip Cam.pan

tiltRad :: CameraHandle -> Float -> CameraHandle
tiltRad = flip Cam.tiltRad

tilt :: CameraHandle -> Float -> CameraHandle
tilt = flip Cam.tilt

rollRad :: CameraHandle -> Float -> CameraHandle
rollRad = flip Cam.rollRad

roll :: CameraHandle -> Float -> CameraHandle
roll = flip Cam.roll

fov :: Camera -> Float -> Camera
fov cam d = cam & cameraFOV +~ d 