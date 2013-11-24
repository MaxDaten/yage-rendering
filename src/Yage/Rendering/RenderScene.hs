module Yage.Rendering.RenderScene
    ( module Yage.Rendering.RenderScene
    , Cam.deg2rad, Cam.rosCamera, Cam.fpsCamera, Cam.projectionMatrix, Cam.camMatrix
    , CameraHandle
    ) where


import           Yage.Prelude

import           Data.List (length)

import           Linear (V3(..), V4(..), Conjugate, Epsilon, M44, Quaternion)
import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Lenses
import           Yage.Rendering.Types



emptyRenderScene :: Camera -> RenderScene
emptyRenderScene = RenderScene []-- ) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)

addEntity :: (Renderable r) => RenderScene -> r -> RenderScene
addEntity scene r = scene & sceneEntities <>~ [SomeRenderable r]

entitiesCount :: RenderScene -> Int
entitiesCount = length . _sceneEntities

mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera

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

--fov :: Camera -> Float -> Camera
--fov cam d = cam & cameraProj +~ d 


orthographicMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                    => a -> a -> a -> a -> a -> a -> M44 a
orthographicMatrix l r t b n f = 
    V4 ( V4 (2/(r-l)) 0        0             (-(r+l)/(r-l)) )
       ( V4 0        (2/(t-b)) 0             (-(t+b)/(t-b)) )
       ( V4 0        0         ((-2)/(f-n))  (-(f+n)/(f-n)) )
       ( V4 0        0         0             1              )

