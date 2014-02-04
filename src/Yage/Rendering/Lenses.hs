{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Yage.Rendering.Lenses where

import           Yage.Prelude

import           Yage.Rendering.Types

import qualified Graphics.GLUtil.Camera3D as C (Camera(..))

import           Linear (V3, Quaternion)


makeLenses ''TextureDefinition
makeLenses ''ShaderResource
makeLenses ''RenderDefinition
makeLenses ''RenderScene
makeLenses ''RenderEntity
makeLenses ''Viewport
makeLenses ''RenderTransformation
makeLenses ''MeshData
makeLenses ''CameraPlanes
--makeLenses ''Mesh

-- for Program
shaderRes = _1
shaderDef = _2

cameraHandle :: Lens' Camera CameraHandle
cameraHandle f (Camera3D hnd planes fov) = fmap (\h -> Camera3D h planes fov) (f hnd)
cameraHandle f (Camera2D hnd planes) = fmap (`Camera2D` planes) (f hnd)


entityPosition = entityTransformation.transPosition
entityScale = entityTransformation.transScale
entityOrientation = entityTransformation.transOrientation

meshId :: Lens' Mesh Int
meshId f (m@Mesh{ _meshId }) = fmap (\ident -> m{_meshId = ident}) (f _meshId)

meshName :: Lens' Mesh String
meshName f (m@Mesh{ _meshName }) = fmap (\name -> m{_meshName = name}) (f _meshName)

meshModToken :: Lens' Mesh ModificationToken
meshModToken f (m@Mesh{ _meshModToken }) = fmap (\token -> m{_meshModToken = token}) (f _meshModToken)


cameraLocation :: Lens' CameraHandle (V3 Float)
cameraLocation f (cam@C.Camera{C.location})= fmap (\l -> cam{C.location = l}) (f location)

cameraOrientation :: Lens' CameraHandle (Quaternion Float)
cameraOrientation f (cam@C.Camera{C.orientation}) = fmap (\o -> cam{C.orientation = o}) (f orientation)