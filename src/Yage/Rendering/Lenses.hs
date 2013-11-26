{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Yage.Rendering.Lenses where

import           Yage.Prelude

import           Yage.Rendering.Types


makeLenses ''TextureDefinition
makeLenses ''ShaderResource
makeLenses ''RenderDefinition
makeLenses ''RenderScene
makeLenses ''RenderEntity
makeLenses ''RenderTransformation
makeLenses ''MeshData
--makeLenses ''Mesh

-- for Program
shaderRes = _1
shaderDef = _2

cameraHandle :: Lens' Camera CameraHandle
cameraHandle f (Camera3D h fov) = fmap (`Camera3D` fov) (f h)
cameraHandle f (Camera2D h) = fmap Camera2D (f h)

entityPosition = entityTransformation.transPosition
entityScale = entityTransformation.transScale
entityOrientation = entityTransformation.transOrientation

meshId :: Lens' Mesh Int
meshId f (m@Mesh{ _meshId }) = fmap (\ident -> m{_meshId = ident}) (f _meshId)
meshName :: Lens' Mesh String
meshName f (m@Mesh{ _meshName }) = fmap (\name -> m{_meshName = name}) (f _meshName)
meshModToken :: Lens' Mesh ModificationToken
meshModToken f (m@Mesh{ _meshModToken }) = fmap (\token -> m{_meshModToken = token}) (f _meshModToken)