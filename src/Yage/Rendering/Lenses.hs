{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Yage.Rendering.Lenses where

import           Yage.Prelude

import           Yage.Rendering.Types


makeLenses ''TextureDefinition
makeLenses ''ShaderResource
makeLenses ''RenderDefinition
makeLenses ''RenderScene
makeLenses ''RenderEntity
makeLenses ''MeshData
makeLenses ''Mesh

-- for Program
shaderRes = _1
shaderDef = _2

cameraHandle :: Lens' Camera CameraHandle
cameraHandle f (Camera3D h fov) = fmap (`Camera3D` fov) (f h)
cameraHandle f (Camera2D h) = fmap Camera2D (f h)
