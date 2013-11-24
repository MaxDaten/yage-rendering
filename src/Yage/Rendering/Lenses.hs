{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Yage.Rendering.Lenses where

import Yage.Prelude

import Yage.Rendering.Types


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
cameraHandle f (Camera3D h fov) = fmap (\h' -> Camera3D h' fov) (f h)
cameraHandle f (Camera2D h) = fmap (\h' -> Camera2D h') (f h)