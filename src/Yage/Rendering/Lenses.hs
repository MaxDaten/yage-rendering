{-# LANGUAGE TemplateHaskell            #-}

module Yage.Rendering.Lenses where

import Control.Lens
import Yage.Rendering.Types


makeLenses ''TextureDefinition
makeLenses ''ShaderResource
makeLenses ''RenderDefinition
makeLenses ''RenderScene
makeLenses ''RenderEntity
makeLenses ''MeshData
makeLenses ''Mesh
makeLenses ''Camera
