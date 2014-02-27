{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Yage.Rendering.Lenses where

import           Yage.Lens

import           Yage.Rendering.Types


makeLenses ''TextureDefinition
makeLenses ''GLBufferSpec
makeLenses ''ShaderResource
makeLenses ''RenderEntity
--makeLenses ''RenderScene
makeLenses ''Mesh
--makeLenses ''Mesh

-- for Program
shaderRes = _1
shaderDef = _2




