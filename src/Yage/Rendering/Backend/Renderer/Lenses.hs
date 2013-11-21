{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering.Backend.Renderer.Lenses where

import Control.Lens (makeLenses)

import Yage.Rendering.Backend.Renderer.Types 

makeLenses ''ViewDefinition
makeLenses ''RenderView
makeLenses ''RenderTarget
makeLenses ''ShaderEnv
makeLenses ''RenderLog
makeLenses ''RenderEnv
makeLenses ''RenderConfig
