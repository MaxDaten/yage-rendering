{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering.Backend.Renderer.Lenses where

import Control.Lens (makeLenses)

import Yage.Rendering.Backend.Renderer.Types 

makeLenses ''RenderLog
makeLenses ''RenderConfig
makeLenses ''RenderState
makeLenses ''RenderData
