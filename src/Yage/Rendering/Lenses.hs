{-# LANGUAGE TemplateHaskell            #-}

module Yage.Rendering.Lenses where

import Control.Lens
import Yage.Rendering.Types


makeLenses ''TextureDefinition
