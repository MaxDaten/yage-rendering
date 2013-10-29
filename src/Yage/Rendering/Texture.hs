{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards            #-}
module Yage.Rendering.Texture (
      TexColor(), TexInfo()
    , module JT
    ) where

import Yage.Prelude

import Graphics.GLUtil.JuicyTextures as JT
import Graphics.GLUtil.Textures

deriving instance Show TexColor

instance Show (TexInfo a) where
    show TexInfo{..} = format "TexInfo: {0} {1} - {2}" [show $ texWidth, show $ texHeight, show $ texColor]