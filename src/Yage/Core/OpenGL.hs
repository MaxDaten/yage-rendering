{-# OPTIONS_GHC -fno-warn-orphans               #-}
module Yage.Core.OpenGL (
	  module GL
	) where

import Graphics.Rendering.OpenGL as GL
import Data.Typeable
import Data.Data

deriving instance Typeable GL.TextureFilter
deriving instance Data GL.TextureFilter
deriving instance Typeable GL.Repetition
deriving instance Data GL.Repetition
deriving instance Typeable GL.Clamping
deriving instance Data GL.Clamping
