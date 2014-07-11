{-# OPTIONS_GHC -fno-warn-orphans               #-}
module Yage.Core.OpenGL (
	  module GL
    , module Yage.Core.OpenGL
	) where

import Yage.Prelude

import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil           as GL hiding (texture3DWrap)
import Data.Data

deriving instance Typeable GL.TextureFilter
deriving instance Data GL.TextureFilter
deriving instance Typeable GL.Repetition
deriving instance Data GL.Repetition
deriving instance Typeable GL.Clamping
deriving instance Data GL.Clamping

-- from GLUtil
texture3DWrap :: ParameterizedTextureTarget t => t -> StateVar (Repetition, Clamping)
texture3DWrap target = makeStateVar (get (textureWrapMode target S))
                             (forM_ [S,T,R] . aux)
  where aux x d = textureWrapMode target d $= x

