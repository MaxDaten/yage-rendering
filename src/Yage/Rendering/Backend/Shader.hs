{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yage.Rendering.Backend.Shader where

import             Yage.Prelude
import             Graphics.GLUtil


deriving instance Show ShaderProgram

-- | The new Generation-Shader:
-- a shader is a simple function which takes an item as input and runs instruction on it

{--

data Shader = Shader
    { RenderItem -> Program -> ... }

RenderPass p d t
    p :: RenderProgram
    d :: RenderData
    t :: RenderTarget
--}