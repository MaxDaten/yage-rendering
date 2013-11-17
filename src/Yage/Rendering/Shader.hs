module Yage.Rendering.Shader
    ( withShader
    ) where

import             Yage.Prelude

import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Yage.Rendering.Types
import             Yage.Rendering.Backend.Renderer.Types

withShader :: ShaderProgram -> (ShaderProgram -> Renderer a) -> Renderer a
withShader shader m = do
    io $! GL.currentProgram $= Just (program shader)
    res <- m shader
    io $! GL.currentProgram $= Nothing
    return res
