{-# LANGUAGE ConstraintKinds #-}
module Yage.Rendering.Uniforms
    ( module Yage.Rendering.Uniforms
    , module VinylGL
    ) where

import           Data.Vinyl                as VinylGL

import           Graphics.VinylGL.Uniforms as VinylGL
import           Data.Vinyl.Reflect

import           Graphics.VinylGL.Vertex   as VinylGL



type Uniforms a = PlainRec a

type UniformFields a = (HasFieldNames a, HasFieldGLTypes a, SetUniformFields a)

---------------------------------------------------------------------------------------------------

--instance AsUniform Float where
--    asUniform = asUniform . GLfloat

--instance AsUniform (M44 Float) where
--    asUniform m = asUniform $ over (mapped.mapped) GLfloat m

--instance AsUniform (M33 Float) where
--    asUniform m = asUniform $ over (mapped.mapped) GLfloat m

--instance AsUniform (M22 Float) where
--    asUniform m = asUniform $ over (mapped.mapped) CFloat m


