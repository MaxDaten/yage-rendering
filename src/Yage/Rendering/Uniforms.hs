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

