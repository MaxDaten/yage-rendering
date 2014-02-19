{-# LANGUAGE ConstraintKinds #-}
module Yage.Rendering.Vertex
    ( module Yage.Rendering.Vertex
    , module Vertex
    , module VinylGL
    ) where

import           Data.Vinyl.Reflect        as VinylGL
import           Foreign.Storable          (Storable)

import           Graphics.VinylGL.Uniforms (HasFieldGLTypes)
import           Graphics.VinylGL.Vertex   as VinylGL
import           Yage.Geometry.Vertex      as Vertex




type ViableVertex t = ( HasFieldNames t
                      , HasFieldSizes t
                      , HasFieldDims t
                      , HasFieldGLTypes t
                      , Storable t
                      )
