{-# LANGUAGE ConstraintKinds #-}
module Yage.Rendering.Vertex
    ( module Yage.Rendering.Vertex
    , module VinylGL
    ) where

import           Data.Vinyl.Reflect        as VinylGL
import           Data.Vinyl
import           Graphics.VinylGL.Vertex   as VinylGL

type Vertex = PlainFieldRec
