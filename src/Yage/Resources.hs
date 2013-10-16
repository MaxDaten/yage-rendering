{-# LANGUAGE RecordWildCards #-}
module Yage.Resources where

import             Yage.Prelude                    hiding (id)

import             Linear                          (V3(..), V4(..))
import             Data.List                       (length)
import             Linear.Quaternion               (Quaternion)
import             Graphics.Rendering.OpenGL       (GLfloat)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Foreign.Storable

---------------------------------------------------------------------------------------------------

