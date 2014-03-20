{-# LANGUAGE TemplateHaskell #-}
module Yage.Rendering.Viewport where

import Yage.Prelude
import Yage.Lens

import Graphics.Rendering.OpenGL as GL (Position(..), Size(..))

import Linear (V2(..), _x, _y)

type ViewportI = Viewport Int
type ViewportD = Viewport Double

data Viewport a = Viewport
    { _vpXY     :: V2 a
    , _vpSize   :: V2 a       -- ^ (width, height) in px
    } 
    deriving ( Typeable, Functor, Show, Eq, Ord )

makeLenses ''Viewport


toGLViewport :: ViewportI -> (GL.Position, GL.Size)
toGLViewport Viewport{..} = 
    let pos  = fromIntegral <$> _vpXY
        size = fromIntegral <$> _vpSize
    in ( GL.Position (pos^._x) (pos^._y), GL.Size (size^._x) (size^._y) )
