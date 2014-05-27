{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering.RenderEntity where

import           Yage.Prelude                        hiding (log, Index)
import           Yage.Lens                           ( makeLenses )

import           Data.Hashable                       ()


import           Control.Monad.State                 ()
import           Control.Monad.Writer                ()
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL           as GL
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Mesh                 as Mesh

---------------------------------------------------------------------------------------------------

data GLDrawSettings = GLDrawSettings
    { _renderMode :: !GL.PrimitiveMode
    , _cullFace   :: !(Maybe GL.Face)
    -- TODO expand
    } deriving ( Show )


-- | RenderEntity provides all data for an item to render.
-- this are not opengl-native resources
data RenderEntity vr dat  = RenderEntity        -- | TODO: just payload data
    { _entMesh      :: !(Mesh vr)
    , _entData      :: !dat -- (ShaderData u t)
    , _entSettings  :: !GLDrawSettings
    -- , _entityTextures :: [Texture]          -- | Textures 
    }


makeLenses ''RenderEntity
makeLenses ''GLDrawSettings

