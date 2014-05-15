{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering.Types where

import           Yage.Prelude                        hiding (log, Index)
import           Yage.Lens                           ( makeLenses )

import           Data.Hashable                       ()


import           Control.Monad.State                 ()
import           Control.Monad.Writer                ()
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL           as GL
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Mesh                 as Mesh
import           Yage.Rendering.Shader               ( ShaderData )
-- =================================================================================================

---------------------------------------------------------------------------------------------------

data GLDrawSettings = GLDrawSettings
    { _renderMode :: !GL.PrimitiveMode
    , _cullFace   :: !(Maybe GL.Face)
    -- TODO expand
    } deriving ( Show )


-- | RenderEntity provides all data for an item to render.
-- this are not opengl-native resources
data RenderEntity vr u t = RenderEntity
    { _entMesh      :: !(Mesh vr)
    , _entData      :: !(ShaderData u t)
    , _entSettings  :: !GLDrawSettings
    -- , _entityTextures :: [Texture]          -- | Textures 
    }


makeLenses ''RenderEntity
makeLenses ''GLDrawSettings


---------------------------------------------------------------------------------------------------

{--
class (ViableVertex (Vertex vr)) => Renderable r vr | r -> vr where
    renderDefinition      :: r -> RenderEntity vr
--}

---------------------------------------------------------------------------------------------------
type Index        = Int
toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

---------------------------------------------------------------------------------------------------
