{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
module Yage.Rendering.Backend.Renderer.Types (
      module Yage.Rendering.Backend.Renderer.Types
    , module GLReExports
    ) where

import             Yage.Prelude                    hiding (log)

import             Data.Vinyl
import             Control.Monad.RWS.Strict        (RWST)

import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))

import             Graphics.GLUtil
import             Yage.Rendering.Backend.Shader   ()


type Renderer = RWST () RenderLog RenderState IO


data RenderConfig = RenderConfig
    { _rcConfDebugNormals      :: !Bool
    , _rcConfWireframe         :: !Bool
    --, _rcConfClearColor        :: !(GL.Color4 Double) -- FIXME: will be managed by fbo
    }


data RenderLog = RenderLog 
    { _rlLogObjCount :: !Int
    , _rlLogTriCount :: !Int
    , _rlLog         :: ![String]
    , _resourceLog   :: ![String]
    } deriving (Show, Eq)



data RenderState = RenderState
    { -- _currentFramebuffer :: Maybe GLFramebuffer
     _currentShader      :: Maybe ShaderProgram
    }

initRenderState :: RenderState
initRenderState = RenderState Nothing



{--
data RenderState = RenderState
    { _currentFBO :: Maybe Framebuffer }
data RenderStatistics = RenderStatistics
    { _rsStatObjectCount       :: !Int
    , lastTriangleCount     :: !Int
    , lastRenderDuration    :: !Double
    , loadedShadersCount    :: !Int
    , loadedMeshesCount     :: !Int
    } deriving Show
--}


emptyRenderLog :: RenderLog
emptyRenderLog = mempty

---------------------------------------------------------------------------------------------------  
type TextureAssignment = (GL.TextureObject, (GL.GLuint, String))
---------------------------------------------------------------------------------------------------

data RenderSet urec = RenderSet -- TODO rename RenderResource
    { _vao             :: GL.VertexArrayObject
    , _uniformDefs     :: PlainRec urec
    , _textureChannels :: [TextureAssignment]
    , _drawMode        :: !GL.PrimitiveMode
    , _vertexCount     :: !GL.GLsizei
    }


instance Show (RenderSet urec) where
    show RenderSet{..} = "RenderSet: { vao: {0}, texs: {1}, mode: {2}, elem# {3} }"
---------------------------------------------------------------------------------------------------

instance Monoid RenderLog where
    mempty = RenderLog 0 0 [] []
    mappend (RenderLog ca ta la ra) (RenderLog cb tb lb rb) = RenderLog (ca + cb) (ta + tb) (mappend la lb) (mappend ra rb)

