{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
module Yage.Rendering.Backend.Renderer.Types (
      module Yage.Rendering.Backend.Renderer.Types
    , module GLReExports
    ) where

import             Yage.Prelude                    hiding (log)

import             Control.Monad.Reader            (ReaderT)
import             Control.Monad.RWS.Strict        (RWST)

import             Linear                          (V2, M44, M33)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))

import             Graphics.GLUtil
import             Yage.Rendering.Backend.Shader   ()
import             Yage.Rendering.Backend.Framebuffer


type Renderer = RWST RenderSettings RenderLog RenderState IO

data RenderSettings = RenderSettings
    { _reRenderConfig         :: !RenderConfig    -- ^ The current settings for the frame
    , _reRenderTarget         :: !RenderTarget
    }


data RenderTarget = RenderTarget
    { _targetXY     :: V2 Int
    , _targetSize   :: V2 Int      -- ^ (width, height)
    , _targetFactor :: !Double      -- ^ for retina use 2
    , _targetZNear  :: !Double
    , _targetZFar   :: !Double
    , _targetDirty  :: !Bool
    --, _targetBuffer :: !Framebuffer
    }


data RenderConfig = RenderConfig
    { _rcConfDebugNormals      :: !Bool
    , _rcConfWireframe         :: !Bool
    --, _rcConfClearColor        :: !(GL.Color4 Double) -- FIXME: will be managed by fbo
    }


data RenderLog = RenderLog 
    { _rlLogObjCount :: !Int
    , _rlLogTriCount :: !Int
    , _rlLog         :: ![String]
    } deriving (Show, Eq)



data RenderState = RenderState
    { _currentFramebuffer :: Maybe Framebuffer
    , _currentShader      :: Maybe ShaderProgram
    }

initRenderState = RenderState Nothing Nothing


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

type ShaderDefinition = ReaderT ShaderProgram Renderer
  
type TextureAssignment = (GL.TextureObject, (GL.GLuint, String))
---------------------------------------------------------------------------------------------------

data RenderData = RenderData -- TODO rename RenderResource
    { _vao             :: GL.VertexArrayObject
    , _uniformDefs     :: ShaderDefinition () -- to monoid
    , _textureChannels :: [TextureAssignment]
    , _drawMode        :: !GL.PrimitiveMode
    , _elementCount    :: !Int
    }


instance Show RenderData where
    show RenderData{..} = "RenderData: { vao: {0}, texs: {1}, mode: {2}, elem# {3} }"
---------------------------------------------------------------------------------------------------


-- TODO : RenderScene -- RenderView overlap
data RenderView = RenderView
    { _rvViewMatrix        :: !(M44 Float)
    , _rvProjectionMatrix  :: !(M44 Float)
    } deriving Show

---------------------------------------------------------------------------------------------------

instance Monoid RenderLog where
    mempty = RenderLog 0 0 []
    mappend (RenderLog ca ta la) (RenderLog cb tb lb) = RenderLog (ca + cb) (ta + tb) (mappend la lb)

