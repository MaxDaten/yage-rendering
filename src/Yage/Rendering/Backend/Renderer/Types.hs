{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
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


type Renderer = RWST RenderSettings RenderLog () IO

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


{--
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

type ShaderDefinition = ReaderT ShaderEnv IO


data ShaderEnv = ShaderEnv
    { _seProgram     :: ShaderProgram
    , _seViewDef     :: ViewEntity
    , _seView        :: RenderView 
    } deriving (Show, Typeable)

---------------------------------------------------------------------------------------------------

data RenderData = RenderData -- TODO rename RenderResource
    { _vao           :: GL.VertexArrayObject
    , _shaderProgram :: ShaderProgram
    , _texObjs       :: [(GL.TextureObject, (GL.GLuint, String))]
    , _drawMode      :: !GL.PrimitiveMode
    , _elementCount  :: !Int
    } deriving Show

---------------------------------------------------------------------------------------------------


-- TODO : RenderScene -- RenderView overlap
data RenderView = RenderView
    { _rvViewMatrix        :: !(M44 Float)
    , _rvProjectionMatrix  :: !(M44 Float)
    } deriving Show


data ViewEntity = ViewEntity
    { _vdMVPMatrix             :: !(M44 Float)
    , _vdModelMatrix           :: !(M44 Float)
    , _vdModelViewMatrix       :: !(M44 Float)
    , _vdNormalMatrix          :: !(M33 Float)
    , _vdRenderData            :: !RenderData
    , _vdUniformDef            :: !(ShaderDefinition (), ShaderEnv)
    }


---------------------------------------------------------------------------------------------------

instance Show ViewEntity where
    show ViewEntity{..} = 
        format "ViewEntity {mvp = {0}, model = {1}, normal = {2}, data = N/A, shader = N/A}"
                    [show _vdMVPMatrix, show _vdModelMatrix, show _vdNormalMatrix]

instance Monoid RenderLog where
    mempty = RenderLog 0 0 []
    mappend (RenderLog ca ta la) (RenderLog cb tb lb) = RenderLog (ca + cb) (ta + tb) (mappend la lb)

