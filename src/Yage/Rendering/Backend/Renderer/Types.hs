{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
module Yage.Rendering.Backend.Renderer.Types (
      module Yage.Rendering.Backend.Renderer.Types
    , module GLReExports
    ) where

import             Yage.Prelude                    hiding (log)

import             Control.Monad.Reader            (ReaderT)
import             Control.Monad.RWS.Strict        (RWST)

import             Linear                          (M44, M33)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))

import             Graphics.GLUtil
import             Yage.Rendering.Backend.Shader   ()


type Renderer = RWST RenderEnv RenderLog () IO

data RenderTarget = RenderTarget
    { _targetSize   :: !(Int, Int)
    , _targetRatio  :: !Double
    }

data RenderEnv = RenderEnv
    { _reRenderConfig         :: !RenderConfig    -- ^ The current settings for the frame
    , _reRenderTarget         :: !RenderTarget
    }

data RenderLog = RenderLog 
    { _rlLogObjCount :: !Int
    , _rlLogTriCount :: !Int
    , _rlLog         :: ![String]
    } deriving (Show, Eq)


data RenderConfig = RenderConfig
    { _rcConfClearColor        :: !(GL.Color4 Double)
    , _rcConfDebugNormals      :: !Bool
    , _rcConfWireframe         :: !Bool
    }

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
    , _seViewDef     :: ViewDefinition
    , _seView        :: RenderView 
    } deriving (Show, Typeable)

---------------------------------------------------------------------------------------------------

data RenderData = RenderData -- TODO rename RenderResource
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , texObjs       :: [(GL.TextureObject, (GL.GLuint, String))]
    , triangleCount :: !Int
    } deriving Show

---------------------------------------------------------------------------------------------------


-- TODO : RenderScene -- RenderView overlap
data RenderView = RenderView
    { _rvViewMatrix        :: !(M44 Float)
    , _rvProjectionMatrix  :: !(M44 Float)
    } deriving Show


data ViewDefinition = ViewDefinition
    { _vdMVPMatrix             :: !(M44 Float)
    , _vdModelMatrix           :: !(M44 Float)
    , _vdNormalMatrix          :: !(M33 Float)
    , _vdRenderData            :: !RenderData
    , _vdUniformDef            :: !(ShaderDefinition (), ShaderEnv)
    }


---------------------------------------------------------------------------------------------------

instance Show ViewDefinition where
    show ViewDefinition{..} = 
        format "ViewDefinition {mvp = {0}, model = {1}, normal = {2}, data = N/A, shader = N/A}"
                    [show _vdMVPMatrix, show _vdModelMatrix, show _vdNormalMatrix]

instance Monoid RenderLog where
    mempty = RenderLog 0 0 []
    mappend (RenderLog ca ta la) (RenderLog cb tb lb) = RenderLog (ca + cb) (ta + tb) (mappend la lb)

