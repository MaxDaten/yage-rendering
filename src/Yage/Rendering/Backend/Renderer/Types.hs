module Yage.Rendering.Backend.Renderer.Types where

import             Yage.Prelude                    hiding (log)

import             Control.Monad.RWS.Strict        (RWST)

import qualified   Graphics.Rendering.OpenGL       as GL


type Renderer = RWST RenderEnv RenderLog () IO
    --deriving (Functor, Applicative, Monad, MonadIO, MonadReader YageRenderEnv, MonadWriter RenderLog MonadState RenderState, Typeable)

data RenderTarget = RenderTarget
    { target'size   :: !(Int, Int)
    , target'ratio  :: !Double
    }

data RenderEnv = RenderEnv
    { envConfig         :: !RenderConfig    -- ^ The current settings for the frame
    , renderTarget      :: !RenderTarget
    }

data RenderLog = RenderLog 
    { rlog'objcount :: !Int
    , rlog'tricount :: !Int
    , rlog'log      :: ![String]
    } deriving (Show, Eq)


data RenderConfig = RenderConfig
    { confClearColor        :: !(GL.Color4 Double)
    , confDebugNormals      :: !Bool
    , confWireframe         :: !Bool
    }

data RenderStatistics = RenderStatistics
    { lastObjectCount       :: !Int
    , lastTriangleCount     :: !Int
    , lastRenderDuration    :: !Double
    , loadedShadersCount    :: !Int
    , loadedMeshesCount     :: !Int
    } deriving Show


---------------------------------------------------------------------------------------------------
instance Monoid RenderLog where
    mempty = RenderLog 0 0 []
    mappend (RenderLog ca ta la) (RenderLog cb tb lb) = RenderLog (ca + cb) (ta + tb) (mappend la lb)

emptyRenderLog :: RenderLog
emptyRenderLog = mempty