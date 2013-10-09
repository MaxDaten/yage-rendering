{-# LANGUAGE ExistentialQuantification, RecordWildCards, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}
module Yage.Rendering.Types where

import             Yage.Prelude                    hiding (log)

import             Data.Maybe                      (fromJust)
import             Data.List                       (length)
import             Foreign.Storable                (sizeOf)

import             Data.Typeable
import             Control.Monad.RWS.Strict        (RWST)
import             Control.Monad.Writer            ()
import             Control.Monad.Reader            ()
import             Control.Monad.State             ()
import             Control.Lens                    ((^.))
import             Linear                          (M44, M33, V3(..), zero, axisAngle, mkTransformation
                                                   , _xyz, (!*!), adjoint, inv33, kronecker, point)
---------------------------------------------------------------------------------------------------
import             Graphics.GLUtil
import qualified   Graphics.GLUtil                 as GL
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------
import             Yage.Math
import             Yage.Resources
import             Yage.Rendering.Shader

-- =================================================================================================

type Renderer = RWST RenderEnv RenderLog RenderState IO
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
    { count :: !Integer
    , log   :: [String]
    } deriving (Show)

data RenderConfig = RenderConfig
    { confClearColor        :: !(GL.Color4 Double)
    , confDebugNormals      :: !Bool
    }


data RenderState = RenderState
    { loadedShaders         :: ![(YageShaderResource, ShaderProgram)]
    , loadedMeshes          :: ![(TriMesh, (VBO, EBO))] -- TODO better Key, better structure
    , loadedDefinitions     :: ![(RenderDefinition, VAO)] -- BETTER KEY!!
    --, renderStatistics      :: !RenderStatistics
    }

--data RenderStatistics = RenderStatistics
--    { lastObjectCount       :: !Int
--    , lastTriangleCount     :: !Int
--    , lastRenderDuration    :: !Double
--    , loadedShadersCount    :: !Int
--    , loadedMeshesCount     :: !Int
--    } deriving Show

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject


type YageShaderDef = ShaderDefs (GL.VertexArrayDescriptor Int) YageShader
type YageShaderProgram = ShaderProgram
newtype YageShader a = YageShader (Shader YageShaderDef YageShaderProgram Renderer a) -- isolate YageRenderer to one
    deriving (Monad, MonadIO, MonadShader YageShaderDef YageShaderProgram)

instance Monoid RenderLog where
    mempty = RenderLog 0 []
    mappend (RenderLog ca la) (RenderLog cb lb) = RenderLog (ca + cb) (mappend la lb) 

shade :: ShaderProgram -> YageShader a -> Renderer a
shade sh (YageShader x) = runShader x globShaderDef sh


globShaderDef :: YageShaderDef
globShaderDef = ShaderDefs
    { sVertexPosition     = mkAttrDef    VertexPos        "in_vert_position" positionVad
    , sVertexNormal       = mkAttrDef    VertexNormal     "in_vert_normal"   normalVad
    , sVertexColor        = mkAttrDef    VertexColor      "in_vert_color"    colorVad
    , sGlobalTime         = mkUniformDef GlobalTime       "global_time"
    , sProjectionMatrix   = mkUniformDef ProjectionMatrix "projection_matrix"
    , sViewMatrix         = mkUniformDef ViewMatrix       "view_matrix"
    , sModelMatrix        = mkUniformDef ModelMatrix      "model_matrix"
    , sNormalMatrix       = mkUniformDef NormalMatrix     "normal_matrix"
    }
    where positionVad   = let stride = fromIntegral $ sizeOf (undefined::Vertex)
                          in GL.VertexArrayDescriptor 4 GL.Float stride offset0
          normalVad     = let stride = fromIntegral $ sizeOf (undefined::Vertex)
                          in GL.VertexArrayDescriptor 3 GL.Float stride (offsetPtr $ sizeOf (undefined :: Position))
          colorVad      = let stride = fromIntegral $ sizeOf (undefined::Vertex)
                          in GL.VertexArrayDescriptor 4 GL.Float stride (offsetPtr $ sizeOf (undefined :: Position) + sizeOf (undefined :: Normal))

mkUniformDef :: AsUniform u => (String -> ShaderUniforms String) -> String -> UniformDef u YageShader
mkUniformDef uni s = (uni s, \p v -> io $! v `asUniform` getUniform p s)

mkAttrDef :: vad ~ GL.VertexArrayDescriptor a => (String -> vad -> ShaderAttributes String vad) -> String -> vad -> AttributeDef vad YageShader
mkAttrDef attr s vad = 
    ( attr s vad,
      \p _ -> io $! do
        GL.enableAttrib p s
        GL.setAttrib p s GL.ToFloat vad
    )
---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where
    --render             :: RenderScene -> RenderData -> r -> YageRenderer ()

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition
    modelAndNormalMatrix :: r -> (M44 GL.GLfloat, M33 GL.GLfloat)
    
    shader :: r -> YageShaderResource
    shader = snd . defs . renderDefinition

    model :: r -> TriMesh
    model = fst . defs . renderDefinition
            


data SomeRenderable = forall r. Renderable r => SomeRenderable r
    deriving (Typeable)


fromRenderable :: Renderable r => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r


instance Renderable SomeRenderable where
    --render scene res (SomeRenderable r) = render scene res r
    renderDefinition (SomeRenderable r) = renderDefinition r
    modelAndNormalMatrix (SomeRenderable r) = modelAndNormalMatrix r


data Renderable r => RenderBatch r = RenderBatch
    { preBatchAction    :: [r] -> Renderer ()
    , perItemAction     :: r -> Renderer ()
    , batch             :: [r]
    }
    

---------------------------------------------------------------------------------------------------


-- how to add statics?!
-- mark ents or seperate?
data RenderScene = RenderScene
    { entities            :: [SomeRenderable]
    , sceneTime           :: !GL.GLfloat
    , viewMatrix          :: !(M44 GL.GLfloat)
    , projectionMatrix    :: !(M44 GL.GLfloat)
    } deriving (Typeable)


emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene [] 0.0 (Cam.camMatrix Cam.fpsCamera) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)


entitiesCount :: RenderScene -> Int
entitiesCount = length . entities

---------------------------------------------------------------------------------------------------

data RenderEntity = RenderEntity 
    { ePosition    :: !Position
    , eOrientation :: !Orientation
    , eScale       :: !Scale
    , renderDef    :: RenderDefinition
    } deriving (Typeable)

mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { ePosition     = zero
    , eOrientation  = axisAngle (V3 0 1 0) (Cam.deg2rad 0)
    , eScale        = (V3 1 1 1)
    , renderDef     = def
    }

data RenderData = RenderData
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , triangleCount :: !Int
    }

instance Renderable RenderEntity where
    renderDefinition = renderDef
    modelAndNormalMatrix r =
        let scaleM      = mkScale . eScale $ r                                         :: M44 GL.GLfloat
            transformM  = mkTransformation (eOrientation $ r) ((ePosition $ r)^._xyz)  :: M44 GL.GLfloat
            modelM      = transformM !*! scaleM                                        :: M44 GL.GLfloat
            normalM     = adjoint $ fromJust . inv33 . fromTransformation $ modelM     :: M33 GL.GLfloat
        in (modelM, normalM)
        where mkScale = kronecker . point

                

from1 :: GL.UniformComponent a => a -> GL.Index1 a
from1 = GL.Index1

