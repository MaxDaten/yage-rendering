{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveDataTypeable, DeriveTraversable #-}
module Yage.Rendering.Types where

import             Yage.Prelude                    hiding (log)

import             Data.List                       (length)
import             Foreign.C.Types                 (CFloat(..))

import             Data.Typeable
import             Control.Monad.RWS.Strict        (RWST)
import             Control.Monad.Writer            ()
import             Control.Monad.Reader            ()
import             Control.Monad.State             ()
import             Control.Lens                    hiding (Index)
import             Linear                          (Quaternion, M22, M33, M44, V3(..), zero, axisAngle)
---------------------------------------------------------------------------------------------------
import             Graphics.GLUtil
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------
import             Yage.Rendering.VertexSpec
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
    { rlog'objcount :: !Integer
    , rlog'log      :: ![String]
    } deriving (Show, Eq)


data RenderConfig = RenderConfig
    { confClearColor        :: !(GL.Color4 Double)
    , confDebugNormals      :: !Bool
    }


data RenderState = RenderState
    { loadedShaders         :: ![(ShaderResource, ShaderProgram)]
    , loadedMeshes          :: ![(Mesh Vertex434, (VBO, EBO))] -- TODO better Key, better structure
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


---------------------------------------------------------------------------------------------------

instance Monoid RenderLog where
    mempty = RenderLog 0 []
    mappend (RenderLog ca la) (RenderLog cb lb) = RenderLog (ca + cb) (mappend la lb)

---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition

    fromRenderable :: SomeRenderable -> Maybe r
    fromRenderable (SomeRenderable r) = cast r
    
    toRenderable :: r -> SomeRenderable
    toRenderable = SomeRenderable
            

   
renderProgram :: (Renderable r) => r -> Program
renderProgram = def'program . renderDefinition

programSrc :: Program -> ShaderResource
programSrc = fst

programDef :: Program -> ShaderDefinition
programDef = snd

renderData :: (Renderable r) => r -> Mesh Vertex434
renderData = def'data . renderDefinition


data SomeRenderable = forall r. (Typeable r, Renderable r) => SomeRenderable r
    deriving (Typeable)

instance Renderable SomeRenderable where
    --render scene res (SomeRenderable r) = render scene res r
    renderDefinition (SomeRenderable r) = renderDefinition r


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
    , sceneTime           :: !Float
    , viewMatrix          :: !(M44 Float)
    , projectionMatrix    :: !(M44 Float)
    } deriving (Typeable)


emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene [] 0.0 (Cam.camMatrix Cam.fpsCamera) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)


entitiesCount :: RenderScene -> Int
entitiesCount = length . entities

---------------------------------------------------------------------------------------------------

type Orientation = Quaternion Float
type Scale       = V3 Float
type Position    = V3 Float

data RenderEntity = RenderEntity
    { ePosition    :: !Position
    , eOrientation :: !Orientation
    , eScale       :: !Scale
    , renderDef    :: RenderDefinition
    } deriving (Typeable, Show)

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

{--
modelAndNormalMatrix r =
        let scaleM      = mkScale . eScale $ r                                         :: M44 GL.GLfloat
            transformM  = mkTransformation (eOrientation $ r) ((ePosition $ r)^._xyz)  :: M44 GL.GLfloat
            modelM      = transformM !*! scaleM                                        :: M44 GL.GLfloat
            normalM     = adjoint $ fromJust . inv33 . fromTransformation $ modelM     :: M33 GL.GLfloat
        in (modelM, normalM)
        where mkScale = kronecker . point
--}
                

from1 :: GL.UniformComponent a => a -> GL.Index1 a
from1 = GL.Index1

---------------------------------------------------------------------------------------------------

deriving instance Show ShaderProgram
type Index     = Int
-- TODO need a layout

data Mesh v = 
    Mesh
    { ident    :: !String
    , vertices :: ![v]
    , indices  :: ![Index]
    } deriving (Show)

instance Eq (Mesh v) where
    a == b = ident a == ident b

instance Ord (Mesh v) where
    compare a b = compare (ident a) (ident b)

mkTriMesh :: String -> [v] -> [Index] -> Mesh v
-- TODO some assertions for invalid meshes
mkTriMesh ident vs ixs = Mesh ident vs ixs -- $ (length ixs) `quot` 3

---------------------------------------------------------------------------------------------------


data ShaderResource = ShaderResource
    { vert'src   :: FilePath
    , frag'src   :: FilePath
    } deriving (Show, Eq, Ord)

data ShaderUniformDef r s = ShaderUniformDef { runUniform :: r -> s -> ShaderProgram -> IO () }

data ShaderDefinition = ShaderDefinition
    { attrib'def  :: [VertexMapDef Vertex434]
    , uniform'def :: ShaderUniformDef SomeRenderable RenderScene
    }

type Program = (ShaderResource, ShaderDefinition)

data RenderDefinition = RenderDefinition
    { def'ident     :: String
    , def'data      :: Mesh Vertex434
    , def'program   :: Program
    }

instance Eq RenderDefinition where
    a == b = def'ident a == def'ident b

instance Ord RenderDefinition where
    compare a b = compare (def'ident a) (def'ident b)

instance Show RenderDefinition where
    show = show . def'ident 

--instance Show ShaderDefinition where
--    show = show . attrib'def

---------------------------------------------------------------------------------------------------
instance AsUniform Float where
    asUniform = asUniform . CFloat

--instance AsUniform Double where
--    asUniform = asUniform . CFloat . double2Float

instance AsUniform (M44 Float) where
    asUniform m = asUniform $ over (mapped.mapped) CFloat m

instance AsUniform (M33 Float) where
    asUniform m = asUniform $ over (mapped.mapped) CFloat m

instance AsUniform (M22 Float) where
    asUniform m = asUniform $ over (mapped.mapped) CFloat m
