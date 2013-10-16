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
module Yage.Rendering.Types where

import             Yage.Prelude                    hiding (log)

import             Data.List                       (length)

import             Data.Typeable
import             Foreign.Storable
import             Control.Monad.RWS.Strict        (RWST)
import             Control.Monad.Writer            ()
import             Control.Monad.Reader            ()
import             Control.Monad.State             ()
import             Linear                          (Quaternion, M44, M33, V4(..), V3(..), zero, axisAngle, mkTransformation
                                                   , _xyz, (!*!), adjoint, inv33, kronecker, point)
---------------------------------------------------------------------------------------------------
import             Graphics.GLUtil
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL       (GLfloat)
---------------------------------------------------------------------------------------------------

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


---------------------------------------------------------------------------------------------------

instance Monoid RenderLog where
    mempty = RenderLog 0 []
    mappend (RenderLog ca la) (RenderLog cb lb) = RenderLog (ca + cb) (mappend la lb)

---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where
    --render             :: RenderScene -> RenderData -> r -> YageRenderer ()

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition
    --modelAndNormalMatrix :: r -> (M44 GL.GLfloat, M33 GL.GLfloat)
            

   
renderProgram :: (Renderable r) => r -> Program
renderProgram = def'program . renderDefinition

programSrc :: Program -> ShaderResource
programSrc = fst

programDef :: Program -> ShaderDefinition
programDef = snd

renderData :: (Renderable r) => r -> TriMesh
renderData = def'data . renderDefinition


data SomeRenderable = forall r. Renderable r => SomeRenderable r
    deriving (Typeable)


fromRenderable :: Renderable r => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r


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


type Orientation = Quaternion GLfloat
type Scale = V3 GLfloat

--type Vertex = V4 GLfloat
type Position = V4 GLfloat
type Normal = V3 GLfloat
type Color = V4 GLfloat
type Index = Int

data Vertex = Vertex 
    { vert'position :: Position
    , vert'normal   :: Normal
    , vert'color    :: Color
    } deriving (Show, Eq)

instance Storable Vertex where
    sizeOf _ = sizeOf (undefined::Position) + sizeOf (undefined::Normal) + sizeOf (undefined::Color)
    alignment _ = alignment (undefined::Position)
    peek ptr = 
        Vertex 
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr (sizeOf (undefined :: Position))
            <*> peekByteOff ptr (sizeOf (undefined :: Position) + sizeOf (undefined :: Normal))

    poke ptr Vertex{..} = do
        pokeByteOff ptr 0 vert'position
        pokeByteOff ptr (sizeOf (undefined :: Position)) vert'normal
        pokeByteOff ptr (sizeOf (undefined :: Position) + sizeOf (undefined :: Normal)) vert'color

-- TODO need a layout

data TriMesh = 
    TriMesh
    { meshId   :: !String
    , vertices :: ![Vertex]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show)

instance Eq TriMesh where
    a == b = meshId a == meshId b

instance Ord TriMesh where
    compare a b = compare (meshId a) (meshId b)

mkTriMesh :: String -> [Vertex] -> [Index] -> TriMesh
-- TODO some assertions for invalid meshes
mkTriMesh id vs ixs = TriMesh id vs ixs $ (length ixs) `quot` 3

---------------------------------------------------------------------------------------------------


data ShaderResource = ShaderResource
    { vert'src   :: FilePath
    , frag'src   :: FilePath
    } deriving (Show, Eq, Ord)

data AttribDef a = AttribDef
    { vad :: GL.VertexArrayDescriptor a
    , ih  :: GL.IntegerHandling
    } deriving (Show, Eq, Ord)
--type ShaderDefinition = forall a. ShaderDef (VAD a) RenderEntity RenderScene

type ShaderAttributeDef a = (String, AttribDef a)
data ShaderUniformDef r s = ShaderUniformDef { runUniform :: r -> s -> IO () }

data ShaderDefinition = ShaderDefinition
    { attrib'def :: forall a. [ShaderAttributeDef a]
    , uniform'def :: (Renderable r) => ShaderUniformDef r RenderScene
    }

instance Show ShaderDefinition where show = show . attrib'def
instance Eq ShaderDefinition where (==) a b = attrib'def a == attrib'def b 
instance Ord ShaderDefinition where compare a b = attrib'def a `compare` attrib'def b

type Program = (ShaderResource, ShaderDefinition)

data RenderDefinition = RenderDefinition
    { def'data      :: TriMesh
    , def'program   :: Program
    } deriving (Show, Eq, Ord)


