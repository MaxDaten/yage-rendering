{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- {-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Yage.Rendering.Types
    (Renderer, RenderEnv(..), RenderState(..), RenderLog(..)
    , RenderConfig(..), RenderStatistics(..), FBO, VBO, VAO, EBO
    , RenderTarget(..)
    , Program

    , Renderable(..), SomeRenderable(..), renderableType, fromRenderable, toRenderable
    , RenderBatch(..)
    , RenderData(..), RenderDefinition(..)
    , RenderScene(..), emptyRenderScene, entitiesCount, addEntity
    , RenderEntity(..), mkRenderEntity
    , Mesh(..), makeMesh, emptyMesh
    , Index, Position, Orientation, Scale
    , ShaderResource(..), ShaderProgram(..), UniShader, ShaderEnv(..), ShaderDefinition(..)
    , TextureDefinition(..), _texChannel, _texResource, TextureResource(..)
    
    , renderProgram, renderData, programDef, programSrc
    , toIndex1
    , module GLRawTypes
    ) where

import             Yage.Prelude                    hiding (log)

import             Data.List                       (length)
import             Data.Hashable                   ()
import qualified   Data.HashMap.Strict             as M
import             Foreign.C.Types                 (CFloat(..))
import             Filesystem.Path.CurrentOS       (encode)

import             Data.Typeable
import             Control.Monad.RWS.Strict        (RWST)
import             Control.Monad.Writer            ()
import             Control.Monad.Reader            (ReaderT)
import             Control.Monad.State             ()
import             Control.Lens                    hiding (Index)
import             Linear                          (Quaternion, M22, M33, M44, V3(..), zero, axisAngle)
---------------------------------------------------------------------------------------------------
import             Graphics.GLUtil
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.Raw.Types as GLRawTypes
---------------------------------------------------------------------------------------------------
import             Yage.Rendering.Texture
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
    { rlog'objcount :: !Int
    , rlog'tricount :: !Int
    , rlog'log      :: ![String]
    } deriving (Show, Eq)


data RenderConfig = RenderConfig
    { confClearColor        :: !(GL.Color4 Double)
    , confDebugNormals      :: !Bool
    }


data RenderState = RenderState
    { loadedShaders         :: !(M.HashMap ShaderResource ShaderProgram)
    , loadedMeshes          :: !(M.HashMap (Mesh Vertex4342) (VBO, EBO))
    , loadedDefinitions     :: !(M.HashMap RenderDefinition VAO)
    , loadedTextures        :: !(M.HashMap TextureResource GL.TextureObject)
    --, renderStatistics      :: !RenderStatistics
    }

data RenderStatistics = RenderStatistics
    { lastObjectCount       :: !Int
    , lastTriangleCount     :: !Int
    , lastRenderDuration    :: !Double
    , loadedShadersCount    :: !Int
    , loadedMeshesCount     :: !Int
    } deriving Show

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject


---------------------------------------------------------------------------------------------------
instance Monoid RenderLog where
    mempty = RenderLog 0 0 []
    mappend (RenderLog ca ta la) (RenderLog cb tb lb) = RenderLog (ca + cb) (ta + tb) (mappend la lb)

---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition    


toRenderable :: (Renderable r) => r -> SomeRenderable
toRenderable r | r `eqType` (undefined :: SomeRenderable) = error "already a SomeRenderable"
               | otherwise = SomeRenderable r


fromRenderable :: (Typeable r) => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r

renderableType :: SomeRenderable -> TypeRep
renderableType (SomeRenderable r) = typeOf r 
   
renderProgram :: (Renderable r) => r -> Program
renderProgram = def'program . renderDefinition

programSrc :: Program -> ShaderResource
programSrc = fst

programDef :: Program -> ShaderDefinition
programDef = snd

renderData :: (Renderable r) => r -> Mesh Vertex4342
renderData = def'data . renderDefinition


data SomeRenderable = forall r. (Typeable r, Renderable r) => SomeRenderable r
    deriving (Typeable)


instance Renderable SomeRenderable where
    --render scene res (SomeRenderable r) = render scene res r
    renderDefinition (SomeRenderable r) = renderDefinition r


data Renderable r => RenderBatch r = RenderBatch
    { withBatch         :: ([r] -> Renderer ()) -> Renderer ()
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

addEntity :: (Renderable r) =>  r -> RenderScene -> RenderScene
addEntity r scene@RenderScene{entities} = scene{ entities = (SomeRenderable r):entities }

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
    , texObjs       :: [(GL.TextureObject, (GLuint, String))]
    , triangleCount :: !Int
    } deriving Show

instance Renderable RenderEntity where
    renderDefinition = renderDef

toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

---------------------------------------------------------------------------------------------------

deriving instance Show ShaderProgram
type Index     = Int

data Mesh v = 
    Mesh
    { ident    :: !String
    , vertices :: ![v]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show)

instance Eq (Mesh v) where
    a == b = ident a == ident b

instance Ord (Mesh v) where
    compare a b = compare (ident a) (ident b)

makeMesh :: String -> [v] -> [Index] -> Mesh v
-- TODO some assertions for invalid meshes
makeMesh ident vs ixs = Mesh ident vs ixs $ (length ixs) `quot` 3

emptyMesh :: String -> Mesh v
emptyMesh ident = Mesh ident [] [] 0

---------------------------------------------------------------------------------------------------

type UniShader = ReaderT ShaderEnv IO

data ShaderEnv = ShaderEnv
    { shaderEnv'Program             :: ShaderProgram
    , shaderEnv'CurrentRenderable   :: SomeRenderable
    , shaderEnv'CurrentScene        :: RenderScene 
    } deriving (Typeable)

data ShaderResource = ShaderResource
    { vert'src   :: FilePath
    , frag'src   :: FilePath
    } deriving (Show, Eq, Ord)

--data ShaderUniformDef r s = ShaderUniformDef { runUniform :: r -> s -> ShaderProgram -> IO () }

data ShaderDefinition = ShaderDefinition
    { attrib'def  :: [VertexMapDef Vertex4342]
    , uniform'def :: UniShader ()
    }

type Program = (ShaderResource, ShaderDefinition)

data RenderDefinition = RenderDefinition
    { def'ident     :: String
    , def'data      :: Mesh Vertex4342
    , def'program   :: Program
    , def'textures  :: [TextureDefinition] -- | (Resource, Shader TextureUnit)
    }

instance Eq RenderDefinition where
    a == b = def'ident a == def'ident b

instance Ord RenderDefinition where
    compare a b = compare (def'ident a) (def'ident b)

instance Show RenderDefinition where
    show = show . def'ident 


---------------------------------------------------------------------------------------------------

instance AsUniform Float where
    asUniform = asUniform . CFloat

instance AsUniform (M44 Float) where
    asUniform m = asUniform $ over (mapped.mapped) CFloat m

instance AsUniform (M33 Float) where
    asUniform m = asUniform $ over (mapped.mapped) CFloat m

instance AsUniform (M22 Float) where
    asUniform m = asUniform $ over (mapped.mapped) CFloat m

---------------------------------------------------------------------------------------------------

data TextureResource = 
      TextureFile FilePath
    | TextureImage String DynamicImage
    deriving (Typeable)

instance Show TextureResource where
    show (TextureFile name) = show name
    show (TextureImage name _ ) = show name

instance Eq TextureResource where
    (==) (TextureFile name1) (TextureFile name2) = name1 == name2
    (==) (TextureImage name1 _ ) (TextureImage name2 _ ) = name1 == name2
    (==) _ _ = False

data TextureDefinition = TextureDefinition
    { __texChannel  :: (Int, String)
    , __texResource :: TextureResource
    } deriving (Typeable, Show, Eq)

makeLenses ''TextureDefinition

---------------------------------------------------------------------------------------------------

instance Hashable (Mesh v) where
    hashWithSalt salt = hashWithSalt salt . ident

instance Hashable RenderDefinition where
    hashWithSalt salt = hashWithSalt salt . def'ident

instance Hashable FilePath where
    hashWithSalt salt = hashWithSalt salt . encode

instance Hashable ShaderResource where
    hashWithSalt salt ShaderResource{..} = 
        salt     `hashWithSalt`
        vert'src `hashWithSalt` frag'src

instance Hashable TextureResource where
    hashWithSalt salt (TextureFile file) =
        salt `hashWithSalt` file
    hashWithSalt salt (TextureImage name _) =
        salt `hashWithSalt` name
