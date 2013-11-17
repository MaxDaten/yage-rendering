{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Yage.Rendering.Types
    ( Program
    , VBO, EBO, FBO, VAO

    , Renderable(..), SomeRenderable(..), renderableType, fromRenderable, toRenderable, toRenderEntity
    , RenderData(..), RenderDefinition(..)
    , RenderScene(..), emptyRenderScene, entitiesCount, addEntity
    , RenderEntity(..), mkRenderEntity
    , Mesh(..), MeshData(..), makeMesh, makeMeshFromVerts, emptyMeshData
    , Index, Position, Orientation, Scale
    , ShaderResource(..), ShaderProgram(..), UniShader, ShaderEnv(..), ShaderDefinition(..)
    , TextureDefinition(..), texChannel, texResource, TextureResource(..)
    
    , renderProgram, renderData, programDef, programSrc
    , toIndex1
    , Color, Color3, Color4
    , module GLRawTypes
    , module RendererTypes
    ) where

import             Yage.Prelude                    hiding (log)

import             Data.List                       (length)
import             Data.Hashable                   ()
import             Foreign.C.Types                 (CFloat(..))
import             Foreign                         (Storable)
import             Filesystem.Path.CurrentOS       (encode)

import             Data.Typeable
import             GHC.Generics                    (Generic)

import             Control.Monad.Writer            ()
import             Control.Monad.Reader            (ReaderT)
import             Control.Monad.State             ()
import             Control.Lens                    hiding (Index)
import             Linear                          (Quaternion, M22, M33, M44, V3(..), zero, axisAngle)
---------------------------------------------------------------------------------------------------
import             Graphics.GLUtil
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL       (Color, Color3, Color4)
import             Graphics.Rendering.OpenGL.Raw.Types as GLRawTypes
---------------------------------------------------------------------------------------------------
import             Yage.Rendering.Texture
import             Yage.Rendering.VertexSpec       (VertexAttribute)
import             Yage.Rendering.Backend.Renderer.Types as RendererTypes
-- =================================================================================================

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject


---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where

    renderDefinition      :: r -> RenderDefinition   
    renderPosition        :: r -> Position
    renderOrientation     :: r -> Orientation
    renderScale           :: r -> Scale 


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

renderData :: (Renderable r) => r -> Mesh
renderData = def'data . renderDefinition


data SomeRenderable = forall r. (Typeable r, Renderable r) => SomeRenderable r
    deriving (Typeable)


instance Renderable SomeRenderable where
    --render scene res (SomeRenderable r) = render scene res r
    renderDefinition  (SomeRenderable r) = renderDefinition r
    renderPosition    (SomeRenderable r) = renderPosition r
    renderOrientation (SomeRenderable r) = renderOrientation r
    renderScale       (SomeRenderable r) = renderScale r 



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
    } deriving (Typeable)

mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { ePosition     = zero
    , eOrientation  = axisAngle (V3 0 1 0) (Cam.deg2rad 0)
    , eScale        = (V3 1 1 1)
    , renderDef     = def
    }

data RenderData = RenderData -- TODO rename RenderResource
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , texObjs       :: [(GL.TextureObject, (GLuint, String))]
    , triangleCount :: !Int
    } deriving Show

instance Renderable RenderEntity where
    renderDefinition  = renderDef
    renderPosition    = ePosition
    renderOrientation = eOrientation
    renderScale       = eScale

toRenderEntity :: SomeRenderable -> RenderEntity
toRenderEntity (SomeRenderable r) = RenderEntity
    { ePosition    = renderPosition r
    , eOrientation = renderOrientation r
    , eScale       = renderScale r
    , renderDef    = renderDefinition r
    }

---------------------------------------------------------------------------------------------------

deriving instance Show ShaderProgram
type Index     = Int

data Mesh = forall v. (Storable v) =>
    Mesh
    { meshId       :: Int
    , meshName     :: String
    , meshData     :: MeshData v
    , meshAttr     :: [VertexAttribute]
    , dirty        :: Bool
    } deriving (Typeable)

data MeshData v = MeshData
    { vertices :: ![v]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show, Generic)

instance Show Mesh where
    show = show . meshName -- TODO

instance Eq Mesh where
    a == b = meshId a == meshId b

instance Ord Mesh where
    compare a b = compare (meshId a) (meshId b)

makeMeshFromVerts :: (Storable v) => Int -> String -> [v] -> [Index] -> [VertexAttribute] -> Mesh
-- TODO some assertions for invalid meshes
makeMeshFromVerts ident name vs ixs attribs = 
    let meshdata = MeshData vs ixs $ (length ixs) `quot` 3
    in Mesh ident name meshdata attribs True

makeMesh :: (Storable v) => Int -> String -> MeshData v -> [VertexAttribute] -> Mesh
makeMesh ident name meshdata attribs = Mesh ident name meshdata attribs True

emptyMeshData :: MeshData v
emptyMeshData = MeshData [] [] 0

---------------------------------------------------------------------------------------------------

toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

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
    { uniform'def :: UniShader () }

type Program = (ShaderResource, ShaderDefinition)

data RenderDefinition = RenderDefinition
    { def'data      :: Mesh
    , def'program   :: Program
    , def'textures  :: [TextureDefinition] -- | (Resource, Shader TextureUnit)
    }

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

-- TODO
instance Ord TextureResource

instance Show TextureResource where
    show (TextureFile name) = show name
    show (TextureImage name _ ) = show name

instance Eq TextureResource where
    (==) (TextureFile name1) (TextureFile name2) = name1 == name2
    (==) (TextureImage name1 _ ) (TextureImage name2 _ ) = name1 == name2
    (==) _ _ = False

data TextureDefinition = TextureDefinition
    { _texChannel  :: (Int, String)
    , _texResource :: TextureResource
    } deriving (Typeable, Show, Eq, Ord)

makeLenses ''TextureDefinition

---------------------------------------------------------------------------------------------------

instance (Hashable v) => Hashable (MeshData v)

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
