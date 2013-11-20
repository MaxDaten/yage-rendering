{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Yage.Rendering.Types
    ( Program
    , VBO, EBO, FBO, VAO

    , Renderable(..), SomeRenderable(..), renderableType, fromRenderable, toRenderable, toRenderEntity
    , RenderDefinition(..)
    , RenderScene(..), emptyRenderScene, entitiesCount, addEntity
    , RenderEntity(..), mkRenderEntity
    , Mesh(..), MeshData(..)
    , Index, Position, Orientation, Scale
    , ShaderResource(..), ShaderProgram(..)
    , TextureDefinition(..), TextureResource(..)

    , toIndex1
    , module GLRawTypes
    ) where

import           Yage.Prelude                        hiding (log)

import           Data.Hashable                       ()
import           Data.List                           (length)
import           Filesystem.Path.CurrentOS           (encode)
import           Foreign                             (Storable)
import           Foreign.C.Types                     (CFloat (..))

import           Data.Typeable
import           GHC.Generics                        (Generic)

import           Control.Lens                        hiding (Index)
import           Control.Monad.State                 ()
import           Control.Monad.Writer                ()
import           Linear                              (M22, M33, M44, Quaternion,
                                                      V3 (..), axisAngle, zero)
---------------------------------------------------------------------------------------------------
import           Graphics.GLUtil
import qualified Graphics.GLUtil.Camera3D            as Cam
import qualified Graphics.Rendering.OpenGL           as GL
import           Graphics.Rendering.OpenGL.Raw.Types as GLRawTypes
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Texture
import           Yage.Rendering.VertexSpec           (VertexAttribute)
import           Yage.Rendering.Backend.Renderer.Types as RendererTypes (ShaderDefinition)
-- =================================================================================================

type VBO = GL.BufferObject
type EBO = GL.BufferObject
type FBO = GL.FramebufferObject

---------------------------------------------------------------------------------------------------

data TextureResource =
      TextureFile FilePath
    | TextureImage String DynamicImage
    deriving (Typeable)


-- TODO
instance Ord TextureResource where
    compare (TextureFile pathA   ) (TextureFile pathB   ) = compare pathA pathB
    compare (TextureImage nameA _) (TextureImage nameB _) = compare nameA nameB
    compare (TextureFile _) _ = GT
    compare _ _               = LT

instance Show TextureResource where
    show (TextureFile name)     = show name
    show (TextureImage name _ ) = show name

instance Eq TextureResource where
    (==) (TextureFile name1) (TextureFile name2)         = name1 == name2
    (==) (TextureImage name1 _ ) (TextureImage name2 _ ) = name1 == name2
    (==) _ _ = False

---------------------------------------------------------------------------------------------------


data TextureDefinition = TextureDefinition
    { _texChannel  :: (Int, String)
    , _texResource :: TextureResource
    } deriving (Typeable, Show, Eq, Ord)



data RenderDefinition = RenderDefinition
    { def'data     :: Mesh
    , def'program  :: Program
    , def'textures :: [TextureDefinition] -- | (Resource, Shader TextureUnit)
    }

---------------------------------------------------------------------------------------------------



class Typeable r => Renderable r where
    renderDefinition      :: r -> RenderDefinition
    renderPosition        :: r -> Position
    renderOrientation     :: r -> Orientation
    renderScale           :: r -> Scale


data SomeRenderable = forall r. (Typeable r, Renderable r) => SomeRenderable r
    deriving (Typeable)


toRenderable :: (Renderable r) => r -> SomeRenderable
toRenderable r | r `eqType` (undefined :: SomeRenderable) = error "already a SomeRenderable"
               | otherwise = SomeRenderable r


fromRenderable :: (Typeable r) => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r

renderableType :: SomeRenderable -> TypeRep
renderableType (SomeRenderable r) = typeOf r


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
    { entities         :: [SomeRenderable]
    , sceneTime        :: !Float
    , viewMatrix       :: !(M44 Float)
    , projectionMatrix :: !(M44 Float)
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

type Index     = Int

data Mesh = forall v. (Storable v) =>
    Mesh
    { meshId   :: Int
    , meshName :: String
    , meshData :: MeshData v
    , meshAttr :: [VertexAttribute]
    , dirty    :: Bool
    } deriving (Typeable)

data MeshData v = MeshData
    { vertices :: ![v]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show, Generic)

instance Show Mesh where
    show Mesh{..} =
        format "Mesh {id = {0}, name = {1}, data = N/A, attribs = {2}, dirty = {3}}"
               [     show meshId,    show meshName,        show meshAttr, show dirty]

instance Eq Mesh where
    a == b = meshId a == meshId b

instance Ord Mesh where
    compare a b = compare (meshId a) (meshId b)


---------------------------------------------------------------------------------------------------

toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

---------------------------------------------------------------------------------------------------

data ShaderResource = ShaderResource
    { vert'src :: FilePath
    , frag'src :: FilePath
    } deriving (Show, Eq, Ord)


type Program = (ShaderResource, ShaderDefinition ())


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

