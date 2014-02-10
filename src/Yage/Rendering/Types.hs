{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Yage.Rendering.Types
    ( Program
    , VBO, EBO, FBO, VAO, GL.PrimitiveMode(..)

    , Renderable(..), SomeRenderable(..), renderableType, fromRenderable, toRenderable
    , RenderDefinition(..)
    , RenderScene(..), Camera(..), CameraHandle, CameraPlanes(..), Viewport(..), ViewportI, ViewportD
    , RenderTransformation(..), idTransformation
    , RenderEntity(..)
    , Mesh(..), MeshData(..), ModificationToken
    , Index, Position, Orientation, Scale
    , ShaderResource(..), ShaderProgram(..)
    , TextureDefinition(..), TextureResource(..), TextureChannel, GLBufferSpec(..)
    , RenderbufferResource(..)

    , toIndex1, toGLViewport, GL.Color4(..), GL.PixelInternalFormat(..), GL.TextureTarget2D(..)
    , module GLRawTypes
    ) where

import           Yage.Prelude                        hiding (log, Index)

import           Data.Hashable                       ()
import           Filesystem.Path.CurrentOS           (encode)
import           Foreign                             (Storable)
import           Foreign.C.Types                     (CFloat (..))

import           Data.Typeable
import           GHC.Generics                        (Generic)

import           Control.Monad.State                 ()
import           Control.Monad.Writer                ()
import           Linear                              (M22, M33, M44, Quaternion, V3 (..), V2, zero, axisAngle, _x, _y)
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

data GLBufferSpec = GLBufferSpec 
    { _glBufferFormat    :: !GL.PixelInternalFormat 
    , _glBufferSize      :: V2 Int
    }

data RenderbufferResource = RenderbufferResource String GLBufferSpec

data TextureResource =
      TextureFile FilePath
    | TextureImage String DynamicImage
    | TextureBuffer String GL.TextureTarget2D GLBufferSpec -- only backed by opengl
    deriving (Typeable)


-- TODO
instance Ord TextureResource where
    compare (TextureFile pathA   ) (TextureFile pathB   )    = compare pathA pathB
    compare (TextureImage nameA _) (TextureImage nameB _)    = compare nameA nameB
    compare (TextureBuffer nameA _ _) (TextureBuffer nameB _ _)  = compare nameA nameB
    compare (TextureFile _) _ = GT
    compare _ _               = LT

instance Show TextureResource where
    show (TextureFile path)     = show path
    show (TextureImage name _ ) = show name
    show (TextureBuffer name _ _)  = show name

instance Eq TextureResource where
    (==) (TextureFile name1) (TextureFile name2)           = name1 == name2
    (==) (TextureImage name1 _ ) (TextureImage name2 _ )   = name1 == name2
    (==) (TextureBuffer name1 _ _) (TextureBuffer name2 _ _)   = name1 == name2
    (==) _ _ = False

instance Show RenderbufferResource where
    show (RenderbufferResource name _) = show name

instance Eq RenderbufferResource where
    (==) (RenderbufferResource nameA _) (RenderbufferResource nameB _) = nameA == nameB

instance Ord RenderbufferResource where
    compare (RenderbufferResource nameA _) (RenderbufferResource nameB _) = compare nameA nameB

---------------------------------------------------------------------------------------------------

type TextureChannel = (Int, String)

data TextureDefinition = TextureDefinition
    { _texChannel  :: TextureChannel
    , _texResource :: TextureResource
    } deriving (Typeable, Show, Eq, Ord)



data RenderDefinition = RenderDefinition
    { _rdefData     :: Mesh
    , _rdefProgram  :: Program
    , _rdefTextures :: [TextureDefinition] -- | (Resource, Shader TextureUnit)
    , _rdefMode     :: GL.PrimitiveMode
    }

---------------------------------------------------------------------------------------------------



class Typeable r => Renderable r where
    renderDefinition      :: r -> RenderDefinition
    renderTransformation  :: r -> RenderTransformation


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
    renderDefinition     (SomeRenderable r) = renderDefinition r
    renderTransformation (SomeRenderable r) = renderTransformation r


---------------------------------------------------------------------------------------------------

type CameraHandle = Cam.Camera Float

data CameraPlanes = CameraPlanes
    { _camZNear  :: !Double
    , _camZFar   :: !Double
    } deriving (Show)


data Camera =
      Camera3D !CameraHandle CameraPlanes !Float
    | Camera2D !CameraHandle CameraPlanes
    deriving (Show)

deriving instance Show CameraHandle

-- how to add statics?!
-- mark ents or seperate?
data RenderScene = RenderScene
    { _sceneEntities         :: [SomeRenderable]
    , _sceneCamera           :: !Camera
    } deriving (Typeable)


type ViewportI = Viewport Int
type ViewportD = Viewport Double
data Viewport a = Viewport
    { _vpXY     :: V2 a
    , _vpSize   :: V2 a       -- ^ (width, height)
    , _vpFactor :: !Double      -- ^ for retina use 2
    } 
    deriving ( Typeable, Functor )

toGLViewport :: ViewportI -> (GL.Position, GL.Size)
toGLViewport Viewport{..} = 
    let pos  = fromIntegral <$> _vpXY
        size = fromIntegral . (floor _vpFactor *) <$> _vpSize
    in ( GL.Position (pos^._x) (pos^._y), GL.Size (size^._x) (size^._y) )

---------------------------------------------------------------------------------------------------

type Orientation = Quaternion Float
type Scale       = V3 Float
type Position    = V3 Float

data RenderTransformation = RenderTransformation
    { _transPosition    :: !Position
    , _transOrientation :: !Orientation
    , _transScale       :: !Scale
    } deriving ( Show, Typeable )

idTransformation :: RenderTransformation
idTransformation =
 RenderTransformation
    zero
    (axisAngle (V3 0 1 0) (Cam.deg2rad 0))
    (V3 1 1 1)

data RenderEntity = RenderEntity
    { _entityTransformation :: !RenderTransformation
    , _entityRenderDef      :: !RenderDefinition
    } deriving (Typeable)

---------------------------------------------------------------------------------------------------

type Index     = Int

type ModificationToken = Int

data Mesh = forall v. (Storable v) =>
    Mesh
    { _meshId         :: Int
    , _meshName       :: String
    , _meshData       :: MeshData v
    , _meshAttr       :: MeshData v -> [VertexAttribute]
    , _meshModToken   :: ModificationToken
    } deriving (Typeable)

data MeshData v = MeshData
    { _mDataVertices :: ![v]
    , _mDataIndices  :: ![Index]
    , _mDataTriCount :: !Int
    } deriving ( Show, Generic )

instance Show Mesh where
    show Mesh{..} =
        format "Mesh {id = {0}, name = {1}, data = N/A, attribs = {2}, modToken = {3}}"
               [show _meshId, show _meshName, "N/A", show _meshModToken]

instance Eq Mesh where
    a == b = _meshId a == _meshId b

instance Ord Mesh where
    compare a b = compare (_meshId a) (_meshId b)


---------------------------------------------------------------------------------------------------

toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

---------------------------------------------------------------------------------------------------

data ShaderResource = ShaderResource
    { _srVertSrc :: FilePath
    , _srFragSrc :: FilePath
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
        salt       `hashWithSalt`
        _srVertSrc `hashWithSalt` _srFragSrc

instance Hashable TextureResource where
    hashWithSalt salt (TextureFile file) =
        salt `hashWithSalt` file
    hashWithSalt salt (TextureImage name _) =
        salt `hashWithSalt` name
    hashWithSalt salt (TextureBuffer name _ _) =
        salt `hashWithSalt` name

