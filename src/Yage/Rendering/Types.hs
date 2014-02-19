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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FunctionalDependencies     #-}
module Yage.Rendering.Types
    ( Program
    , GL.PrimitiveMode(..)

    , Renderable(..), SomeRenderable(..), renderableType, fromRenderable
    , RenderDefinition(..)
    , Index, Position, Orientation, Scale
    , ShaderResource(..), ShaderProgram(..)
    , TextureDefinition(..), TextureResource(..), TextureChannel, GLBufferSpec(..)
    , RenderbufferResource(..)

    , toIndex1, GL.PixelInternalFormat(..), GL.TextureTarget2D(..)
    , module GLRawTypes
    , module Mesh
    , module Vertex
    , module ResTypes
    ) where

import           Yage.Prelude                        hiding (log, Index)
import           Yage.Geometry.Vertex                as Vertex

import           Data.Hashable                       ()
import           Filesystem.Path.CurrentOS           (encode)
import           Foreign.C.Types                     (CFloat (..))

import           Data.Typeable
import           Data.Typeable                       as Ty (cast)

import           Control.Monad.State                 ()
import           Control.Monad.Writer                ()
import           Linear                              (M22, M33, M44)
---------------------------------------------------------------------------------------------------
import           Graphics.GLUtil
import qualified Graphics.Rendering.OpenGL           as GL
import           Graphics.Rendering.OpenGL.Raw.Types as GLRawTypes
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Mesh                 as Mesh
import           Yage.Rendering.Backend.Renderer.Types as RendererTypes (ShaderDefinition)
import           Yage.Rendering.Resources.ResTypes   as ResTypes
import           Yage.Rendering.Transformation
-- =================================================================================================

---------------------------------------------------------------------------------------------------

type TextureChannel = (Int, String)

data TextureDefinition = TextureDefinition
    { _texChannel  :: TextureChannel
    , _texResource :: TextureResource
    } deriving (Typeable, Show, Eq, Ord)



data RenderDefinition vr = RenderDefinition
    { _rdefData     :: Mesh vr
    --, _rdefProgram  :: Program
    , _rdefTextures :: [TextureDefinition] -- | (Resource, Shader TextureUnit)
    , _rdefMode     :: GL.PrimitiveMode
    }

---------------------------------------------------------------------------------------------------



class (ViableVertex (Vertex vr)) => Renderable r vr | r -> vr where
    renderDefinition      :: r -> RenderDefinition vr
    renderTransformation  :: r -> Transformation

--{--
data SomeRenderable vr = forall r. (Typeable r, Renderable r vr) 
                   => SomeRenderable r
    deriving (Typeable)


fromRenderable :: (Typeable r) => SomeRenderable rv -> Maybe r
fromRenderable (SomeRenderable r) = Ty.cast r

renderableType :: SomeRenderable rv -> TypeRep
renderableType (SomeRenderable r) = typeOf r


instance (Typeable vr, ViableVertex (Vertex vr)) => Renderable (SomeRenderable vr) vr where
    renderDefinition     (SomeRenderable r) = renderDefinition r
    renderTransformation (SomeRenderable r) = renderTransformation r

--}

---------------------------------------------------------------------------------------------------
type Index        = Int
toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

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

