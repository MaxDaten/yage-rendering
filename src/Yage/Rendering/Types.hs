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
    ( Renderable(..)
    , RenderEntity(..), GLDrawSettings(..)
    , Index, Position, Orientation, Scale

    , ShaderResource(..), ShaderProgram(..)
    , TextureDefinition(..), Texture(..), TextureChannel, GLBufferSpec(..)
    , Renderbuffer(..)

    , toIndex1, GL.PixelInternalFormat(..), GL.TextureTarget2D(..), GL.PrimitiveMode(..), GL.Face(..)
    , module GLRawTypes
    , module ResTypes
    ) where

import           Yage.Prelude                        hiding (log, Index)

import           Data.Hashable                       ()
import           Filesystem.Path.CurrentOS           (encode)


import           Control.Monad.State                 ()
import           Control.Monad.Writer                ()
---------------------------------------------------------------------------------------------------
import           Graphics.GLUtil
import qualified Graphics.Rendering.OpenGL           as GL
import           Graphics.Rendering.OpenGL.Raw.Types as GLRawTypes
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Mesh                 as Mesh
import           Yage.Rendering.Vertex
import           Yage.Rendering.Resources.ResTypes   as ResTypes
import           Yage.Rendering.Transformation
-- =================================================================================================

---------------------------------------------------------------------------------------------------

type TextureChannel = (Int, String)

data TextureDefinition = TextureDefinition
    { _texChannel  :: TextureChannel        -- | to opengl texture unit
    , _texResource :: Texture
    } deriving (Typeable, Show, Eq, Ord)


data RenderEntity vr = RenderEntity
    { _renderData     :: Mesh vr
    --, _rdefProgram  :: Program
    , _drawSettings   :: !GLDrawSettings
    , _entityTextures :: [TextureDefinition] -- | (Resource, Shader TextureUnit)
    }

data GLDrawSettings = GLDrawSettings
    { _renderMode :: !GL.PrimitiveMode
    , _cullFace   :: !(Maybe GL.Face)
    } 

---------------------------------------------------------------------------------------------------

class (ViableVertex (Vertex vr)) => Renderable r vr | r -> vr where
    renderDefinition      :: r -> RenderEntity vr
    --renderTransformation  :: r -> Transformation GLfloat


---------------------------------------------------------------------------------------------------
type Index        = Int
toIndex1 :: a -> GL.Index1 a
toIndex1 = GL.Index1

--type Program = (ShaderResource, ShaderDefinition ())



---------------------------------------------------------------------------------------------------

instance Hashable FilePath where
    hashWithSalt salt = hashWithSalt salt . encode

instance Hashable ShaderResource where
    hashWithSalt salt ShaderResource{..} =
        salt       `hashWithSalt`
        _srVertSrc `hashWithSalt` _srFragSrc

---------------------------------------------------------------------------------------------------
