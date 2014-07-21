{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE UndecidableInstances         #-} -- FIXME : should not be neccessary
module Yage.Rendering.Shader
    ( module Yage.Rendering.Shader
    , module VinylGL
    , module U
    , Symbol, KnownSymbol
    , ShaderType(..)
    ) where

import           Yage.Prelude
import           Yage.Lens

import           GHC.TypeLits (Symbol, KnownSymbol)

import           Data.Vinyl                as VinylGL
import           Data.Vinyl.Universe       as U
import qualified Data.Vinyl.Universe.Const as U

import           Control.Exception         (ErrorCall)

import           Graphics.VinylGL.Uniforms as VinylGL hiding (setUniforms)

import qualified Graphics.VinylGL.Uniforms as V
import qualified Data.Vinyl.Idiom.Identity as I


import           Yage.Core.OpenGL hiding (Shader)

import           Yage.Rendering.Resources.ResTypes

class HasTexture t where
    getTexture :: t -> Texture
instance HasTexture (Texture) where
    getTexture = id
instance HasTexture (I.Identity Texture) where
    getTexture (I.Identity t) = t

type FieldNames           = PlainRec (U.Const String)
type AllHasTextures ts    = RecAll ElField I.Identity ts HasTexture
type IsShaderData us ts   = (UniformFields (Uniforms us), AllHasTextures ts, Implicit (FieldNames ts))

type Uniforms us = PlainFieldRec us
type Textures ts = PlainFieldRec ts

data ShaderData uniforms textures = ShaderData
    { _shaderUniforms :: Uniforms uniforms
    , _shaderTextures :: Textures textures
    }

makeLenses ''ShaderData


data ShaderSource = ShaderSource
    { _srcDebugName   :: !String
    -- ^ just for debug
    , _srcType        :: !ShaderType
    , _srcRaw         :: !ByteString
    -- ^ strict `ByteString`
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderSource

compilationUnit :: Getter ShaderSource (ShaderType, ByteString)
compilationUnit = to unit where
    unit src = (src^.srcType, src^.srcRaw)

instance Hashable ShaderSource where
    hashWithSalt salt ShaderSource{_srcRaw} =
        salt `hashWithSalt` _srcRaw


data ShaderProgramUnit = ShaderProgramUnit
    { _shaderName    :: !String
    , _shaderSources :: [ShaderSource]
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderProgramUnit


data ShaderUnit d = ShaderUnit
    { _shaderProgram    :: !ShaderProgramUnit
    , _shaderData       :: d
    } deriving (Show, Eq, Ord)

makeLenses ''ShaderUnit

---------------------------------------------------------------------------------------------------


type TextureUniform u = (u::Symbol) ::: Texture

deriving instance Show ShaderProgram



textureFields :: (AllHasTextures ts, Implicit (FieldNames ts)) => Textures ts -> [(String, Texture)]
textureFields rec = go implicitly rec [] where 
    go :: AllHasTextures ts => FieldNames ts -> PlainFieldRec ts -> [(String, Texture)] -> [(String, Texture)]
    go RNil RNil ts = ts
    go (I.Identity n :& ns) (x :& xs) ts = (n, getTexture x) : go ns xs ts


instance (Monoid (Uniforms u), Monoid (Textures t)) => Monoid (ShaderData u t) where
    mempty = ShaderData mempty mempty
    (ShaderData u t) `mappend` (ShaderData u' t') = ShaderData (mappend u u') (mappend t t')


append :: ShaderData u t -> ShaderData u' t' -> ShaderData (u ++ u') (t ++ t')
append (ShaderData u t) (ShaderData u' t') = ShaderData (u <+> u') (t <+> t')

setUniforms :: forall ts. UniformFields (Uniforms ts)
            => ShaderProgram -> Uniforms ts -> IO ()
setUniforms s x = 
    V.setUniforms s x 
        `catch` \(e::ErrorCall) -> error . show $ format "{}: {}" (Shown e, Shown s)

instance Implicit (FieldNames '[]) where
    implicitly = RNil
