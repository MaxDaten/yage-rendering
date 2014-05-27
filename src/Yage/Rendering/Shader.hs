{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE UndecidableInstances         #-} -- FIXME : should not be neccessary
module Yage.Rendering.Shader
    ( module Yage.Rendering.Shader
    , module VinylGL
    ) where

import          Yage.Prelude
import          Yage.Lens

import          Data.Vinyl                as VinylGL
import          Data.Vinyl.Utils          as VinylGL
import          Data.Vinyl.Idiom.Identity as V

import          Graphics.VinylGL.Uniforms as VinylGL
import          Data.Vinyl.Reflect

import          Graphics.VinylGL.Vertex   as VinylGL


import          Graphics.GLUtil
import          Yage.Rendering.Resources.ResTypes


type Uniforms a = PlainRec a
type Textures a = PlainRec a

data ShaderData uniforms textures = ShaderData
    { _shaderUniforms :: Uniforms uniforms
    , _shaderTextures :: Textures textures
    }

makeLenses ''ShaderData

---------------------------------------------------------------------------------------------------
type UniformFields a   = (HasFieldNames a, HasFieldGLTypes a, SetUniformFields a)
type HashTextures t    = HasFieldAssocs t Texture
type IsShaderData u t  = (UniformFields (Uniforms u), HashTextures t)



type TextureUniform u = u ::: Texture

deriving instance Show ShaderProgram


type HasFieldAssocs ts t = ( FoldRec (Rec ts V.Identity) (V.Identity t), HasFieldNames (Rec ts V.Identity) )
fieldAssocs :: HasFieldAssocs ts t => 
            Rec ts V.Identity -> [(String, t)]
fieldAssocs r = fieldNames r `zip` recToList' r


instance (Monoid (Uniforms u), Monoid (Textures t)) => Monoid (ShaderData u t) where
    mempty = ShaderData mempty mempty
    (ShaderData u t) `mappend` (ShaderData u' t') = ShaderData (mappend u u') (mappend t t')


append :: ShaderData u t -> ShaderData u' t' -> ShaderData (u ++ u') (t ++ t')
append (ShaderData u t) (ShaderData u' t') = ShaderData (u <+> u') (t <+> t')