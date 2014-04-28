{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE UndecidableInstances        #-}
 -- some want this an error ;) 
{-- 
    some orphans, bad voodoo 
    => UndecidableInstances : is needed because of the PixelBaseComponent assoc type
--}
module Yage.Rendering.Textures.Instances where

import Yage.Prelude

import Data.Data

import Codec.Picture
import JuicySRGB

import qualified Graphics.Rendering.OpenGL      as GL

deriving instance Typeable Image

deriving instance Typeable PixelRGB8
deriving instance Data PixelRGB8

deriving instance Typeable PixelRGBF
deriving instance Data PixelRGBF

deriving instance Typeable PixelRGBA8
deriving instance Data PixelRGBA8

deriving instance Typeable PixelSRGB8
deriving instance Data PixelSRGB8

type TyPixel pixel = ( Data (PixelBaseComponent pixel)
                     , Typeable (PixelBaseComponent pixel)
                     , Storable (PixelBaseComponent pixel)
                     , Data pixel
                     )

deriving instance ( TyPixel pixel ) => Data (Image pixel)

deriving instance Typeable GL.DataType
deriving instance Data GL.DataType

deriving instance Typeable GL.PixelFormat
deriving instance Data GL.PixelFormat

deriving instance Typeable GL.PixelInternalFormat
deriving instance Data GL.PixelInternalFormat

deriving instance Typeable GL.TextureTarget2D
deriving instance Data GL.TextureTarget2D