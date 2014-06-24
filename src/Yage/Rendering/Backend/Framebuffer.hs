{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
module Yage.Rendering.Backend.Framebuffer
    ( module Yage.Rendering.Backend.Framebuffer
    ) where

import Yage.Prelude

import qualified   Graphics.Rendering.OpenGL       as GL
-- https://developer.apple.com/library/mac/documentation/graphicsimaging/conceptual/opengl-macprogguide/opengl_offscreen/opengl_offscreen.html

data FBOTarget = DrawTarget
               | FramebufferTarget
               | ReadTarget 

data FramebufferAttachmentSlot
    = ColorAttachment Int       -- (BufferMode, BufferMode) ColorSettings
    | DepthAttachment           -- DepthSettings
    | StencilAttachment         -- StencilSettings
    | DepthStencilAttachment    -- (DepthSettings, StencilSettings)
    deriving (Eq, Ord)


data AttachmentTarget tex rbuff
    = TextureTarget GL.TextureTarget2D tex GL.Level 
    | RenderbufferTarget rbuff
    deriving (Eq, Ord)


data AttachmentTypes tex rbuff
    = Attachment FramebufferAttachmentSlot (AttachmentTarget tex rbuff)
    deriving (Eq, Ord)


class FramebufferSpec f a | f -> a where
    fboColors       :: f -> [a]
    fboColors       = const []
    
    fboDepth        :: f -> Maybe a
    fboDepth        = const Nothing
    
    fboStencil      :: f -> Maybe a
    fboStencil      = const Nothing
    
    fboDepthStencil :: f -> Maybe a
    fboDepthStencil = const Nothing

data DefaultFramebuffer a = DefaultFramebuffer
instance FramebufferSpec (DefaultFramebuffer a) a where {}


allAttachments :: FramebufferSpec f a => f -> [a]
allAttachments fbo =
    fboColors fbo 
    ++ (maybeToList $ fboDepth fbo)
    ++ (maybeToList $ fboStencil fbo)
    ++ (maybeToList $ fboDepthStencil fbo) 


isDefault :: FramebufferSpec f a => f -> Bool
isDefault = null . allAttachments

data Framebuffer f = Framebuffer 
    { fbo      :: GL.FramebufferObject 
    , fboSpec  :: f
    }


---------------------------------------------------------------------------------------------------

