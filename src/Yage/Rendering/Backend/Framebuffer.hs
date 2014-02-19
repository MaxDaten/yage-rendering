{-# LANGUAGE TemplateHaskell            #-}
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
    = ColorAttachment -- (BufferMode, BufferMode) ColorSettings
    | DepthAttachment -- DepthSettings
    | StencilAttachment -- StencilSettings
    | DepthStencilAttachment -- (DepthSettings, StencilSettings)
    deriving (Eq, Ord)


data AttachmentTarget tex rbuff
    = TextureTarget GL.TextureTarget2D tex GL.Level 
    | RenderbufferTarget rbuff
    deriving (Eq, Ord)


data FramebufferAttachment tex rbuff
    = FramebufferAttachment FramebufferAttachmentSlot (AttachmentTarget tex rbuff)
    deriving (Eq, Ord)



data FramebufferSpec tex rbuff = FramebufferSpec
    { _fboColors       :: [FramebufferAttachment tex rbuff]
    , _fboDepth        :: Maybe (FramebufferAttachment tex rbuff)
    , _fboStencil      :: Maybe (FramebufferAttachment tex rbuff)
    , _fboDepthStencil :: Maybe (FramebufferAttachment tex rbuff)
    -- | GL Spec: Attaching a level of a texture to GL_DEPTH_STENCIL_ATTACHMENT 
    -- is equivalent to attaching that level to both the GL_DEPTH_ATTACHMENT and 
    -- the GL_STENCIL_ATTACHMENT attachment points simultaneously
    } deriving (Eq, Ord)

makeLenses ''FramebufferSpec


data Framebuffer tex rbuff = Framebuffer 
    { _fbo      :: GL.FramebufferObject 
    , _spec     :: FramebufferSpec tex rbuff
    }

makeLenses ''Framebuffer


-- type FramebufferAttachment = GL.FramebufferObjectAttachment

{--
data BufferMask = BitMask GL.GLuint
                | ColorMask (GL.Color4 GL.Capability)
                | DepthMask GL.Capability

data BufferClear = ClearColor (GL.Color4 GL.GL)


data BufferSettings = BufferSettings
    { bufferEnabled :: GL.Capability
    , bufferMask    :: BufferMask
    , bufferClear   :: 
    }
--}



instance Monoid (FramebufferSpec tex rbff) where
    mempty = FramebufferSpec mempty Nothing Nothing Nothing
    mappend (FramebufferSpec c d s ds) (FramebufferSpec c' d' s' ds') 
        = FramebufferSpec (c   <>  c') 
                          (d'  <|> d ) 
                          (s'  <|> s )
                          (ds' <|> ds)


mkEmptyFramebuffer :: FramebufferSpec tex rbuff
mkEmptyFramebuffer = mempty


attach :: FramebufferSpec tex rbuff -> FramebufferAttachment tex rbuff -> FramebufferSpec tex rbuff
attach s a@(FramebufferAttachment ColorAttachment _)        = s & fboColors       <>~ [a]
attach s a@(FramebufferAttachment DepthAttachment _)        = s & fboDepth        ?~ a
attach s a@(FramebufferAttachment StencilAttachment _)      = s & fboStencil      ?~ a
attach s a@(FramebufferAttachment DepthStencilAttachment _) = s & fboDepthStencil ?~ a

colorAttachment :: AttachmentTarget tex rbuff -> FramebufferSpec tex rbuff
colorAttachment = (attach mempty) . FramebufferAttachment ColorAttachment

depthAttachment :: AttachmentTarget tex rbuff -> FramebufferSpec tex rbuff
depthAttachment = (attach mempty) . FramebufferAttachment DepthAttachment

stencilAttachment :: AttachmentTarget tex rbuff -> FramebufferSpec tex rbuff
stencilAttachment = (attach mempty) . FramebufferAttachment StencilAttachment

depthStencilAttachment :: AttachmentTarget tex rbuff -> FramebufferSpec tex rbuff
depthStencilAttachment = (attach mempty) . FramebufferAttachment DepthStencilAttachment


---------------------------------------------------------------------------------------------------

getGLTarget :: FBOTarget -> GL.FramebufferTarget
getGLTarget DrawTarget        = GL.DrawFramebuffer 
getGLTarget ReadTarget        = GL.ReadFramebuffer
getGLTarget FramebufferTarget = GL.Framebuffer
