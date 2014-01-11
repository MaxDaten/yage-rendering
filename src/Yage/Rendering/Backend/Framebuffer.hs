{-# LANGUAGE TemplateHaskell            #-}
module Yage.Rendering.Backend.Framebuffer where

import Yage.Prelude

import Data.List (null)
import Control.Monad (when)

import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL       (get, ($=))

data Framebuffer = Framebuffer GL.FramebufferObject FramebufferSpec (Framebuffer -> IO ()) -- TODO will be replaced by settings


data AttachmentTarget = TextureTarget GL.TextureTarget2D GL.TextureObject GL.Level 
                      | RenderbufferTarget GL.RenderbufferObject
    deriving (Eq, Ord)

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

data FramebufferAttachment = ColorAttachment AttachmentTarget -- (BufferMode, BufferMode) ColorSettings
                           | DepthAttachment AttachmentTarget -- DepthSettings
                           | StencilAttachment AttachmentTarget -- StencilSettings
                           | DepthStencilAttachment AttachmentTarget -- (DepthSettings, StencilSettings)
    deriving (Eq, Ord)



data FramebufferSpec = FramebufferSpec
    { _fboColors       :: [FramebufferAttachment]
    , _fboDepth        :: Maybe FramebufferAttachment
    , _fboStencil      :: Maybe FramebufferAttachment
    -- | GL Spec: Attaching a level of a texture to GL_DEPTH_STENCIL_ATTACHMENT 
    -- is equivalent to attaching that level to both the GL_DEPTH_ATTACHMENT and 
    -- the GL_STENCIL_ATTACHMENT attachment points simultaneously
    } deriving (Eq, Ord)

makeLenses ''FramebufferSpec



instance Monoid FramebufferSpec where
    mempty = FramebufferSpec mempty Nothing Nothing
    mappend (FramebufferSpec c d s) (FramebufferSpec c' d' s') 
        = FramebufferSpec (c  <> c') 
                          (d' <|> d) 
                          (s' <|> s)


mkEmptyFramebuffer :: FramebufferSpec
mkEmptyFramebuffer = mempty


attach :: FramebufferSpec -> FramebufferAttachment -> FramebufferSpec
attach spec a@(ColorAttachment _)        = spec & fboColors <>~ [a]
attach spec a@(DepthAttachment _)        = spec & fboDepth   ?~ a
attach spec a@(StencilAttachment _)      = spec & fboStencil ?~ a
attach spec (DepthStencilAttachment target)   = spec `attach` (DepthAttachment target) 
                                                     `attach` (StencilAttachment target)

colorAttachment :: AttachmentTarget -> FramebufferSpec
colorAttachment = (attach mempty) . ColorAttachment

depthAttachment :: AttachmentTarget -> FramebufferSpec
depthAttachment = (attach mempty) . DepthAttachment

stencilAttachment :: AttachmentTarget -> FramebufferSpec
stencilAttachment = (attach mempty) . StencilAttachment

depthStencilAttachment :: AttachmentTarget -> FramebufferSpec
depthStencilAttachment = (attach mempty) . DepthStencilAttachment


-- | creates a new framebuffer according to the given specs
compileFBO :: FramebufferSpec -> IO (Either GL.FramebufferStatus Framebuffer)
compileFBO spec = do
    fbo <- GL.genObjectName
    GL.bindFramebuffer GL.Framebuffer $= fbo
    
    doAttachments
    when (null $ spec^.fboColors) $ GL.drawBuffer $= GL.NoBuffers

    
    status <- get $ GL.framebufferStatus GL.Framebuffer
    return $ case status of
        GL.Complete -> Right $ Framebuffer fbo spec (const (return ()))
        _           -> Left status 
    
    where
        doAttachments = do
            attaching $ spec^.fboColors
            attaching $ spec^.fboStencil^..traverse
            attaching $ spec^.fboDepth^..traverse

        --attachTargetToFBO :: Int -> FramebufferAttachment -> IO ()
        attaching = imapM_ attachTargetToFBO

        attachTargetToFBO index at = 
            case toGLAttachment at index of
                (glAt, TextureTarget tt2d tobj level) -> GL.framebufferTexture2D    GL.Framebuffer glAt tt2d tobj level
                (glAt, RenderbufferTarget robj)       -> GL.framebufferRenderbuffer GL.Framebuffer glAt GL.Renderbuffer robj

        toGLAttachment :: FramebufferAttachment -> Int -> (GL.FramebufferObjectAttachment, AttachmentTarget)
        toGLAttachment (ColorAttachment target) index    = (GL.ColorAttachment (fromIntegral index), target)
        toGLAttachment (DepthAttachment target) _        = (GL.DepthAttachment, target)
        toGLAttachment (StencilAttachment target) _      = (GL.StencilAttachment, target)
        toGLAttachment (DepthStencilAttachment target) _ = (GL.DepthStencilAttachment, target)



getCurrentFramebuffer :: IO (GL.FramebufferObject)
getCurrentFramebuffer = undefined


-- | the current bound fbo is NOT restored (lack of support by the OpenGL lib),
-- instead the default is restored 
withFramebuffer :: Framebuffer -> GL.FramebufferTarget -> (Framebuffer -> IO a) -> IO a
withFramebuffer fb@(Framebuffer fbo _ _) target action = do
    -- old <- return GL.FramebufferObject 0 -- TODO get real git glGetIntegerv GL_FRAMEBUFFER_BINDING
    GL.bindFramebuffer target $= fbo
    result <- action fb
    GL.bindFramebuffer target $= GL.defaultFramebufferObject
    return result

