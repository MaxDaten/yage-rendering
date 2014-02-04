{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# LANGUAGE NamedFieldPuns               #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE RecordWildCards              #-}
{-# LANGUAGE Rank2Types                   #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TupleSections                #-}

module Yage.Rendering.ResourceManager
    ( module Yage.Rendering.ResourceManager
    , module Types
    ) where

import           Yage.Prelude

import           Foreign.Ptr                      (nullPtr)
import           Data.List

import           Control.Monad
import           Control.Monad.Trans.State.Strict


import           Graphics.GLUtil                  hiding (loadShader, loadTexture)
import           Graphics.Rendering.OpenGL        (($=))
import qualified Graphics.Rendering.OpenGL        as GL

import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.ResourceManager.Types as Types

import           Yage.Rendering.Lenses
import qualified Yage.Rendering.Texture           as Tex
import           Yage.Rendering.Types             hiding (Index)
import           Yage.Rendering.VertexSpec


---------------------------------------------------------------------------------------------------




runResourceManager :: (MonadIO m) => GLResources -> ResourceManager a -> m (a, GLResources)
runResourceManager res rm = io $ runStateT rm res


requestResource :: (Ord d) => Lens' GLResources (Map d r) -> (d -> ResourceManager r) -> d -> ResourceManager r 
requestResource accessor loader resource = do
    resmap <- use accessor
    unless (resmap^.contains resource) $ do
        item <- loader resource
        accessor.at resource ?= item
    use accessor <&> \m -> m^.at resource ^?! _Just


requestFramebuffer :: (String, FramebufferSpec TextureResource RenderbufferResource) -> ResourceManager Framebuffer
requestFramebuffer (fboIdent, spec) =
    let compiler = const $ compileFBO spec <&> flip (^?!) folded -- unsafe head 
    in requestResource compiledFBOs compiler fboIdent

    where 

    -- | creates a new framebuffer according to the given specs
    compileFBO :: FramebufferSpec TextureResource RenderbufferResource -> ResourceManager (Either GL.FramebufferStatus Framebuffer)
    compileFBO spec = do
        fbo <- io $ GL.genObjectName
        io $ GL.bindFramebuffer GL.Framebuffer $= fbo
        
        doAttachments
        when (null $ spec^.fboColors) $ io $ GL.drawBuffer $= GL.NoBuffers
        
        status <- io . GL.get $ GL.framebufferStatus GL.Framebuffer
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

        attachTargetToFBO index attchmnt = 
            case toGLAttachment attchmnt index of
                (glAt, TextureTarget tt2d tex level) -> do
                    texObj <- requestTexture tex
                    io $ GL.framebufferTexture2D GL.Framebuffer glAt tt2d texObj level
                (glAt, RenderbufferTarget rbuff)     -> do
                    robj <- requestRenderbuffer rbuff 
                    io $ GL.framebufferRenderbuffer GL.Framebuffer glAt GL.Renderbuffer robj

        toGLAttachment :: FramebufferAttachment TextureResource RenderbufferResource -> Int -> (GL.FramebufferObjectAttachment, AttachmentTarget TextureResource RenderbufferResource)
        toGLAttachment (FramebufferAttachment ColorAttachment target) index    = (GL.ColorAttachment (fromIntegral index), target)
        toGLAttachment (FramebufferAttachment DepthAttachment target) _        = (GL.DepthAttachment, target)
        toGLAttachment (FramebufferAttachment StencilAttachment target) _      = (GL.StencilAttachment, target)
        toGLAttachment (FramebufferAttachment DepthStencilAttachment target) _ = (GL.DepthStencilAttachment, target)



requestTexture :: TextureResource -> ResourceManager GLTexture
requestTexture = requestResource loadedTextures loadTexture
    where
    -- | pushes texture to opengl
    loadTexture :: TextureResource -> ResourceManager GLTexture
    loadTexture (TextureImage _ img) = io $
        handleTexObj =<< Tex.readTextureImg img

    loadTexture (TextureFile texFile) = io $
        handleTexObj =<< (Tex.readTexture . encodeString $ texFile)

    loadTexture (TextureBuffer _ (GLTextureSpec target format (w,h))) = io $ do
        texObj <- GL.genObjectName
        GL.textureBinding target $= Just texObj
        GL.texImage2D target GL.NoProxy 0 format 
            (GL.TextureSize2D (fromIntegral w) (fromIntegral h)) 0 
            (GL.PixelData GL.RGB GL.UnsignedByte nullPtr)
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
        GL.textureBinding target $= Nothing
        -- TODO release binding
        return texObj

    handleTexObj res = do
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
        printErrorMsg $ "tex: " ++ show res
        case res of
            Left msg  -> error msg
            Right obj -> return obj


requestRenderbuffer :: RenderbufferResource -> ResourceManager GLRenderbuffer
requestRenderbuffer = requestResource loadedRenderbuffers loadRenderbuffer
    where

    loadRenderbuffer (RenderbufferResource _ (GLRenderbufferSpec format (w,h))) = io $ do
        rObj <- GL.genObjectName
        GL.bindRenderbuffer GL.Renderbuffer $= rObj
        GL.renderbufferStorage GL.Renderbuffer format (GL.RenderbufferSize (fromIntegral w) (fromIntegral h))
        return rObj


requestShader :: ShaderResource -> ResourceManager GLShader
requestShader = requestResource loadedShaders loadShader
    where
    loadShader :: ShaderResource -> ResourceManager ShaderProgram
    loadShader res = io $!
        simpleShaderProgram (encodeString $ res^.srVertSrc) (encodeString $ res^.srFragSrc)



requestVertexbuffer :: Mesh -> ResourceManager GLVertexbuffer
requestVertexbuffer mesh = do
    buff <- requestResource loadedVertexBuffer loadVertexBuffer mesh
    updateVertexBuffer mesh buff
    return buff
    
    where
    loadVertexBuffer :: Mesh -> ResourceManager GLVertexbuffer
    loadVertexBuffer Mesh{_meshModToken, _meshData, _meshAttr} =
        io $ (_meshModToken,,)
                <$> makeVertexBufferF (_meshData^.to _meshAttr)
                <*> (bufferIndices $ map fromIntegral $ _meshData^.mDataIndices)

    updateVertexBuffer :: Mesh -> GLVertexbuffer -> ResourceManager ()
    updateVertexBuffer mesh@Mesh{_meshModToken, _meshData, _meshAttr} (oldtoken, buff, ebo) = do
        unless (_meshModToken == oldtoken) $ do
            io $ do
                _ <- updateVertexBufferF (_meshData^.to _meshAttr) buff
                GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
                replaceIndices $ map fromIntegral $ _meshData^.mDataIndices
                GL.bindBuffer GL.ElementArrayBuffer $= Nothing
            loadedVertexBuffer.at mesh ?= (_meshModToken, buff, ebo)


requestRenderItem :: Mesh -> ShaderResource -> ResourceManager GLVertexArray
requestRenderItem mesh shader = requestResource loadedVertexArrays (uncurry loadVertexArray) (mesh,shader)
    where
    loadVertexArray :: Mesh -> ShaderResource -> ResourceManager GLVertexArray
    loadVertexArray mesh shader = do
        (_, buff, ebo)  <- requestVertexbuffer mesh
        shaderProg      <- requestShader shader
        io $ makeVAO $ do
            GL.bindBuffer GL.ArrayBuffer        $= Just (vbo buff) -- not part of state, neccessary for vad
            mapM_ (setVertexAttribute shaderProg) (attribVADs buff) -- just this relevant?

            -- is really part of vao:
            -- http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            GL.bindBuffer GL.ElementArrayBuffer $= Just ebo  

    setVertexAttribute prog (VertexDescriptor name vad) = do
        enableAttrib prog name -- not neccessary ?
        setAttrib prog name GL.ToFloat vad

---------------------------------------------------------------------------------------------------
initialGLRenderResources :: GLResources
initialGLRenderResources = 
    GLResources mempty mempty mempty mempty mempty mempty

defaultFramebuffer :: Framebuffer
defaultFramebuffer = Framebuffer GL.defaultFramebufferObject mempty (const (return ()))

replaceIndices :: [Word32] -> IO ()
replaceIndices = replaceBuffer GL.ElementArrayBuffer




