{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# LANGUAGE NamedFieldPuns               #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE RecordWildCards              #-}
module Yage.Rendering.ResourceManager
    ( module Yage.Rendering.ResourceManager
    , module Types
    ) where

import           Yage.Prelude

import           Foreign.Ptr                      (nullPtr)
import           Data.List

import           Control.Monad.Trans.State.Strict

import           Graphics.GLUtil                  hiding (loadShader, loadTexture)
import           Graphics.Rendering.OpenGL        (($=))
import qualified Graphics.Rendering.OpenGL        as GL

import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.ResourceManager.Types as Types

import           Yage.Rendering.Lenses
import qualified Yage.Rendering.Texture           as Tex
import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec


---------------------------------------------------------------------------------------------------



instance ResourceManaging GLResourceManager where
    requestFramebuffer   = undefined
    requestTexture       = undefined
    requestShader        = undefined
    requestGeometry      = undefined
    requestRenderItem    = undefined



runResourceManager :: (MonadIO m) => res -> ResourceManager res a -> m (a, res)
runResourceManager res rm = io $ runStateT rm res


{--

    do
    mapM_ (loadRenderResourcesFor . _entityRenderDef) =<< view entities
    fbo <- view fbufferSpec
    case fbo of
        Just spec -> loadFramebuffer spec
        Nothing   -> return ()

    where

    loadFramebuffer :: (String, FramebufferSpec TextureResource RenderbufferResource) -> ResourceManager ()
    loadFramebuffer (fboIdent, spec) = do
        res <- get
        unless (res^.compiledFBOs.contains fboIdent) $ do
            Right fbo <- compileFBO spec
            compiledFBOs.at fboIdent ?= fbo


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
                            prefetchTexture tex -- TODO : capture this with new loadAndGet
                            Just texObj <- use $ loadedTextures.at tex
                            io $ GL.framebufferTexture2D GL.Framebuffer glAt tt2d texObj level
                    (glAt, RenderbufferTarget rbuff)       -> do
                        prefetchRenderbuffer rbuff
                        Just robj <- use $ loadedRenderbuffers.at rbuff 
                        io $ GL.framebufferRenderbuffer GL.Framebuffer glAt GL.Renderbuffer robj

            toGLAttachment :: FramebufferAttachment TextureResource RenderbufferResource -> Int -> (GL.FramebufferObjectAttachment, AttachmentTarget TextureResource RenderbufferResource)
            toGLAttachment (FramebufferAttachment ColorAttachment target) index    = (GL.ColorAttachment (fromIntegral index), target)
            toGLAttachment (FramebufferAttachment DepthAttachment target) _        = (GL.DepthAttachment, target)
            toGLAttachment (FramebufferAttachment StencilAttachment target) _      = (GL.StencilAttachment, target)
            toGLAttachment (FramebufferAttachment DepthStencilAttachment target) _ = (GL.DepthStencilAttachment, target)
 


    loadRenderResourcesFor :: RenderDefinition -> ResourceManager ()
    loadRenderResourcesFor rdef = do
        let shRes = rdef^.rdefProgram.shaderRes

        -- Shader on demand loading
        prefetchShader shRes

        -- VertexBuffer on demand with shader prog for vertex attributes
        prefetchVertexBuffer (rdef^.rdefData)
        updateVertexBuffer   (rdef^.rdefData)
        prefetchVertexArray  (rdef^.rdefData) shRes

        -- TextureObjects on demand
        forM_ (rdef^.rdefTextures^..traverse.texResource) prefetchTexture
    

    prefetchShader :: ShaderResource -> ResourceManager ()
    prefetchShader shRes = do
        res <- get
        unless (res^.loadedShaders.contains shRes) $ do
            shaderProg <- loadShader shRes
            loadedShaders.at shRes ?= shaderProg

    prefetchVertexBuffer :: Mesh -> ResourceManager ()
    prefetchVertexBuffer mesh@Mesh{_meshModToken, _meshData, _meshAttr} = do
        res <- get
        unless (res^.loadedVertexBuffer.contains mesh) $ do
            buff <- io $ makeVertexBufferF (_meshData^.to _meshAttr)
            ebo  <- io $ bufferIndices $ map fromIntegral $ _meshData^.mDataIndices
            loadedVertexBuffer.at mesh ?= (_meshModToken, buff, ebo)

    updateVertexBuffer :: Mesh -> ResourceManager ()
    updateVertexBuffer mesh@Mesh{_meshData, _meshAttr} = do
        res <- get
        let (oldtoken, buff, ebo) = res^.loadedVertexBuffer.at mesh ^?!_Just
            currentToken          = _meshModToken mesh 
        unless (oldtoken == currentToken) $ do
            io $ do
                _ <- updateVertexBufferF (_meshData^.to _meshAttr) buff
                GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
                replaceIndices $ map fromIntegral $ _meshData^.mDataIndices
                GL.bindBuffer GL.ElementArrayBuffer $= Nothing
            loadedVertexBuffer.at mesh ?= (currentToken, buff, ebo)

    prefetchVertexArray :: Mesh -> ShaderResource -> ResourceManager ()
    prefetchVertexArray mesh shRes = do
        res <- get
        unless (res^.loadedVertexArrays.contains (mesh, shRes)) $ do
            let shaderProg = res^.loadedShaders.at shRes ^?!_Just
            vao            <- loadVertexArray mesh shaderProg
            loadedVertexArrays.at (mesh, shRes) ?= vao

    prefetchTexture :: TextureResource -> ResourceManager ()
    prefetchTexture texture = do
        res <- get
        unless (res^.loadedTextures.contains texture) $ do
            texObj <- loadTexture texture
            loadedTextures.at texture ?= texObj


    prefetchRenderbuffer :: RenderbufferResource -> ResourceManager ()
    prefetchRenderbuffer rbuff = do
        res <- get
        unless (res^.loadedRenderbuffers.contains rbuff) $ do
            rObj <- loadRenderbuffer rbuff
            loadedRenderbuffers.at rbuff ?= rObj


    -- | creates vbo and ebo, sets shader attributes and creates finally a vao
    loadVertexArray :: Mesh -> ShaderProgram -> ResourceManager VAO
    loadVertexArray mesh shaderProg = do
        res <- get
        let (_, buff, ebo) = res^.loadedVertexBuffer.at mesh ^?!_Just
        io $ makeVAO $ do
            GL.bindBuffer GL.ArrayBuffer        $= Just (vbo buff) -- not part of state, neccessary for vad
            mapM_ (setVertexAttribute shaderProg) (attribVADs buff) -- just this relevant?

            -- is really part of vao:
            -- http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            GL.bindBuffer GL.ElementArrayBuffer $= Just ebo  

    setVertexAttribute prog (VertexDescriptor name vad) = do
        enableAttrib prog name -- not neccessary ?
        setAttrib prog name GL.ToFloat vad

    -- | compiles shader
    loadShader :: ShaderResource -> ResourceManager ShaderProgram
    loadShader res = io $!
        simpleShaderProgram (encodeString $ res^.srVertSrc) (encodeString $ res^.srFragSrc)

    -- | pushes texture to opengl
    loadTexture :: TextureResource -> ResourceManager GL.TextureObject
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

    loadRenderbuffer (RenderbufferResource _ (GLRenderbufferSpec format (w,h))) = io $ do
        rObj <- GL.genObjectName
        GL.bindRenderbuffer GL.Renderbuffer $= rObj
        GL.renderbufferStorage GL.Renderbuffer format (GL.RenderbufferSize (fromIntegral w) (fromIntegral h))
        return rObj
--}

---------------------------------------------------------------------------------------------------
initialGLRenderResources :: GLRenderResources
initialGLRenderResources = mempty


defaultFramebuffer :: Framebuffer
defaultFramebuffer = Framebuffer GL.defaultFramebufferObject mempty (const (return ()))

replaceIndices :: [Word32] -> IO ()
replaceIndices = replaceBuffer GL.ElementArrayBuffer




