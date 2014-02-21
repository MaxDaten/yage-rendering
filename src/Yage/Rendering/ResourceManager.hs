{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Yage.Rendering.ResourceManager
    ( module Yage.Rendering.ResourceManager
    , module Types
    ) where

import           Yage.Prelude
import           Yage.Geometry.Vertex

import           Data.List

import           Foreign.Ptr                          (nullPtr)

import           Control.Monad.RWS


import           Graphics.GLUtil                      hiding (loadShader, loadTexture)
import           Graphics.Rendering.OpenGL            (($=))
import qualified Graphics.Rendering.OpenGL            as GL


import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.Resources.Types       as Types


import           Foreign.Storable

import           Yage.Rendering.Lenses
import qualified Yage.Rendering.Texture               as Tex
import           Yage.Rendering.Vertex
import           Yage.Rendering.Types                 hiding (Index)


import           Linear                               (V2 (..), _x, _y)


---------------------------------------------------------------------------------------------------




runResourceManager :: (MonadIO m) => GLResources -> ResourceManager a -> m (a, GLResources, [String])
runResourceManager res rm = io $ runRWST rm () res


requestResource :: (Ord key)
                => Lens' GLResources (Map key res)          -- ^ state-field accessor
                -> (key -> ResourceManager res)             -- ^ loading function
                -> (res -> ResourceManager res)             -- ^ updating function
                -> key                                      -- ^ resource to load
                -> ResourceManager res
requestResource accessor loader updater resource = do
    resmap <- use accessor
    item <- maybe
        (loader resource)
        (updater)
        (resmap^.at resource)

    accessor.at resource ?= item
    return $ item


requestFramebuffer :: (String, FramebufferSpec TextureResource RenderbufferResource) -> ResourceManager GLFramebuffer
requestFramebuffer (fboIdent, spec) =
    let compiler = const $ compileFBO spec <&> flip (^?!) folded -- unsafe head
    in requestResource compiledFBOs compiler (updateFramebuffer spec) fboIdent

    where

    -- | creates a new framebuffer according to the given specs
    compileFBO :: FramebufferSpec TextureResource RenderbufferResource -> ResourceManager (Either GL.FramebufferStatus GLFramebuffer)
    compileFBO spec = do
        tell [format "create fbo {0}" [ fboIdent ]]
        fbo <- io $ GL.genObjectName
        io $ GL.bindFramebuffer GL.Framebuffer $= fbo

        doAttachments spec
        if (null $ spec^.fboColors)
            then io $ GL.drawBuffer  $= GL.NoBuffers
            else io $ GL.drawBuffers $= (mapColorAttachments $ spec^.fboColors)

        status <- io . GL.get $ GL.framebufferStatus GL.Framebuffer
        return $ case status of
            GL.Complete -> Right $ Framebuffer fbo spec
            _           -> Left status

    doAttachments spec = do
        attaching $ spec^.fboColors
        attaching $ spec^.fboStencil^..traverse
        attaching $ spec^.fboDepth^..traverse
        attaching $ spec^.fboDepthStencil^..traverse

    --attachTargetToFBO :: Int -> FramebufferAttachment -> IO ()
    attaching = imapM_ attachTargetToFBO

    attachTargetToFBO index attchmnt =
        case toGLAttachment attchmnt index of
            (glAt, TextureTarget tt2d tex level) -> do
                (_, texObj) <- requestTexture tex
                io $ GL.framebufferTexture2D GL.Framebuffer glAt tt2d texObj level
            (glAt, RenderbufferTarget rbuff)     -> do
                (_, robj) <- requestRenderbuffer rbuff
                io $ GL.framebufferRenderbuffer GL.Framebuffer glAt GL.Renderbuffer robj

    toGLAttachment :: FramebufferAttachment TextureResource RenderbufferResource -> Int -> (GL.FramebufferObjectAttachment, AttachmentTarget TextureResource RenderbufferResource)
    toGLAttachment (FramebufferAttachment ColorAttachment target) index    = (GL.ColorAttachment (fromIntegral index), target)
    toGLAttachment (FramebufferAttachment DepthAttachment target) _        = (GL.DepthAttachment, target)
    toGLAttachment (FramebufferAttachment StencilAttachment target) _      = (GL.StencilAttachment, target)
    toGLAttachment (FramebufferAttachment DepthStencilAttachment target) _ = (GL.DepthStencilAttachment, target)

    mapColorAttachments = imap toBufferMode
    toBufferMode index _ = GL.FBOColorAttachment $ fromIntegral index

    updateFramebuffer :: FramebufferSpec TextureResource RenderbufferResource -> GLFramebuffer -> ResourceManager GLFramebuffer
    updateFramebuffer newspec f@(Framebuffer _fbo _oldspec) = do
        let attachments = (newspec^.fboColors) ++
                          (newspec^.fboStencil^..traverse) ++
                          (newspec^.fboDepth^..traverse) ++
                          (newspec^.fboDepthStencil^..traverse)
        mapM_ requestAttachment attachments
        return $ f{_spec = newspec}

    requestAttachment :: FramebufferAttachment TextureResource RenderbufferResource -> ResourceManager ()
    requestAttachment (FramebufferAttachment _ (TextureTarget _ tex _))    = void $ requestTexture tex
    requestAttachment (FramebufferAttachment _ (RenderbufferTarget rbuff)) = void $ requestRenderbuffer rbuff


requestTexture :: TextureResource -> ResourceManager GLTexture
requestTexture res = requestResource loadedTextures loadTexture (resizeTexture res) res
    -- RESIZE HERE
    where
    -- | pushes texture to opengl
    loadTexture :: TextureResource -> ResourceManager GLTexture
    loadTexture (TextureImage _ img) = io $
        (imgSpec img,) <$> (handleTexObj =<< Tex.readTextureImg img)

    loadTexture (TextureFile texFile) = io $ do
        eimg     <- Tex.readImage $ encodeString texFile
        case eimg of
            Left msg    -> error msg
            Right img   -> do
                texObj  <- handleTexObj =<< (Tex.readTextureImg img)
                return (imgSpec img, texObj)

    loadTexture (TextureBuffer name target spec) =
        let size    = fromIntegral <$> spec^.glBufferSize
            texSize = GL.TextureSize2D (size^._x) (size^._y)
            fmt     = spec^.glBufferFormat
        in do
            tell [ format "create tex {0} to {1}" [name, show size] ]
            io $ do
                texObj <- GL.genObjectName
                GL.textureBinding target $= Just texObj

                GL.textureLevelRange target $= (0,0)
                GL.textureFilter target     $= ((GL.Nearest, Nothing), GL.Nearest) -- TODO Def
                GL.texImage2D target GL.NoProxy 0 fmt texSize 0 (GL.PixelData (texFormat fmt) GL.UnsignedByte nullPtr)

                GL.textureBinding target $= Nothing
                return (spec, texObj)

    imgSpec :: Tex.DynamicImage -> GLBufferSpec
    imgSpec img = GLBufferSpec GL.RGB8 $ V2 (Tex.dynamicMap Tex.imageWidth img) (Tex.dynamicMap Tex.imageHeight img)

    texFormat :: GL.PixelInternalFormat -> GL.PixelFormat
    texFormat GL.DepthComponent24  = GL.DepthComponent
    texFormat GL.DepthComponent32  = GL.DepthComponent
    texFormat GL.DepthComponent32f = GL.DepthComponent
    texFormat GL.DepthComponent16  = GL.DepthComponent
    texFormat GL.RGB8              = GL.RGB
    texFormat GL.RGBA8             = GL.RGBA
    texFormat GL.RGBA'             = GL.RGBA
    texFormat _ = error "Yage.Rendering.ResourceManager.texFormat: not supported internal format"

    handleTexObj res = do
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
        printErrorMsg $ "tex: " ++ show res
        case res of
            Left msg  -> error msg
            Right obj -> return obj

    resizeTexture (TextureBuffer name target newSpec) (currentSpec, texObj) = do
        let newSize = fromIntegral <$> newSpec^.glBufferSize
            texSize = GL.TextureSize2D (newSize^._x) (newSize^._y)
            fmt     = newSpec^.glBufferFormat
        when (newSpec^.glBufferSize /= currentSpec^.glBufferSize) $ do
            tell [ format "resize tex {0} to {1}" [name, show newSize] ]
            io $ do
                GL.textureBinding target $= Just texObj
                GL.texImage2D target GL.NoProxy 0 fmt texSize 0 (GL.PixelData (texFormat fmt) GL.UnsignedByte nullPtr)
                GL.textureBinding target $= Nothing
        return (newSpec, texObj)
    resizeTexture _ (old, texObj) = return (old, texObj)



requestRenderbuffer :: RenderbufferResource -> ResourceManager GLRenderbuffer
requestRenderbuffer res = requestResource loadedRenderbuffers loadRenderbuffer (resizeBuffer res) res
    where

    loadRenderbuffer (RenderbufferResource name spec@(GLBufferSpec fmt sz)) =
        let size = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
        in do
            tell [ format "create buffer {0} to {1}" [name, show sz] ]
            io $ do
                rObj <- GL.genObjectName
                GL.bindRenderbuffer GL.Renderbuffer $= rObj
                GL.renderbufferStorage GL.Renderbuffer fmt size
                GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (spec, rObj)

    resizeBuffer (RenderbufferResource name newSpec) (currentSpec, rObj) = do
        let newSize  = fromIntegral <$> newSpec^.glBufferSize
            buffSize = GL.RenderbufferSize (newSize^._x) (newSize^._y)
            fmt      = newSpec^.glBufferFormat
        when (newSpec^.glBufferSize /= currentSpec^.glBufferSize) $ do
            tell [ format "resize buffer {0} to {1}" [name, show newSize] ]
            io $ do
                GL.bindRenderbuffer GL.Renderbuffer $= rObj
                GL.renderbufferStorage GL.Renderbuffer fmt buffSize
                GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
        return (newSpec, rObj)



requestShader :: ShaderResource -> ResourceManager GLShader
requestShader = requestResource loadedShaders loadShader return
    where
    loadShader :: ShaderResource -> ResourceManager ShaderProgram
    loadShader res = io $!
        simpleShaderProgram (encodeString $ res^.srVertSrc) (encodeString $ res^.srFragSrc)



requestVertexbuffer :: (Storable (Vertex vr)) => Mesh vr -> ResourceManager (BufferedVertices vr)
requestVertexbuffer mesh = do
    vMap <- use loadedVertexBuffer

    vbuff <- maybe
                (loadVertexBuffer)
                (updateVertexBuffer)
                (vMap^.at (mesh^.meshId))

    loadedVertexBuffer.at (mesh^.meshId) ?= (mesh^.meshHash, getVertexBuffer vbuff)
    return vbuff

    where

    loadVertexBuffer = 
        io $ bufferVertices (mesh^.meshData)

    updateVertexBuffer (oldHash, buff) = do
        let vBuff = BufferedVertices buff
        when (mesh^.meshHash /= oldHash) $ io $ reloadVertices vBuff (mesh^.meshData)
        return vBuff




requestRenderSet :: (ViableVertex (Vertex vr)) => Mesh vr -> ShaderResource -> ResourceManager GLVertexArray
requestRenderSet mesh shader = requestResource loadedVertexArrays (uncurry loadVertexArray) return (mesh^.meshId,shader)
    where
    --loadVertexArray :: (ViableVertex (Vertex vr)) => Mesh vr -> MeshId -> ShaderResource -> ResourceManager GLVertexArray
    loadVertexArray _ _ = do
        vbuff           <- requestVertexbuffer mesh
        shaderProg      <- requestShader shader
        tell [ format "RenderSet: {0} - {1}" [show mesh, show shader] ] 
        io $ makeVAO $ do
            bindVertices vbuff
            enableVertices' shaderProg vbuff
            -- is really part of vao:
            -- http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            --GL.bindBuffer GL.ElementArrayBuffer $= Just ebo

---------------------------------------------------------------------------------------------------
initialGLRenderResources :: GLResources
initialGLRenderResources =
    GLResources mempty mempty mempty mempty mempty mempty

defaultFramebuffer :: GLFramebuffer
defaultFramebuffer = Framebuffer GL.defaultFramebufferObject mempty

replaceIndices :: [Word32] -> IO ()
replaceIndices = replaceBuffer GL.ElementArrayBuffer


