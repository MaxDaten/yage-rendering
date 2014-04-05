{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}

module Yage.Rendering.ResourceManager
    ( module Yage.Rendering.ResourceManager
    , module Types
    ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Geometry.Vertex

import           Foreign.Ptr                          (nullPtr)

import           Control.Monad.RWS                    hiding (forM, forM_, mapM_)


import           Graphics.GLUtil                      hiding (loadShader, loadTexture)
import           Graphics.Rendering.OpenGL            (($=))
import qualified Graphics.Rendering.OpenGL            as GL


import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.Backend.RenderPass
import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.Resources.Types       as Types


import           Foreign.Storable

import           Yage.Rendering.Lenses
import           Yage.Rendering.Uniforms
import qualified Yage.Rendering.Textures              as Tex
import           Yage.Rendering.Vertex
import           Yage.Rendering.Mesh                  as Mesh
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


requestFramebuffer :: (Show ident, MultipleRenderTargets mrt) => 
                   (ident, mrt) -> ResourceManager GLFramebuffer
requestFramebuffer (fboIdent, mrt) 
    | isDefault mrt = return GL.defaultFramebufferObject
    | otherwise =
    let compiler = const $ compileFBO <&> flip (^?!) folded -- unsafe head
    in requestResource compiledFBOs compiler updateFramebuffer (show fboIdent)

    where

    -- | creates a new framebuffer according to the given specs
    compileFBO = do
        tell [format "create fbo {0}" [ show fboIdent ]]
        fbo <- io $ GL.genObjectName
        io $ GL.bindFramebuffer GL.Framebuffer $= fbo

        forM_ (allAttachments mrt) attachTargetToFBO
        if (null $ fboColors mrt)
            then io $ GL.drawBuffer  $= GL.NoBuffers
            else io $ GL.drawBuffers $= (map toBufferMode $ fboColors mrt)

        status <- io . GL.get $ GL.framebufferStatus GL.Framebuffer
        return $ case status of
            GL.Complete -> Right fbo
            _           -> Left status

    attachTargetToFBO attchmnt =
        case toGLAttachment attchmnt of
            (glSlot, TextureTarget tt2d tex level) -> do
                (_, texObj) <- requestTexture tex
                io $ GL.framebufferTexture2D GL.Framebuffer glSlot tt2d texObj level
            (glSlot, RenderbufferTarget rbuff)     -> do
                (_, robj) <- requestRenderbuffer rbuff
                io $ GL.framebufferRenderbuffer GL.Framebuffer glSlot GL.Renderbuffer robj
            (_, NullTarget) -> return ()

    -- toGLAttachment :: GLFBOAttachment -> (GL.FramebufferObjectAttachment, GLFBOAttachment)
    toGLAttachment (Attachment (ColorAttachment index) target) = (GL.ColorAttachment (fromIntegral index), target)
    toGLAttachment (Attachment DepthAttachment target)         = (GL.DepthAttachment, target)
    toGLAttachment (Attachment StencilAttachment target)       = (GL.StencilAttachment, target)
    toGLAttachment (Attachment DepthStencilAttachment target)  = (GL.DepthStencilAttachment, target)

    toBufferMode (Attachment (ColorAttachment idx) _) = GL.FBOColorAttachment . fromIntegral $ idx
    toBufferMode _ = error "Yage.Rendering.ResourceManager: invalid buffer-mode"


    --updateFramebuffer :: Framebuffer TextureResource RenderbufferResource -> ResourceManager (Framebuffer TextureResource RenderbufferResource)
    updateFramebuffer f = do
        forM_  (allAttachments mrt) requestAttachment
        return f

    requestAttachment :: RenderTargets -> ResourceManager ()
    requestAttachment (Attachment _ (TextureTarget _ tex _))    = void $ requestTexture tex
    requestAttachment (Attachment _ (RenderbufferTarget rbuff)) = void $ requestRenderbuffer rbuff
    requestAttachment (Attachment _ NullTarget) = return ()


requestTexture :: Texture -> ResourceManager GLTexture
requestTexture res = requestResource loadedTextures loadTexture (resizeTexture res) res
    -- RESIZE HERE
    where
    -- | pushes texture to opengl
    loadTexture :: Texture -> ResourceManager GLTexture
    loadTexture (TextureImage _ img) = io $ do
        checkErrorOf ("loadTexture TextureImage: " ++ show res) $
            (imgSpec img,) <$> (handleTexObj GL.Texture2D =<< Tex.readTextureImg img)

    loadTexture (TextureImageCube _ cubemap) = io $
        (imgSpec . Tex.cubeFaceRight $ cubemap,) <$> (handleTexObj GL.TextureCubeMap =<< Tex.readCubeTextures cubemap)
    
    loadTexture (TextureBuffer name target spec) =
        let size    = fromIntegral <$> spec^.glBufferSize
            texSize = GL.TextureSize2D (size^._x) (size^._y)
            fmt     = spec^.glBufferFormat
        in do
            tell [ format "create tex {0} to {1}" [name, show size] ]
            checkErrorOf ("loadTexture TextureBuffer: " ++ show res) $ io $ do
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

    handleTexObj target res  =
        case res of
        Left msg  -> error msg
        Right obj -> checkErrorOf ("Texture settings: " ++ show res) $ do
            GL.textureBinding target $= Just obj
            GL.textureFilter  target $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
            GL.textureBinding target $= Nothing
            return obj

    resizeTexture (TextureBuffer name target newSpec) (currentSpec, texObj) = do
        let newSize = fromIntegral <$> newSpec^.glBufferSize
            texSize = GL.TextureSize2D (newSize^._x) (newSize^._y)
            fmt     = newSpec^.glBufferFormat
        when (newSpec^.glBufferSize /= currentSpec^.glBufferSize) $ do
            tell [ format "resize tex {0} to {1}" [name, show newSize] ]
            checkErrorOf ("Texture resize: " ++ show res) $ io $ do
                GL.textureBinding target $= Just texObj
                GL.texImage2D target GL.NoProxy 0 fmt texSize 0 (GL.PixelData (texFormat fmt) GL.UnsignedByte nullPtr)
                GL.textureBinding target $= Nothing
        return (newSpec, texObj)
    resizeTexture _ (old, texObj) = return (old, texObj)



requestRenderbuffer :: Renderbuffer -> ResourceManager GLRenderbuffer
requestRenderbuffer res = requestResource loadedRenderbuffers loadRenderbuffer (resizeBuffer res) res
    where

    loadRenderbuffer (Renderbuffer name spec@(GLBufferSpec fmt sz)) =
        let size = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
        in do
            tell [ format "create buffer {0} to {1}" [name, show sz] ]
            checkErrorOf ("loadRenderbuffer: ") $ io $ do
                rObj <- GL.genObjectName
                GL.bindRenderbuffer GL.Renderbuffer $= rObj
                GL.renderbufferStorage GL.Renderbuffer fmt size
                GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (spec, rObj)

    resizeBuffer (Renderbuffer name newSpec) (currentSpec, rObj) = do
        let newSize  = fromIntegral <$> newSpec^.glBufferSize
            buffSize = GL.RenderbufferSize (newSize^._x) (newSize^._y)
            fmt      = newSpec^.glBufferFormat
        when (newSpec^.glBufferSize /= currentSpec^.glBufferSize) $ do
            tell [ format "resize buffer {0} to {1}" [name, show newSize] ]
            checkErrorOf ("resizeBuffer: ") $ io $ do
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
        io $ bufferVertices (mesh^.meshVertices)

    updateVertexBuffer (oldHash, buff) = do
        let vBuff = BufferedVertices buff
        when (mesh^.meshHash /= oldHash) $ io $ reloadVertices vBuff (mesh^.meshVertices)
        return vBuff


{--
TODO reimplement
requestElementBuffer (ident, indices) = do
requestElementBuffer :: (ident, V.Vector Int) -> ResourceManager EBO
    eMap <- use loadedElementBuffer

    ebo <- maybe
            (loadElementBuffer)
            (updateElementBuffer)
            (eMap^.at (mesh^.meshId))

    loadedElementBuffer.at (mesh^.meshId) ?= (mesh^.meshHash, ebo)
    return ebo
    where

    loadElementBuffer =
        io $ bufferIndices (VS.map fromIntegral $ getMeshElementIndices mesh)

    updateElementBuffer (oldHash, buff) = io $ do
        GL.bindBuffer GL.ElementArrayBuffer $= Just buff
        let elems = VS.map fromIntegral $ getMeshElementIndices mesh :: VS.Vector Word32
        when (mesh^.meshHash /= oldHash) $ replaceVector GL.ElementArrayBuffer elems
        GL.bindBuffer GL.ElementArrayBuffer $= Nothing
        return buff
--}


requestVAO :: (ViableVertex (Vertex vr)) => Mesh vr -> ShaderResource -> ResourceManager GLVertexArray
requestVAO mesh shader = aux (mesh^.meshId,shader) 
    where
    loadVertexArray _ _ = do
        vbuff           <- requestVertexbuffer mesh
        shaderProg      <- requestShader shader

        tell [ format "RenderSet: {0} - {1}" [show mesh, show shader] ] 
        io $ makeVAO $ do
            bindVertices vbuff
            enableVertices' shaderProg vbuff
            -- it is really part of vao:
            -- http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            --GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
    aux = requestResource loadedVertexArrays (uncurry loadVertexArray) return



requestRenderSet :: (ViableVertex (Vertex vr), UniformFields (Uniforms u))
                => ShaderResource 
                -> Uniforms u
                -> RenderEntity vr
                -> ResourceManager (RenderSet u)
requestRenderSet withProgram entityUniforms ent = do
    RenderSet  <$> ( requestVAO (ent^.renderData) withProgram )
               <*> ( pure $ entityUniforms )
               <*> ( forM (ent^.entityTextures) makeTexAssignment )
               <*> ( pure $ fromIntegral . Mesh.vertexCount $ ent^.renderData )
               <*> ( pure $ ent^.drawSettings )



requestFramebufferSetup :: (UniformFields (Uniforms global), Show ident, MultipleRenderTargets mrt) =>
                        PassDescr ident mrt ent global local -> ResourceManager (FramebufferSetup global)
requestFramebufferSetup PassDescr{..} = 
    FramebufferSetup 
        <$> case passTarget of
            RenderTarget ident mrt -> requestFramebuffer (ident, mrt)
        <*> requestShader passShader
        <*> pure passGlobalUniforms
        <*> forM passGlobalTextures makeTexAssignment
        <*> pure passPreRendering
        <*> pure passPostRendering


data TT = forall t. (GL.BindableTextureTarget t, Show t) => TT t
makeTexAssignment :: TextureDefinition -> ResourceManager TextureAssignment
makeTexAssignment tex =
    let ch        = fromIntegral $ tex^.texChannel._1
        name      = tex^.texChannel._2
    in case tex^.texResource.to glTarget of
       (TT target) ->
            TextureAssignment <$> (snd <$> requestTexture (tex^.texResource))
                              <*> pure target
                              <*> pure ch
                              <*> pure name
    where
    glTarget (TextureImage _ _)     = TT GL.Texture2D
    glTarget (TextureImageCube _ _) = TT GL.TextureCubeMap
    glTarget (TextureBuffer _ t _)  = TT t


---------------------------------------------------------------------------------------------------
initialGLRenderResources :: GLResources
initialGLRenderResources =
    GLResources mempty mempty mempty mempty mempty mempty


replaceIndices :: [Word32] -> IO ()
replaceIndices = replaceBuffer GL.ElementArrayBuffer


