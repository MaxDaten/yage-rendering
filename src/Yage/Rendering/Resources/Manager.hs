{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}

module Yage.Rendering.Resources.Manager where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Geometry.Vertex

import           Foreign.Ptr                          (nullPtr)

import           Control.Monad.RWS                    hiding (forM, forM_, mapM_)


import qualified Yage.Core.OpenGL                     as GL
import           Yage.Core.OpenGL                     (($=))


import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.Backend.RenderPass
import           Yage.Rendering.Backend.Renderer

import           Yage.Rendering.Resources.ResTypes
import           Yage.Rendering.Resources.Types



import           Yage.Rendering.Shader
import qualified Yage.Rendering.Textures              as Tex
import           Yage.Rendering.Vertex
import           Yage.Rendering.Mesh                  as Mesh
import           Yage.Rendering.RenderEntity


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
                   (ident, mrt) -> ResourceManager FramebufferRHI
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
                (_,_, texObj) <- requestTexture tex
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



requestTexture :: Texture -> ResourceManager TextureRHI
requestTexture res = requestResource loadedTextures newTexture (updateTexture res) res
    where

    newTexture :: Texture -> ResourceManager TextureRHI
    newTexture tex = do
        let chkErr = checkErrorOf ( format "newTexture: {0}" [show tex ] )
        
        obj <- chkErr $ loadData $ textureData tex
        
        tell [ format "newTexture: RHI = {0}: {1}" [ show tex, show obj ] ]
        return (textureSpec tex, textureConfig tex, obj)

    -- | pushes texture to opengl
    loadData :: TextureData -> ResourceManager GL.TextureObject
    loadData (Texture2D img) =
        io $ withNewGLTextureAt GL.Texture2D $ \obj -> do            
            Tex.loadTextureImage' GL.Texture2D img
            setTextureConfig res
            return obj

    loadData (TextureCube cubemap) = 
        io $ withNewGLTextureAt GL.TextureCubeMap $ \obj -> do
            Tex.loadCubeTextureImage cubemap
            setTextureConfig res
            return obj

    loadData (TextureBuffer target spec) =
        let V2 w h      = Tex.texSpecDimension spec
            texSize     = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
            Tex.PixelSpec dataType pxfmt internalFormat = Tex.texSpecPixelSpec spec
            glPixelData = GL.PixelData pxfmt dataType nullPtr
        in 
        io $ withNewGLTextureAt target $ \obj -> do
            GL.textureLevelRange target $= (0,0)
            setTextureConfig res
            GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 glPixelData
            return obj

    -- | creates a gl texture object and binds it to the target
    withNewGLTextureAt target ma = do
        texObj <- GL.genObjectName
        withTextureObj target texObj ma

    withTextureObj target texObj ma = do
        GL.textureBinding target $= Just texObj
        a <- ma texObj
        GL.textureBinding target $= Nothing
        return a

    updateTexture tex current = 
        updateConfig tex =<< resizeTexture tex current

    updateConfig tex@( Texture _ newConf _ ) current@(_, currentConf, _)
        | newConf == currentConf = return current
        | otherwise = do
            setTextureConfig tex
            return $ current & _2 .~ newConf
    
    resizeTexture tex@( Texture name _ (TextureBuffer target newSpec) ) current@(currentSpec, _, texObj) 
        | newSpec == currentSpec = return current
        | otherwise =
            let V2 newW newH    = Tex.texSpecDimension newSpec
                texSize         = GL.TextureSize2D (fromIntegral newW) (fromIntegral newH)
                Tex.PixelSpec dataType pxfmt internalFormat = Tex.texSpecPixelSpec newSpec
                glPixelData     = GL.PixelData pxfmt dataType nullPtr
            in do
                tell [ format "resizeTexture: {0}\nfrom:\t{1}\nto:\t{2}" [ show name, show currentSpec, show newSpec ] ]
                checkErrorOf ( format "resizeTexture: {0}" [ show tex ] ) $ io $ do
                    GL.textureBinding target $= Just texObj
                    GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 glPixelData
                    GL.textureBinding target $= Nothing
                return $ current & _1 .~ newSpec
    resizeTexture _ current = return current

    setTextureConfig (Texture name config texData) = 
        checkErrorOf (format "setTextureConfig {0} {1}" [ show name, show config ]) $ io $ do
        let TextureFiltering minification mipmap magnification = config^.texConfFiltering
            TextureWrapping repetition clamping = config^.texConfWrapping
            
        -- NOTE: filtering is neccessary for texture completeness
        case texData of
            Texture2D _   -> do
                when (isJust mipmap) $ GL.generateMipmap' GL.Texture2D
                GL.textureFilter   GL.Texture2D      $= ((minification, mipmap), magnification)
                GL.texture2DWrap   $= (repetition, clamping)
            
            TextureCube _ -> do 
                when (isJust mipmap) $ GL.generateMipmap' GL.TextureCubeMap
                GL.textureFilter   GL.TextureCubeMap      $= ((minification, mipmap), magnification)
                GL.texture3DWrap   GL.TextureCubeMap      $= (repetition, clamping)
            
            TextureBuffer target _ -> do
                when (isJust mipmap) $ GL.generateMipmap' target
                GL.textureFilter   target      $= ((minification, mipmap), magnification)
                GL.texture2DWrap   $= (repetition, clamping)


requestRenderbuffer :: Renderbuffer -> ResourceManager RenderbufferRHI
requestRenderbuffer res = requestResource loadedRenderbuffers loadRenderbuffer (resizeBuffer res) res
    where

    loadRenderbuffer buff@(Renderbuffer _ spec@(Tex.TextureImageSpec sz pxSpec)) =
        let size           = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
            internalFormat = Tex.pxSpecGLFormat pxSpec
        in do
            tell [ format "loadRenderbuffer: {0}" [ show buff ] ]
            checkErrorOf ("loadRenderbuffer: ") $ io $ do
                rObj <- GL.genObjectName
                GL.bindRenderbuffer GL.Renderbuffer $= rObj
                GL.renderbufferStorage GL.Renderbuffer internalFormat size
                GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (spec, rObj)

    resizeBuffer buff@(Renderbuffer _ newSpec@(Tex.TextureImageSpec sz pxSpec)) current@(currentSpec, rObj) 
        | newSpec == currentSpec = return current
        | otherwise =
            let size            = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
                internalFormat  = Tex.pxSpecGLFormat pxSpec
            in do 
                tell [ format "resizeBuffer {0}" [ show buff ] ]
                checkErrorOf ("resizeBuffer: ") $ io $ do
                    GL.bindRenderbuffer GL.Renderbuffer $= rObj
                    GL.renderbufferStorage GL.Renderbuffer internalFormat size
                    GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (newSpec, rObj)



requestShader :: ShaderResource -> ResourceManager ShaderRHI
requestShader = requestResource loadedShaders loadShader return
    where
    loadShader :: ShaderResource -> ResourceManager ShaderRHI
    loadShader res = io $!
        GL.simpleShaderProgram (encodeString $ res^.srVertSrc) (encodeString $ res^.srFragSrc)



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


requestVAO :: (ViableVertex (Vertex vr)) => Mesh vr -> ShaderResource -> ResourceManager VertexArrayRHI
requestVAO mesh shader = aux (mesh^.meshId,shader) 
    where
    loadVertexArray _ _ = do
        vbuff           <- requestVertexbuffer mesh
        shaderProg      <- requestShader shader

        tell [ format "RenderSet: {0} - {1}" [show mesh, show shader] ] 
        io $ GL.makeVAO $ do
            bindVertices vbuff
            enableVertices' shaderProg vbuff
            -- it is really part of vao:
            -- http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            --GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
    aux = requestResource loadedVertexArrays (uncurry loadVertexArray) return



requestRenderSet :: ( ViableVertex (Vertex vr), IsShaderData u t ) => 
                 ShaderResource -> RenderEntity vr (ShaderData u t) -> ResourceManager (RenderSet u)
requestRenderSet withProgram ent = 
    RenderSet  <$> ( requestVAO ( ent^.entMesh ) withProgram )
               <*> ( pure $ ent^.entData.shaderUniforms )
               <*> ( forM ( ent^.entData.shaderTextures.to fieldAssocs ) requestTextureItem)
               <*> ( pure $ fromIntegral . Mesh.vertexCount $ ent^.entMesh )
               <*> ( pure $ ent^.entSettings )



requestTextureItem :: (String, Texture) -> ResourceManager GLTextureItem
requestTextureItem (fieldName, texture) = do
    (_,_, texObj) <- requestTexture texture
    return $ case textureData texture of
        Texture2D _       -> mkTextureItem texObj GL.Texture2D
        TextureCube _     -> mkTextureItem texObj GL.TextureCubeMap
        TextureBuffer t _ -> mkTextureItem texObj t
    where
    mkTextureItem :: forall t. ( GL.BindableTextureTarget t, Show t ) => GL.TextureObject -> t -> GLTextureItem
    mkTextureItem texObj target =  GLTextureItem target $ TextureItem texObj fieldName
        

requestFramebufferSetup :: ( MultipleRenderTargets mrt, IsShaderData frameU frameT ) =>
                        PassDescr mrt (ShaderData frameU frameT) e v -> ResourceManager (FramebufferSetup frameU)
requestFramebufferSetup PassDescr{..} = 
    FramebufferSetup 
        <$> case passTarget of
            RenderTarget ident mrt -> requestFramebuffer (ident, mrt)
        <*> requestShader passShader
        <*> ( pure $ passPerFrameData^.shaderUniforms )
        <*> ( forM ( passPerFrameData^.shaderTextures.to fieldAssocs ) requestTextureItem )
        <*> pure passPreRendering
        <*> pure passPostRendering

---------------------------------------------------------------------------------------------------
initialGLRenderResources :: GLResources
initialGLRenderResources =
    GLResources mempty mempty mempty mempty mempty mempty


replaceIndices :: [Word32] -> IO ()
replaceIndices = GL.replaceBuffer GL.ElementArrayBuffer


