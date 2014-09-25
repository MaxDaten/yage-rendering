{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}
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
{-# LANGUAGE TypeOperators         #-}

module Yage.Rendering.Resources.Manager where

import           Yage.Prelude
import           Yage.Lens

import           Foreign.Ptr                          (nullPtr)
import qualified Data.Vector.Storable                 as VS (Vector, map)

import           Control.Monad.RWS                    hiding (forM, forM_, mapM_, mapM)


import qualified Yage.Core.OpenGL                     as GL
import           Yage.Core.OpenGL                     (($=))


import           Yage.Rendering.Backend.Framebuffer
import           Yage.Rendering.Backend.RenderPass
import           Yage.Rendering.Backend.Renderer

import           Yage.Rendering.Resources.ResTypes
import           Yage.Rendering.Resources.Types



import           Yage.Rendering.Shader                as Shader
import qualified Yage.Rendering.Textures              as Tex
import           Yage.Rendering.Vertex
import           Yage.Rendering.Mesh                  as Mesh
import           Yage.Rendering.RenderEntity


import           Linear                               (V2 (..), _x, _y)

---------------------------------------------------------------------------------------------------




runResourceManager :: (MonadIO m) => GLResources -> ResourceManager a -> m (a, GLResources, [String])
runResourceManager res rm = io $ runRWST rm () res


requestResource :: (Ord key)
                => Lens' GLResources (Map key res)
                -- ^ state-field accessor
                -> (ResourceManager res)
                -- ^ loading function
                -> (res -> ResourceManager (UpdateTag res))
                -- ^ updating function old res to new res
                -> key
                -- ^ resource to load
                -> ResourceManager res
requestResource accessor loader updater resource = do
    resmap <- use accessor
    uitem <- maybe
        ( Dirty <$> loader  )
        ( updater )
        ( {-# SCC "requestResource.at" #-} resmap^.at resource )

    updateTag return updateItem uitem

    where
    updateItem item = {-# SCC "requestResource.updateItem" #-} do
        accessor.at resource ?= item
        return item


requestFramebuffer :: MultipleRenderTargets mrt =>
                   (TargetSlot, mrt) -> ResourceManager FramebufferRHI
requestFramebuffer (fboIdent, mrt)
    | isDefault mrt = return GL.defaultFramebufferObject
    | otherwise =
    let compiler = {-# SCC "compileFBO" #-} compileFBO <&> flip (^?!) folded -- unsafe head
    in requestResource compiledFBOs compiler ({-# SCC "compileFBO" #-} updateFramebuffer) fboIdent

    where

    -- | creates a new framebuffer according to the given specs
    compileFBO = do
        tell [ unpack $ format "create fbo {}" (Only $ Shown fboIdent ) ]
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

    -- toGLAttachment :: GLFBOAttachment -> (GL.FramebufferObjectAttachment, GLFBOAttachment)
    toGLAttachment (Attachment (ColorAttachment index) target) = (GL.ColorAttachment (fromIntegral index), target)
    toGLAttachment (Attachment DepthAttachment target)         = (GL.DepthAttachment, target)
    toGLAttachment (Attachment StencilAttachment target)       = (GL.StencilAttachment, target)
    toGLAttachment (Attachment DepthStencilAttachment target)  = (GL.DepthStencilAttachment, target)

    toBufferMode (Attachment (ColorAttachment idx) _) = GL.FBOColorAttachment . fromIntegral $ idx
    toBufferMode _ = error "Yage.Rendering.ResourceManager: invalid buffer-mode"


    -- updateFramebuffer :: Framebuffer TextureResource RenderbufferResource -> ResourceManager (Framebuffer TextureResource RenderbufferResource)
    updateFramebuffer f = do
        forM_  (allAttachments mrt) requestAttachment
        return $ Dirty f -- | TODO : Correct UpdateTag

    requestAttachment :: RenderTargets -> ResourceManager ()
    requestAttachment (Attachment _ (TextureTarget _ tex _))    = void $ requestTexture tex
    requestAttachment (Attachment _ (RenderbufferTarget rbuff)) = void $ requestRenderbuffer rbuff



requestTexture :: Texture -> ResourceManager TextureRHI
requestTexture texture@(Texture name newConfig texData) = do
    res <- requestResource loadedTextures newTexture updateTexture texture
    io $ withTextureBind texture $= Nothing
    return res
    where

    newTexture :: ResourceManager TextureRHI
    newTexture = do
        let chkErr = checkErrorOf ( unpack $ format "newTexture: {}" (Only $ Shown texture ) )
        obj <- io $ GL.genObjectName
        io $ withTextureBind texture $= Just obj

        chkErr $ loadData $ texture^.textureData
        setTextureConfig

        tell [ unpack $ format "newTexture: RHI = {}: {}" ( Shown texture, Shown obj ) ]
        return (texture^.textureSpec, texture^.textureConfig, obj)

    -- | pushes texture to opengl
    loadData :: TextureData -> ResourceManager ()
    loadData (Texture2D img) = io $ do
        Tex.uploadTextureImage' GL.Texture2D img

    loadData (TextureCube cubemap) = io $ do
        Tex.uploadCubeTextureImage cubemap

    loadData (TextureBuffer target spec) = io $ do
        let V2 w h      = spec^.Tex.texSpecDimension
            texSize     = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
            Tex.PixelSpec dataType pxfmt internalFormat = spec^.Tex.texSpecPixelSpec
            glPixelData = GL.PixelData pxfmt dataType nullPtr
        GL.textureLevelRange target $= (0,0)
        GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 glPixelData

    withTextureBind :: Texture -> GL.StateVar (Maybe GL.TextureObject)
    withTextureBind tex =
        case tex^.textureData of
            Texture2D _ -> GL.textureBinding GL.Texture2D
            TextureCube _ -> GL.textureBinding GL.TextureCubeMap
            TextureBuffer t _ -> GL.textureBinding t


    withTextureParameter :: Texture -> (forall t. GL.ParameterizedTextureTarget t => t -> a) -> a
    withTextureParameter tex varFun =
        case tex^.textureData of
            Texture2D _ -> varFun GL.Texture2D
            TextureCube _ -> varFun GL.TextureCubeMap
            TextureBuffer t _ -> varFun t


    updateTexture current = do
        io $ withTextureBind texture $= Just (current^._3)
        updateConfig =<< resizeTexture current


    updateConfig :: UpdateTag TextureRHI -> ResourceManager (UpdateTag TextureRHI)
    updateConfig = updateTag innerUpdate (fmap (join.Dirty) . innerUpdate)


    innerUpdate :: TextureRHI -> ResourceManager (UpdateTag TextureRHI)
    innerUpdate current@(_, currentConfig, _)
        | newConfig == currentConfig = return (Clean current)
        | otherwise = do
            setTextureConfig
            return $ tagDirty $ current & _2 .~ newConfig


    resizeTexture current@(currentSpec, _,_)
        | texture^.textureSpec == currentSpec = return $ Clean current
        | otherwise =
            let newSpec                                     = texture^.textureSpec
                V2 newWidth newHeight                       = fromIntegral <$> newSpec^.Tex.texSpecDimension
                texSize                                     = GL.TextureSize2D newWidth newHeight
                glPixelData                                 = GL.PixelData pxfmt dataType nullPtr
                Tex.PixelSpec dataType pxfmt internalFormat = newSpec^.Tex.texSpecPixelSpec
            in do
                tell [ unpack $ format "resizeTexture: {}\nfrom:\t{}\nto:\t{}" ( Shown name, Shown currentSpec, Shown newSpec ) ]

                checkErrorOf ( unpack $ format "resizeTexture: {}" (Only $ Shown texture ) ) $ io $
                    case texData of
                        TextureBuffer target _ -> GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 glPixelData
                        _                      -> error "Manager.resizeTexture: invalid texture resize. Only TextureBuffer resizing currently supported!"
                return $ tagDirty $ current & _1 .~ newSpec


    setTextureConfig :: ResourceManager ()
    setTextureConfig = checkErrorOf ( unpack $ format "setTextureConfig {} {}" ( Shown name, Shown newConfig ) ) $ io $ do
        let TextureFiltering minification mipmap magnification = newConfig^.texConfFiltering
            TextureWrapping repetition clamping = newConfig^.texConfWrapping

        when (isJust mipmap) $ autoGenerateMipMap texData

        -- NOTE: filtering is neccessary for texture completeness
        withTextureParameter texture GL.textureFilter $= ((minification, mipmap), magnification)
        case texData of
            Texture2D _ -> do
                GL.texture2DWrap $= (repetition, clamping)
            TextureBuffer _ _ -> do
                GL.texture2DWrap $= (repetition, clamping)

            TextureCube _ -> do
                GL.texture3DWrap GL.TextureCubeMap $= (repetition, clamping)


    autoGenerateMipMap :: TextureData -> IO ()
    autoGenerateMipMap = \case
        Texture2D   _     -> GL.generateMipmap' GL.Texture2D
        TextureCube _     -> GL.generateMipmap' GL.TextureCubeMap
        TextureBuffer t _ -> GL.generateMipmap' t



requestRenderbuffer :: Renderbuffer -> ResourceManager RenderbufferRHI
requestRenderbuffer buff@(Renderbuffer _ newSpec@(Tex.TextureImageSpec sz pxSpec)) =
    requestResource loadedRenderbuffers loadRenderbuffer resizeBuffer buff
    where

    loadRenderbuffer =
        let size           = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
            internalFormat = Tex.pxSpecGLFormat pxSpec
        in do
            tell [ unpack $ format "loadRenderbuffer: {}" ( Only $ Shown buff ) ]
            checkErrorOf "loadRenderbuffer: " $ io $ do
                rObj <- GL.genObjectName
                GL.bindRenderbuffer GL.Renderbuffer $= rObj
                GL.renderbufferStorage GL.Renderbuffer internalFormat size
                GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (newSpec, rObj)

    resizeBuffer current@(currentSpec, rObj)
        | newSpec == currentSpec = return $ return current
        | otherwise =
            let size            = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
                internalFormat  = Tex.pxSpecGLFormat pxSpec
            in do
                tell [ unpack $ format "resizeBuffer {}" ( Only $ Shown buff ) ]
                checkErrorOf "resizeBuffer: " $ io $ do
                    GL.bindRenderbuffer GL.Renderbuffer $= rObj
                    GL.renderbufferStorage GL.Renderbuffer internalFormat size
                    GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return $ tagDirty (newSpec, rObj)



requestShader :: ShaderProgramUnit -> ResourceManager ShaderRHI
requestShader shader = requestResource loadedShaders loadShader (return.tagClean) shader where

    loadShader :: ResourceManager ShaderRHI
    loadShader = do -- _shaderSrcRaw.to unRaw
        prg <- io $! Shader.loadShaderProgram ( shader^.shaderSources^..traverse.compilationUnit )
                `catch`
                (\(e::IOError) -> ioError (userError $ (shader^.shaderName) ++ ": " ++ show e))

        -- warning missing Link error
        --printTF "shader: {}\n{}\n{}\n\n"
        --        ( Shown $ shader^.shaderName
        --        , Shown $ shader^.shaderSources^..traverse.compilationUnit
        --        , Shown $ prg
        --        )
        return prg


requestVertexbuffer :: (Storable (Vertex vr)) => Mesh (Vertex vr) -> ResourceManager (BufferedVertices vr)
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
        io $ bufferVertices (mesh^.meshVertexBuffer)

    updateVertexBuffer (oldHash, buff) = do
        let vBuff = BufferedVertices buff
        when (mesh^.meshHash /= oldHash) $ io $ reloadVertices vBuff (mesh^.meshVertexBuffer)
        return vBuff


-- | request a ElementBuffer for all `MeshComponent`s in a Mesh
requestElementBuffer :: Mesh v -> ResourceManager EBO
requestElementBuffer mesh = do
    eMap <- use loadedIndexBuffers

    ebo <- maybe
            (loadIndexBuffer)
            (updateIndexBuffer)
            (eMap^.at (mesh^.meshId))

    loadedIndexBuffers.at (mesh^.meshId) ?= (mesh^.meshComponentsHash, ebo)
    return ebo
    where

    -- flatten all indices
    loadIndexBuffer = io $ GL.bufferIndices (VS.map fromIntegral $ mesh^.concatedMeshIndices)

    updateIndexBuffer (oldHash, ebo)
        | mesh^.meshComponentsHash == oldHash = return ebo
        | otherwise = io $ do
            GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
            GL.replaceVector GL.ElementArrayBuffer $ (VS.map fromIntegral $ mesh^.concatedMeshIndices :: VS.Vector Word32)
            GL.bindBuffer GL.ElementArrayBuffer $= Nothing
            return ebo


requestVAO :: (ViableVertex (Vertex vr)) => Mesh (Vertex vr) -> ShaderProgramUnit -> ResourceManager VertexArrayRHI
requestVAO mesh shader = requestResource loadedVertexArrays loadVertexArray updateBindings (mesh^.meshId,shader)
    where

    updateBindings vao = do
        _vbuff           <- requestVertexbuffer mesh
        _shaderProg      <- requestShader shader
        _ebo             <- requestElementBuffer mesh
        return $ Clean vao

    loadVertexArray = do
        vbuff           <- requestVertexbuffer mesh
        shaderProg      <- requestShader shader
        ebo             <- requestElementBuffer mesh

        tell [ unpack $ format "RenderSet: {} - {}" ( Shown $ mesh^.meshId, Shown shader ) ]
        io $ GL.makeVAO $ do
            bindVertices vbuff
            enableVertices' shaderProg vbuff
            -- it is really part of vao:
            --   - http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            -- The index buffer binding is stored within the VAO. If no VAO is bound, then you cannot bind a buffer object to GL_ELEMENT_ARRAY_BUFFER​.
            --   - http://www.opengl.org/wiki/Vertex_Specification#Vertex_Array_Object
            GL.bindBuffer GL.ElementArrayBuffer $= Just ebo



requestRenderSet :: ( ViableVertex (Vertex vr), IsShaderData u t ) =>
                    ShaderProgramUnit
                 -> RenderEntity (Vertex vr) (ShaderData u t)
                 -> ResourceManager ( RenderSet (Uniforms u) )
requestRenderSet withProgram ent =
    RenderSet  <$> ( requestVAO ( ent^.entMesh ) withProgram )
               <*> ( pure $ ent^.entData.shaderUniforms )
               <*> ( mapM requestTextureItem $ textureFields $ ent^.entData.shaderTextures )
               <*> ( pure $ fromIntegral $ ent^.entMesh.vertexCount )
               <*> ( pure $ ent^.entMesh.meshIndexRanges )
               <*> ( pure $ ent^.entSettings )



requestTextureItem :: HasTexture t => (String, t) -> ResourceManager GLTextureItem
requestTextureItem (fieldName, t) = do
    let texture = getTexture t
    (_,_, texObj) <- requestTexture texture
    return $ case texture^.textureData of
        Texture2D _       -> GLTextureItem GL.Texture2D $ TextureItem texObj fieldName
        TextureCube _     -> GLTextureItem GL.TextureCubeMap $ TextureItem texObj fieldName
        TextureBuffer t _ -> GLTextureItem t $ TextureItem texObj fieldName


requestFramebufferSetup :: ( MultipleRenderTargets mrt, IsShaderData frameU frameT, u <: frameU, t <: frameT ) =>
                           PassDescr mrt (Shader u t v)
                        -> ShaderData frameU frameT
                        -> ResourceManager ( FramebufferSetup (Uniforms frameU) )
requestFramebufferSetup pass frameData =
    FramebufferSetup
        <$> case pass^.passTarget of
            RenderTarget ident mrt -> {-# SCC "rq.fb" #-} requestFramebuffer (ident, mrt)
        <*> ( requestShader (pass^.passShader.shaderProgram) )
        <*> ( pure $ frameData^.shaderUniforms )
        <*> ( mapM requestTextureItem $ textureFields $ frameData^.shaderTextures )
        <*> ( pure $ pass^.passPreRendering )
        <*> ( pure $ pass^.passPostRendering )

---------------------------------------------------------------------------------------------------
initialGLRenderResources :: GLResources
initialGLRenderResources =
    GLResources mempty mempty mempty mempty mempty mempty mempty


replaceIndices :: [Word32] -> IO ()
replaceIndices = GL.replaceBuffer GL.ElementArrayBuffer


