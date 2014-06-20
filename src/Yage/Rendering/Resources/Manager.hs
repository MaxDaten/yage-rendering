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

module Yage.Rendering.Resources.Manager where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Geometry.Vertex

import           Foreign.Ptr                          (nullPtr)
import qualified Data.Vector.Storable                 as VS (Vector, map)

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
                => Lens' GLResources (Map key res)
                -- ^ state-field accessor
                -> (ResourceManager res)
                -- ^ loading function
                -> (res -> ResourceManager res)
                -- ^ updating function old res to new res
                -> key
                -- ^ resource to load
                -> ResourceManager res
requestResource accessor loader updater resource = do
    resmap <- use accessor
    item <- maybe
        ( loader  )
        ( updater )
        ( resmap^.at resource )

    accessor.at resource ?= item
    return $ item


requestFramebuffer :: (Show ident, MultipleRenderTargets mrt) => 
                   (ident, mrt) -> ResourceManager FramebufferRHI
requestFramebuffer (fboIdent, mrt) 
    | isDefault mrt = return GL.defaultFramebufferObject
    | otherwise =
    let compiler = compileFBO <&> flip (^?!) folded -- unsafe head
    in requestResource compiledFBOs compiler updateFramebuffer (show fboIdent)

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

        chkErr $ loadData $ textureData texture
        setTextureConfig
        
        tell [ unpack $ format "newTexture: RHI = {}: {}" ( Shown texture, Shown obj ) ]
        return (textureSpec texture, textureConfig texture, obj)

    -- | pushes texture to opengl
    loadData :: TextureData -> ResourceManager ()
    loadData (Texture2D img) = io $ do
        Tex.loadTextureImage' GL.Texture2D img

    loadData (TextureCube cubemap) = io $ do
        Tex.loadCubeTextureImage cubemap

    loadData (TextureBuffer target spec) = io $ do
        let V2 w h      = Tex.texSpecDimension spec
            texSize     = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
            Tex.PixelSpec dataType pxfmt internalFormat = Tex.texSpecPixelSpec spec
            glPixelData = GL.PixelData pxfmt dataType nullPtr    
        GL.textureLevelRange target $= (0,0)
        GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 glPixelData

    withTextureBind :: Texture -> GL.StateVar (Maybe GL.TextureObject)
    withTextureBind tex =
        case textureData tex of
            Texture2D _ -> GL.textureBinding GL.Texture2D
            TextureCube _ -> GL.textureBinding GL.TextureCubeMap
            TextureBuffer t _ -> GL.textureBinding t


    withTextureParameter :: Texture -> (forall t. GL.ParameterizedTextureTarget t => t -> a) -> a
    withTextureParameter tex varFun =
        case textureData tex of
            Texture2D _ -> varFun GL.Texture2D
            TextureCube _ -> varFun GL.TextureCubeMap
            TextureBuffer t _ -> varFun t


    updateTexture current = do
        io $ withTextureBind texture $= Just (current^._3)
        updateConfig =<< resizeTexture current


    updateConfig :: TextureRHI -> ResourceManager TextureRHI
    updateConfig current@(_, currentConfig, _)
        | newConfig == currentConfig = return current
        | otherwise = do
            setTextureConfig
            return $ current & _2 .~ newConfig
    

    resizeTexture current@(currentSpec, _,_) 
        | textureSpec texture == currentSpec = return current
        | otherwise =
            let newSpec                                     = textureSpec texture
                V2 newWidth newHeight                       = fromIntegral <$> Tex.texSpecDimension newSpec
                texSize                                     = GL.TextureSize2D newWidth newHeight
                glPixelData                                 = GL.PixelData pxfmt dataType nullPtr
                Tex.PixelSpec dataType pxfmt internalFormat = Tex.texSpecPixelSpec newSpec
            in do
                tell [ unpack $ format "resizeTexture: {}\nfrom:\t{}\nto:\t{}" ( Shown name, Shown currentSpec, Shown newSpec ) ]
                
                checkErrorOf ( unpack $ format "resizeTexture: {}" (Only $ Shown texture ) ) $ io $
                    case texData of
                        TextureBuffer target _ -> GL.texImage2D target GL.NoProxy 0 internalFormat texSize 0 glPixelData
                        _                      -> error "Manager.resizeTexture: invalid texture resize. Only TextureBuffer resizing currently supported!"
                return $ current & _1 .~ newSpec


    setTextureConfig :: ResourceManager ()
    setTextureConfig = checkErrorOf ( unpack $ format "setTextureConfig {} {}" ( Shown name, Shown newConfig ) ) $ io $ do
        let TextureFiltering minification mipmap magnification = newConfig^.texConfFiltering
            TextureWrapping repetition clamping = newConfig^.texConfWrapping
            
        -- NOTE: filtering is neccessary for texture completeness
        when (isJust mipmap) $ GL.generateMipmap' texData
        withTextureParameter texture GL.textureFilter $= ((minification, mipmap), magnification)
        case texData of
            Texture2D _ -> do
                GL.texture2DWrap $= (repetition, clamping)
            
            TextureCube _ -> do 
                GL.texture3DWrap GL.TextureCubeMap $= (repetition, clamping)
            
            TextureBuffer _ _ -> do                
                GL.texture2DWrap $= (repetition, clamping)


requestRenderbuffer :: Renderbuffer -> ResourceManager RenderbufferRHI
requestRenderbuffer buff@(Renderbuffer _ newSpec@(Tex.TextureImageSpec sz pxSpec)) = 
    requestResource loadedRenderbuffers loadRenderbuffer resizeBuffer buff
    where

    loadRenderbuffer =
        let size           = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
            internalFormat = Tex.pxSpecGLFormat pxSpec
        in do
            tell [ unpack $ format "loadRenderbuffer: {}" ( Only $ Shown buff ) ]
            checkErrorOf ("loadRenderbuffer: ") $ io $ do
                rObj <- GL.genObjectName
                GL.bindRenderbuffer GL.Renderbuffer $= rObj
                GL.renderbufferStorage GL.Renderbuffer internalFormat size
                GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (newSpec, rObj)

    resizeBuffer current@(currentSpec, rObj) 
        | newSpec == currentSpec = return current
        | otherwise =
            let size            = GL.RenderbufferSize (fromIntegral $ sz^._x) (fromIntegral $ sz^._y)
                internalFormat  = Tex.pxSpecGLFormat pxSpec
            in do 
                tell [ unpack $ format "resizeBuffer {}" ( Only $ Shown buff ) ]
                checkErrorOf ("resizeBuffer: ") $ io $ do
                    GL.bindRenderbuffer GL.Renderbuffer $= rObj
                    GL.renderbufferStorage GL.Renderbuffer internalFormat size
                    GL.bindRenderbuffer GL.Renderbuffer $= GL.noRenderbufferObject
                return (newSpec, rObj)



requestShader :: ShaderResource -> ResourceManager ShaderRHI
requestShader shader = requestResource loadedShaders loadShader return shader
    where
    loadShader :: ResourceManager ShaderRHI
    loadShader = io $!
        GL.simpleShaderProgram (encodeString $ shader^.srVertSrc) (encodeString $ shader^.srFragSrc)



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


requestVAO :: (ViableVertex (Vertex vr)) => Mesh (Vertex vr) -> ShaderResource -> ResourceManager VertexArrayRHI
requestVAO mesh shader = requestResource loadedVertexArrays loadVertexArray return (mesh^.meshId,shader) 
    where
    loadVertexArray = do
        vbuff           <- requestVertexbuffer mesh
        shaderProg      <- requestShader shader
        ebo             <- requestElementBuffer mesh

        tell [ unpack $ format "RenderSet: {} - {}" ( Shown "mesh", Shown shader ) ] 
        io $ GL.makeVAO $ do
            bindVertices vbuff
            enableVertices' shaderProg vbuff
            -- it is really part of vao:
            --   - http://stackoverflow.com/questions/8973690/vao-and-element-array-buffer-state
            -- The index buffer binding is stored within the VAO. If no VAO is bound, then you cannot bind a buffer object to GL_ELEMENT_ARRAY_BUFFER​.
            --   - http://www.opengl.org/wiki/Vertex_Specification#Vertex_Array_Object
            GL.bindBuffer GL.ElementArrayBuffer $= Just ebo



requestRenderSet :: ( ViableVertex (Vertex vr), IsShaderData u t ) => 
                 ShaderResource -> RenderEntity (Vertex vr) (ShaderData u t) -> ResourceManager (RenderSet u)
requestRenderSet withProgram ent = 
    RenderSet  <$> ( requestVAO ( ent^.entMesh ) withProgram )
               <*> ( pure $ ent^.entData.shaderUniforms )
               <*> ( forM ( ent^.entData.shaderTextures.to fieldAssocs ) requestTextureItem )
               <*> ( pure $ fromIntegral $ ent^.entMesh.vertexCount )
               <*> ( pure $ ent^.entMesh.meshIndexRanges )
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
    GLResources mempty mempty mempty mempty mempty mempty mempty


replaceIndices :: [Word32] -> IO ()
replaceIndices = GL.replaceBuffer GL.ElementArrayBuffer


