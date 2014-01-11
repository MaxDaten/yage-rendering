{-# OPTIONS_GHC -fwarn-name-shadowing  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
module Yage.Rendering.ResourceManager
    ( module Yage.Rendering.ResourceManager
    , module Types
    ) where

import           Yage.Prelude

import           Data.List

import           Control.Monad.RWS.Strict

import           Graphics.GLUtil                  hiding (loadShader, loadTexture)
import           Graphics.Rendering.OpenGL        (($=))
import qualified Graphics.Rendering.OpenGL        as GL

import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.ResourceManager.Types as Types

import           Yage.Rendering.Lenses
import qualified Yage.Rendering.Texture           as Tex
import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec


---------------------------------------------------------------------------------------------------



runResourceManager :: (MonadIO m) => RenderView -> Resourceables -> RenderResources -> m RenderResources
runResourceManager rview env st = do
    ((),st',_) <- io $ runRWST prepareResources env st
    return st'


prepareResources :: ResourceManager ()
prepareResources = view entities >>= mapM_ (loadRenderResourcesFor . _entityRenderDef)



loadRenderResourcesFor :: RenderDefinition -> ResourceManager ()
loadRenderResourcesFor rdef = do
    let shRes = rdef^.rdefProgram.shaderRes

    -- Shader on demand loading
    requestShader shRes

    -- VertexBuffer on demand with shader prog for vertex attributes
    requestVertexBuffer (rdef^.rdefData)
    updateVertexBuffer (rdef^.rdefData)
    requestVertexArray (rdef^.rdefData) shRes

    -- TextureObjects on demand
    forM_ (rdef^.rdefTextures^..traverse.texResource) requestTexture
    where
        requestShader :: ShaderResource -> ResourceManager ()
        requestShader shRes = do
            res <- get
            unless (res^.loadedShaders.contains shRes) $ do
                shaderProg <- loadShader shRes
                loadedShaders.at shRes ?= shaderProg

        requestVertexBuffer :: Mesh -> ResourceManager ()
        requestVertexBuffer mesh@Mesh{_meshModToken, _meshData, _meshAttr} = do
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

        requestVertexArray :: Mesh -> ShaderResource -> ResourceManager ()
        requestVertexArray mesh shRes = do
            res <- get
            unless (res^.loadedVertexArrays.contains (mesh, shRes)) $ do
                let shaderProg = res^.loadedShaders.at shRes ^?!_Just
                vao            <- loadVertexArray mesh shaderProg
                loadedVertexArrays.at (mesh, shRes) ?= vao

        requestTexture :: TextureResource -> ResourceManager ()
        requestTexture texture = do
            res <- get
            unless (res^.loadedTextures.contains texture) $ do
                texObj <- loadTexture texture
                loadedTextures.at texture ?= texObj


        -- | creates vbo and ebo, sets shader attributes and creates finally a vao
        loadVertexArray :: Mesh -> ShaderProgram -> ResourceManager VAO
        loadVertexArray mesh shaderProg = do
            res <- get
            let (_, buff, ebo) = res^.loadedVertexBuffer.at mesh ^?!_Just
            io $ makeVAO $ do
                GL.bindBuffer GL.ArrayBuffer        $= Just (vbo buff)
                mapM_ (setVertexAttribute shaderProg) (attribVADs buff)
                GL.bindBuffer GL.ElementArrayBuffer $= Just ebo

        setVertexAttribute prog (VertexDescriptor name vad) = do
            enableAttrib prog name
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

        handleTexObj res = do
            GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
            printErrorMsg $ "tex: " ++ show res
            case res of
                Left msg  -> error msg
                Right obj -> return obj

---------------------------------------------------------------------------------------------------
initialRenderResources :: RenderResources
initialRenderResources = mempty

replaceIndices :: [Word32] -> IO ()
replaceIndices = replaceBuffer GL.ElementArrayBuffer
