{-# OPTIONS_GHC -fwarn-name-shadowing  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
module Yage.Rendering.RenderWorld
    ( module Yage.Rendering.RenderWorld
    , module Types
    ) where

import           Yage.Math
import           Yage.Prelude

import           Data.List

import           Control.Monad.RWS.Strict

import           Linear

import           Graphics.GLUtil                  hiding (loadShader, loadTexture)
import           Graphics.Rendering.OpenGL        (($=))
import qualified Graphics.Rendering.OpenGL        as GL

import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.RenderWorld.Types as Types

import           Yage.Rendering.Lenses
import qualified Yage.Rendering.Texture           as Tex
import           Yage.Rendering.Mesh              as Mesh
import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec


---------------------------------------------------------------------------------------------------



runRenderWorld :: RenderView -> RenderWorldEnv -> RenderResources -> IO ([ViewDefinition], RenderResources)
runRenderWorld rview env st =
    let theRun    = processRenderView rview
    in do
        (a,st',_) <- runRWST theRun env st
        return (a,st')


processRenderView :: RenderView -> RenderWorld [ViewDefinition]
-- process all entities, load render resources
-- generate list of currently contributing entities in view (intermediates)
-- send them to renderer
processRenderView renderview = do
    prepareResources
    res <- get
    ents <- findContributingEntities
    return $ map (toViewDefinition renderview res) ents



findContributingEntities :: RenderWorld [RenderEntity]
findContributingEntities = view worldEntities



toViewDefinition :: RenderView -> RenderResources -> RenderEntity -> ViewDefinition
toViewDefinition rview@RenderView{..} RenderResources{..} ent =
    let scaleM       = kronecker . point $ ent^.entityTransformation.transScale
        transM       = mkTransformation (ent^.entityTransformation.transOrientation) (ent^.entityTransformation.transPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in ViewDefinition
        { _vdMVPMatrix         = _rvProjectionMatrix !*! _rvViewMatrix !*! modelM
        , _vdModelViewMatrix   = _rvViewMatrix !*! modelM
        , _vdModelMatrix       = modelM
        , _vdNormalMatrix      = normalM
        , _vdRenderData        = getRenderData $ ent^.entityRenderDef
        , _vdUniformDef        = (ent^.entityRenderDef.rdefProgram._2, uniformEnv)
        }
    where
        getRenderData renderDef =
            let rData = renderDef^.rdefData
                rProg = renderDef^.rdefProgram^._1
                rTexs  = renderDef^.rdefTextures
            in RenderData
                { _vao           = _loadedVertexArrays^.at (rData, rProg) ^?!_Just
                , _shaderProgram = _loadedShaders^.at rProg ^?!_Just
                , _texObjs       = map makeTexObj rTexs
                , _elementCount  = meshTriangleCount rData
                , _drawMode      = renderDef^.rdefMode
                }
        makeTexObj tex =
            let obj = _loadedTextures^.at (tex^.texResource) ^?!_Just
                ch  = tex^.texChannel & _1 %~ fromIntegral
            in (obj, ch)
        uniformEnv = ShaderEnv
            { _seProgram           = _loadedShaders^.at (ent^.entityRenderDef^.rdefProgram._1) ^?!_Just
            , _seViewDef           = undefined -- TODO init is currently in renderer (awkward!!)
            , _seView              = rview
            }



prepareResources :: RenderWorld ()
prepareResources = view worldEntities >>= mapM_ (loadRenderResourcesFor . _entityRenderDef)



loadRenderResourcesFor :: RenderDefinition -> RenderWorld ()
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
        requestShader :: ShaderResource -> RenderWorld ()
        requestShader shRes = do
            res <- get
            unless (res^.loadedShaders.contains shRes) $ do
                shaderProg <- loadShader shRes
                loadedShaders.at shRes ?= shaderProg

        requestVertexBuffer :: Mesh -> RenderWorld ()
        requestVertexBuffer mesh@Mesh{_meshModToken, _meshData, _meshAttr} = do
            res <- get
            unless (res^.loadedVertexBuffer.contains mesh) $ do
                buff <- io $ makeVertexBufferF (_meshData^.to _meshAttr)
                ebo  <- io $ bufferIndices $ map fromIntegral $ _meshData^.mDataIndices
                loadedVertexBuffer.at mesh ?= (_meshModToken, buff, ebo)

        updateVertexBuffer :: Mesh -> RenderWorld ()
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

        requestVertexArray :: Mesh -> ShaderResource -> RenderWorld ()
        requestVertexArray mesh shRes = do
            res <- get
            unless (res^.loadedVertexArrays.contains (mesh, shRes)) $ do
                let shaderProg = res^.loadedShaders.at shRes ^?!_Just
                vao            <- loadVertexArray mesh shaderProg
                loadedVertexArrays.at (mesh, shRes) ?= vao

        requestTexture :: TextureResource -> RenderWorld ()
        requestTexture texture = do
            res <- get
            unless (res^.loadedTextures.contains texture) $ do
                texObj <- loadTexture texture
                loadedTextures.at texture ?= texObj


        -- | creates vbo and ebo, sets shader attributes and creates finally a vao
        loadVertexArray :: Mesh -> ShaderProgram -> RenderWorld VAO
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
        loadShader :: ShaderResource -> RenderWorld ShaderProgram
        loadShader res = io $!
            simpleShaderProgram (encodeString $ res^.srVertSrc) (encodeString $ res^.srFragSrc)

        -- | pushes texture to opengl
        loadTexture :: TextureResource -> RenderWorld GL.TextureObject
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
initialRenderWorldState :: RenderResources
initialRenderWorldState = mempty

replaceIndices :: [Word32] -> IO ()
replaceIndices = replaceBuffer GL.ElementArrayBuffer
