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

import           Control.Lens                     hiding (indices)
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



runRenderWorld :: RenderView -> RenderWorldEnv -> RenderWorldState -> IO ([ViewDefinition], RenderWorldState)
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
    res <- use renderResources
    ents <- findContributingEntities
    return $ map (toViewDefinition renderview res) ents



findContributingEntities :: RenderWorld [RenderEntity]
findContributingEntities = view worldEntities



toViewDefinition :: RenderView -> RenderWorldResources -> RenderEntity -> ViewDefinition
toViewDefinition rview@RenderView{..} RenderWorldResources{..} ent =
    let scaleM       = kronecker . point $ ent^.entityScale
        transM       = mkTransformation (ent^.entityOrientation) (ent^.entityPosition)
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in ViewDefinition
        { _vdMVPMatrix         = _rvProjectionMatrix !*! _rvViewMatrix !*! modelM
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
                { vao           = _loadedVertexBuffer^.at (rData, rProg) ^?!_Just
                , shaderProgram = _loadedShaders^.at rProg ^?!_Just
                , texObjs       = map makeTexObj rTexs
                , triangleCount = meshTriangleCount rData
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
    let shaderRes = rdef^.rdefProgram._1

    -- Shader on demand loading
    requestShader shaderRes

    -- VertexBuffer on demand with shader prog for vertex attributes
    requestVertexBuffer (rdef^.rdefData) shaderRes

    -- TextureObjects on demand
    forM_ (rdef^.rdefTextures^..traverse.texResource) requestTexture
    where
        requestShader :: ShaderResource -> RenderWorld ()
        requestShader shaderRes = do
            res <- use renderResources
            unless (res^.loadedShaders.contains shaderRes) $ do
                shaderProg <- loadShader shaderRes
                renderResources.loadedShaders.at shaderRes ?= traceShow' shaderProg

        requestVertexBuffer :: Mesh -> ShaderResource -> RenderWorld ()
        requestVertexBuffer mesh shaderRes = do
            res <- use renderResources
            unless (res^.loadedVertexBuffer.contains (mesh, shaderRes)) $ do
                let shaderProg = res^.loadedShaders.at shaderRes ^?!_Just
                vao            <- loadVertexBuffer mesh shaderProg
                renderResources.loadedVertexBuffer.at (mesh, shaderRes) ?= vao

        requestTexture :: TextureResource -> RenderWorld ()
        requestTexture texture = do
            res <- use renderResources
            unless (res^.loadedTextures.contains texture) $ do
                texObj <- loadTexture texture
                renderResources.loadedTextures.at texture ?= texObj

        -- | creates vbo and ebo, sets shader attributes and creates finally a vao
        loadVertexBuffer :: Mesh -> ShaderProgram -> RenderWorld VAO
        loadVertexBuffer Mesh{_meshData, _meshAttr} shaderProg = do
            buff <- io $ makeVertexBufferF _meshAttr
            ebo  <- io $ bufferIndices $ map fromIntegral $ _meshData^.mDataIndices
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
initialRenderWorldState :: RenderWorldState
initialRenderWorldState = RenderWorldState mempty
