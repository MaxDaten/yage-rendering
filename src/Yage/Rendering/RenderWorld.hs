{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
module Yage.Rendering.RenderWorld
    ( module Yage.Rendering.RenderWorld
    , module RenderWorldTypes
    ) where

import           Yage.Math
import           Yage.Prelude

import           Data.List
import           Data.Map.Strict                          as M hiding (map)
import           Data.Maybe                               (fromJust)
import           Data.Set                                 as S hiding (map)

import           Control.Lens                             hiding (indices)
import           Control.Monad.RWS.Strict
import           Control.Monad.State

import           Linear

import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL                (($=))
import qualified Graphics.Rendering.OpenGL                as GL

import           Yage.Rendering.Backend.Renderer
import           Yage.Rendering.Internal.RenderWorldTypes as RenderWorldTypes
import qualified Yage.Rendering.Texture                   as Tex
import           Yage.Rendering.Types
import           Yage.Rendering.VertexSpec


---------------------------------------------------------------------------------------------------



runRenderWorld :: RenderView -> RenderWorldEnv -> RenderWorldState -> IO ([ViewDefinition], RenderWorldState)
runRenderWorld view env st =
    let theRun    = processRenderView view
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
toViewDefinition view@RenderView{..} RenderWorldResources{..} ent@RenderEntity{..} =
    let scaleM       = kronecker . point $ eScale
        transM       = mkTransformation eOrientation ePosition
        modelM       = transM !*! scaleM
        -- TODO rethink the normal matrix here
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) <|> Just eye3) ^?!_Just
    in ViewDefinition
        { _vdMVPMatrix         = _rvProjectionMatrix !*! _rvViewMatrix !*! modelM
        , _vdModelMatrix       = modelM
        , _vdNormalMatrix      = normalM
        , _vdRenderData        = getRenderData renderDef
        , _vdUniformDef        = (snd . def'program $ renderDef, uniformEnv)
        }
    where
        getRenderData RenderDefinition{..} =
            RenderData
                { vao           = _loadedVertexBuffer^.at (def'data, def'program^._1) ^?!_Just
                , shaderProgram = _loadedShaders^.at (def'program^._1) ^?!_Just
                , texObjs       = map makeTexObj def'textures
                , triangleCount = meshTriangleCount def'data
                }
        makeTexObj tex =
            let obj = _loadedTextures^.at (tex^.texResource) ^?!_Just
                ch  = tex^.texChannel & _1 %~ fromIntegral
            in (obj, ch)
        uniformEnv = ShaderEnv
            { shaderEnv'Program           = _loadedShaders^.at ((def'program renderDef)^._1) ^?!_Just
            , shaderEnv'CurrentRenderable = undefined -- TODO init is currently in renderer (awkward!!)
            , shaderEnv'CurrentScene      = view
            }



prepareResources :: RenderWorld ()
prepareResources = view worldEntities >>= mapM_ (loadRenderResourcesFor . renderDef)



loadRenderResourcesFor :: RenderDefinition -> RenderWorld ()
loadRenderResourcesFor RenderDefinition{..} = do
    res <- use renderResources
    let shaderRes = def'program^._1

    -- Shader on demand loading
    requestShader shaderRes

    -- VertexBuffer on demand with shader prog for vertex attributes    
    requestVertexBuffer def'data shaderRes

    -- TextureObjects on demand
    forM_ (def'textures^..traverse.texResource) $ requestTexture
    where
        requestShader :: ShaderResource -> RenderWorld ()
        requestShader shaderRes = do
            res <- use renderResources
            unless (res^.loadedShaders.contains shaderRes) $ do
                shaderProg <- loadShader $ shaderRes
                renderResources.loadedShaders.at shaderRes ?= traceShow' shaderProg

        requestVertexBuffer :: Mesh -> ShaderResource -> RenderWorld ()
        requestVertexBuffer def'data shaderRes = do
            res <- use renderResources
            unless (res^.loadedVertexBuffer.contains (def'data, shaderRes)) $ do
                let shaderProg = res^.loadedShaders.at shaderRes ^?!_Just
                io $ print "load VAO"
                vao            <- loadVertexBuffer def'data shaderProg
                renderResources.loadedVertexBuffer.at (def'data, shaderRes) ?= vao

        requestTexture :: TextureResource -> RenderWorld ()
        requestTexture texture = do
            res <- use renderResources
            unless (res^.loadedTextures.contains texture) $ do
                texObj <- loadTexture texture
                renderResources.loadedTextures.at texture ?= texObj

        -- | creates vbo and ebo, sets shader attributes and creates finally a vao
        loadVertexBuffer :: Mesh -> ShaderProgram -> RenderWorld VAO
        loadVertexBuffer Mesh{meshData, meshAttr} shaderProg = do
            buff <- io $ makeVertexBufferF meshAttr
            ebo  <- io $ bufferIndices $ map fromIntegral $ indices meshData
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
            simpleShaderProgram (encodeString $ vert'src res) (encodeString $ frag'src res)

        -- | pushes texture to opengl
        loadTexture :: TextureResource -> RenderWorld GL.TextureObject
        loadTexture (TextureImage _ img) = io $
            handleTexObj =<< Tex.readTextureImg img

        loadTexture (TextureFile texFile) = io $
            handleTexObj =<< (Tex.readTexture . encodeString $ texFile)

        handleTexObj res = do
            GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
            printErrorMsg $ "tex: " ++ (show res )
            case res of
                Left msg  -> error msg
                Right obj -> return obj


