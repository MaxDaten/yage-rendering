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
import           Data.Map                                 as M hiding (map)
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
toViewDefinition RenderView{..} RenderWorldResources{..} RenderEntity{..} =
    let scaleM       = kronecker . point $ eScale
        transM       = mkTransformation eOrientation ePosition
        modelM       = transM !*! scaleM
        normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM))^?!_Just
    in ViewDefinition
        { _vdViewMatrix        = _rvViewMatrix
        , _vdProjectionMatrix  = _rvProjectionMatrix
        , _vdModelMatrix       = modelM
        , _vdNormalMatrix      = normalM
        , _vdRenderData        = getRenderData renderDef
        , _vdUniformDef        = snd . def'program $ renderDef
        }
    where
        getRenderData RenderDefinition{..} =
            RenderData
                { vao           = undefined -- fromJust $ loadedVertexBuffer^.at def'data
                , shaderProgram = _loadedShaders^.at (def'program^._1) ^?!_Just
                , texObjs       = map makeTexObj def'textures
                , triangleCount = 0
                }
        makeTexObj tex =
            let obj = (_loadedTextures^.at (tex^.texResource))^?!_Just
                ch  = tex^.texChannel & _1 %~ fromIntegral
            in (obj, ch)



prepareResources :: RenderWorld ()
prepareResources = view worldEntities >>= mapM_ (loadRenderResourcesFor . renderDef)



loadRenderResourcesFor :: RenderDefinition -> RenderWorld ()
loadRenderResourcesFor RenderDefinition{..} = do
    res <- use renderResources
    let shaderRes = def'program^._1

    unless (res^.loadedShaders.contains shaderRes) $ do
        shaderProg <- loadShader $ shaderRes
        renderResources.loadedShaders.at shaderRes ?= shaderProg


    unless (res^.loadedVertexBuffer.contains (def'data, shaderRes)) $ do
        let shaderProg = res^.loadedShaders.at shaderRes ^?!_Just
        vao            <- loadVertexBuffer def'data shaderProg
        renderResources.loadedVertexBuffer.at (def'data, shaderRes) ?= vao


    forM_ (def'textures^..traverse.texResource) $ \tex ->
        unless (res^.loadedTextures.contains tex) $ do
            texture <- loadTexture tex
            renderResources.loadedTextures.at tex ?= texture
    where
        -- | creates vbo and ebo, sets shader attributes and creates finally a vao
        loadVertexBuffer :: Mesh -> ShaderProgram -> RenderWorld VAO
        loadVertexBuffer Mesh{meshData, meshAttr} shaderProg = do
            buff <- io $ makeVertexBufferF meshAttr
            ebo <- io $ bufferIndices $ map fromIntegral $ indices meshData
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


