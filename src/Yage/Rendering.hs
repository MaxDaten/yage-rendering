{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
module Yage.Rendering (
      module GLReExports
    , runRenderer
    , renderScene
    , initialRenderState
    , logRenderM
    ) where

import             Yage.Prelude                    hiding (log)

import             Data.List                       (length, head, sum, map, lookup, groupBy, (++))

import             Control.Monad.RWS.Strict        (gets, modify, asks, tell, listen, runRWST)
import             Control.Monad                   (liftM, mapM)
import             Filesystem.Path.CurrentOS       (encodeString)

import             Graphics.GLUtil                 hiding (makeVAO)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
import             Yage.Rendering.Types
import             Yage.Rendering.Shader           ((.=))
import qualified   Yage.Rendering.Shader           as Shader
import             Yage.Rendering.Utils
import             Yage.Rendering.Logging
import             Yage.Resources
{-=================================================================================================-}


--initRenderStatistics :: RenderStatistics
--initRenderStatistics = RenderStatistics
--    { lastObjectCount       = 0
--    , lastTriangleCount     = 0
--    , lastRenderDuration    = 0.0
--    , loadedShadersCount    = 0
--    , loadedMeshesCount     = 0
--    }

initialRenderState :: RenderState
initialRenderState = RenderState 
    { loadedShaders         = []
    , loadedMeshes          = []
    , loadedDefinitions     = []
    --, renderStatistics      = initRenderStatistics
    }


renderScene :: RenderScene -> Renderer ()
renderScene scene = renderFrame scene >> afterFrame



afterFrame :: Renderer ()
afterFrame = io $ do
    return ()


renderFrame :: RenderScene -> Renderer ()
renderFrame scene = do
    beforeRender
    
    (objCount, renderTime) <- ioTime $ doRender scene

    shCount <- gets $! length . loadedShaders
    mshCount <- gets $! length . loadedMeshes
    --let stats = RenderStatistics
    --        { lastObjectCount    = objCount
    --        , lastRenderDuration = renderTime
    --        , lastTriangleCount  = sum $! map (triCount . model) $ entities scene
    --        , loadedShadersCount = shCount
    --        , loadedMeshesCount  = mshCount
    --        }

    afterRender


doRender :: RenderScene -> Renderer Int
doRender scene@RenderScene{..} =
    let batches = createShaderBatches scene entities
    in sum `liftM` mapM renderBatch batches


renderWithData :: RenderScene -> SomeRenderable -> Renderer ()
renderWithData scene r = requestRenderData r >>= \res -> render scene res r


renderBatch :: RenderBatch SomeRenderable -> Renderer Int
renderBatch RenderBatch{..} = preBatchAction batch >> length `liftM` mapM perItemAction batch


createShaderBatches :: RenderScene -> [SomeRenderable] -> [RenderBatch SomeRenderable]
createShaderBatches scene rs = 
    let shaderGroups = groupBy sameShader rs
    in map mkShaderBatch shaderGroups
    where
        sameShader :: SomeRenderable -> SomeRenderable -> Bool
        sameShader a b = shader a == shader b

        mkShaderBatch :: [SomeRenderable] -> RenderBatch SomeRenderable
        mkShaderBatch rs =
            let batchShader = shader . head $ rs
            in RenderBatch
                { preBatchAction = \_ -> do
                    shader <- requestShader batchShader 
                    io $! GL.currentProgram $= Just (program shader)
                    setSceneGlobals scene shader
                , perItemAction = renderWithData scene
                , batch = rs
                }



beforeRender :: Renderer ()
beforeRender = do
    setupFrame
    prepareResources


setupFrame :: Renderer ()
setupFrame = do
    clearC <- asks $ confClearColor . envConfig
    target <- asks renderTarget
    io $! do
        GL.clearColor $= fmap realToFrac clearC
        GL.depthFunc $= Just GL.Less    -- TODO to init
        GL.depthMask $= GL.Enabled      -- TODO to init
        
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]


        let (w, h) = target'size target
            r      = floor $ target'ratio target
        GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


-- | Unloads unneeded render-resources and loads needed resources
prepareResources :: Renderer ()
prepareResources = return ()

---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()
    --withWindow $ \win -> io . endDraw $ win
    --updateStatistics
            

---------------------------------------------------------------------------------------------------

render :: RenderScene -> RenderData -> SomeRenderable -> Renderer ()
render scene RenderData{..} r = do
    shadeItem shaderProgram scene r
    io $! withVAO vao $! drawIndexedTris . fromIntegral $ triangleCount


setSceneGlobals :: RenderScene -> ShaderProgram -> Renderer ()
setSceneGlobals scene sProg = shade sProg $! do
    Shader.sGlobalTime       .= sceneTime scene
    Shader.sProjectionMatrix .= projectionMatrix scene
    Shader.sViewMatrix       .= viewMatrix scene


shadeItem :: ShaderProgram -> RenderScene -> SomeRenderable -> Renderer ()
shadeItem sProg _scene r = shade sProg $! do
    let (modelM, normalM) = modelAndNormalMatrix $! r
    Shader.sModelMatrix      .= modelM
    Shader.sNormalMatrix     .= normalM
---------------------------------------------------------------------------------------------------


-- | runs the renderer in the given environment to render one frame.
-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> RenderState -> RenderEnv -> IO (a, RenderState, RenderLog)
runRenderer renderer state env = runRWST renderer env state

---------------------------------------------------------------------------------------------------

requestRenderData :: SomeRenderable -> Renderer RenderData
requestRenderData r = do
    sh  <- requestShader $ shader r
    vao <- requestVAO $ renderDefinition r
    return $ RenderData vao sh (triCount . model $ r)


requestRenderResource :: (Eq a, Show b)
                  => (RenderState -> [(a, b)])              -- ^ accassor function for state
                  -> (a -> Renderer b)                      -- ^ load function for resource
                  -> ((a,b) -> Renderer ())                 -- ^ function to add loaded resource to state
                  -> a                                      -- ^ the value to load resource from
                  -> Renderer b                             -- ^ the loaded resource
requestRenderResource accessor loadResource addResource a = do
    rs <- gets accessor
    r <- case lookup a rs of
        Just res ->
            return res
        Nothing  -> do
            loaded <- loadResource a
            logRenderMf "loaded resource: {0}" [show loaded]
            addResource (a, loaded)
            return $! loaded
    return r 


requestVAO :: RenderDefinition -> Renderer (VAO)
requestVAO = requestRenderResource loadedDefinitions loadDefinition addDefinition
    where
        loadDefinition :: RenderDefinition -> Renderer (VAO)
        loadDefinition RenderDefinition{..} = do
            (vbo, ebo) <- requestMesh def'mesh
            sProg      <- requestShader def'shader

            makeVAO $ do
                io $ GL.bindBuffer GL.ArrayBuffer        $= Just vbo
                io $ GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
                shade sProg $ do
                    Shader.enableAttrib Shader.sVertexPosition
                    Shader.enableAttrib Shader.sVertexNormal
                    Shader.enableAttrib Shader.sVertexColor


requestShader :: ShaderResource -> Renderer ShaderProgram
requestShader = requestRenderResource loadedShaders loadShaders addShader
    where
        loadShaders :: ShaderResource -> Renderer ShaderProgram
        loadShaders shader = do
            logRenderMf "loadShader: {0}" [show shader]
            sProg <- io $! simpleShaderProgram (encodeString $ vert shader) (encodeString $ frag shader)
            return $! sProg


requestMesh :: TriMesh -> Renderer (VBO, EBO)
requestMesh = requestRenderResource loadedMeshes loadMesh addMesh
    where
        loadMesh :: TriMesh -> Renderer (VBO, EBO)
        loadMesh mesh = io $ do
            vbo <- makeBuffer GL.ArrayBuffer $ vertices $ mesh
            ebo <- bufferIndices $ map fromIntegral $ indices mesh
            return $! (vbo, ebo)

---------------------------------------------------------------------------------------------------

addMesh :: (TriMesh, (VBO, EBO)) -> Renderer ()
addMesh m = modify $! \st -> st{ loadedMeshes = m:(loadedMeshes st) }


addShader :: (ShaderResource, ShaderProgram) -> Renderer ()
addShader s = modify $! \st -> st{ loadedShaders = s:(loadedShaders st) }


addDefinition :: (RenderDefinition, VAO) -> Renderer ()
addDefinition d = modify $ \st -> st{ loadedDefinitions = d:(loadedDefinitions st) }


--updateStatistics :: RenderStatistics -> Renderer ()
--updateStatistics stats = modify $ \st -> st{ renderStatistics = stats }

