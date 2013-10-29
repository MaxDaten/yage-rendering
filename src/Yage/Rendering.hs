{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, TupleSections, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
module Yage.Rendering (
      module GLReExports
    , runRenderer
    , renderScene
    , initialRenderState
    , (!=), shaderEnv
    , logRenderM
    , version
    ) where
---------------------------------------------------------------------------------------------------
import             Yage.Prelude                    hiding (log)
import             Control.Lens                    hiding (indices)

import             Data.List                       (length, head, map, lookup, groupBy, (++))

import             Control.Monad.RWS.Strict        (gets, modify, asks, runRWST)
import             Control.Monad.Reader            (runReaderT, ask)
import             Control.Monad                   (mapM, mapM_,)
import             Filesystem.Path.CurrentOS       (encodeString)

import             Graphics.GLUtil                 hiding (makeVAO, offset0)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))
---------------------------------------------------------------------------------------------------
import             Yage.Rendering.Types
import qualified   Yage.Rendering.Texture          as Tex
import             Yage.Rendering.Shader
import             Yage.Rendering.VertexSpec
import             Yage.Rendering.Utils
import             Yage.Rendering.Logging
{-=================================================================================================-}

import Paths_yage_rendering


initialRenderState :: RenderState
initialRenderState = RenderState 
    { loadedShaders         = []
    , loadedMeshes          = []
    , loadedDefinitions     = []
    , loadedTextures        = []
    }


renderScene :: RenderScene -> Renderer ()
renderScene scene = renderFrame scene >> afterFrame



afterFrame :: Renderer ()
afterFrame = io $ do
    return ()


renderFrame :: RenderScene -> Renderer ()
renderFrame scene = do
    beforeRender
    
    (_, renderTime) <- ioTime $ doRender scene

    shCount         <- gets $! length . loadedShaders
    mshCount        <- gets $! length . loadedMeshes
    --let stats = RenderStatistics
    --        { lastObjectCount    = -1
    --        , lastRenderDuration = renderTime
    --        , lastTriangleCount  = -1 -- sum $! map (triCount . model) $ entities scene
    --        , loadedShadersCount = shCount
    --        , loadedMeshesCount  = mshCount
    --        }
    --logRenderM $ show stats

    afterRender


doRender :: RenderScene -> Renderer ()
doRender scene@RenderScene{..} =
    mapM_ renderBatch $ createShaderBatches scene entities


renderWithData :: RenderScene -> SomeRenderable -> Renderer ()
renderWithData scene r = requestRenderData r >>= \rdata -> render scene rdata r


renderBatch :: RenderBatch SomeRenderable -> Renderer ()
renderBatch RenderBatch{..} = withBatch $ \rs -> mapM_ perItemAction rs


createShaderBatches :: RenderScene -> [SomeRenderable] -> [RenderBatch SomeRenderable]
createShaderBatches scene rs = 
    let shaderGroups = groupBy sameShader rs
    in map mkShaderBatch shaderGroups
    where
        sameShader :: SomeRenderable -> SomeRenderable -> Bool
        sameShader a b = (programSrc $ renderProgram a) == (programSrc $ renderProgram b)

        mkShaderBatch :: [SomeRenderable] -> RenderBatch SomeRenderable
        mkShaderBatch rs =
            let batchShader = programSrc $ renderProgram . head $ rs
            in RenderBatch
                { withBatch = \m -> requestShader batchShader >>= \s -> withShader s (\_s -> m rs)
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
    io $! withVAO vao . withTexturesAt GL.Texture2D tUnits $! do
        let udefs = uniform'def . programDef  . renderProgram $ r
        runUniform (udefs >> mapTextureSamplers texObjs) shaderEnv
        drawIndexedTris . fromIntegral $ triangleCount
    logCountObj
    logCountTriangles triangleCount
    where
        tUnits = over (mapped._2) (^._1) texObjs
        shaderEnv = ShaderEnv 
            { shaderEnv'Program           = shaderProgram
            , shaderEnv'CurrentRenderable = r
            , shaderEnv'CurrentScene      = scene
            }

---------------------------------------------------------------------------------------------------


-- | runs the renderer in the given environment to render one frame.
-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> RenderState -> RenderEnv -> IO (a, RenderState, RenderLog)
runRenderer renderer state env = runRWST renderer env state

runUniform :: UniShader a -> ShaderEnv -> IO a
runUniform u env = runReaderT u env

---------------------------------------------------------------------------------------------------

requestRenderData :: SomeRenderable -> Renderer RenderData
requestRenderData r = do
    let rdef = renderDefinition r
        tris = triCount . def'data $ rdef
    sh       <- requestShader . programSrc . renderProgram $ r
    vao      <- requestVAO rdef
    texs     <- loadTexs (def'textures rdef)
    return $ RenderData vao sh texs tris
    where
        loadTexs :: [TextureDefinition] -> Renderer [(GL.TextureObject, (GLuint, String))]
        loadTexs = let chId td = fromIntegral $ td^._texChannel._1
                       chName td = td^._texChannel._2
                   in mapM $ \td -> (, (chId td, chName td)) <$> requestTexture (td^._texResource)


requestRenderResource :: (Eq a, Show b)
                  => (RenderState -> [(a, b)])              -- ^ accassor function for state
                  -> (a -> Renderer b)                      -- ^ load function for resource
                  -> ((a,b) -> Renderer ())                 -- ^ function to add loaded resource to state
                  -> a                                      -- ^ the value to load resource from
                  -> Renderer b                             -- ^ the loaded resource
requestRenderResource accessor loadResource addResource a = do
    rs <- gets accessor
    r  <- case lookup a rs of
        Just res ->
            return res
        Nothing  -> do
            loaded <- loadResource a
            logRenderMf "loaded resource: {0}" [show loaded]
            addResource (a, loaded)
            return $! loaded
    return r 


requestVAO :: RenderDefinition -> Renderer VAO
requestVAO = requestRenderResource loadedDefinitions loadDefinition addDefinition
    where
        loadDefinition :: RenderDefinition -> Renderer VAO
        loadDefinition RenderDefinition{..} = do
            (vbo, ebo) <- requestMesh     $ def'data
            sProg      <- requestShader   $ programSrc def'program
            let mapping = attrib'def      $ programDef def'program 
                vert    = head . vertices $ def'data
                defs    = define mapping vert

            io $ makeVAO $ do
                GL.bindBuffer GL.ArrayBuffer        $= Just vbo
                setVertexAttributes sProg defs
                GL.bindBuffer GL.ElementArrayBuffer $= Just ebo

setVertexAttributes :: ShaderProgram -> VertexDef -> IO ()
setVertexAttributes prog vdef = 
    let stride = vdef^._vertSize
    in vdef^._vertMap & mapM_ (setAttribute prog stride) 
    where
        setAttribute :: ShaderProgram -> VertexSize -> VertexAttribMapping -> IO ()
        setAttribute prog stride (name, layout) = do
            let vad = makeVAD layout stride
            setAttrib prog name GL.ToFloat vad
            enableAttrib prog name
        
        makeVAD :: VertexAttribDef -> VertexSize -> GL.VertexArrayDescriptor a
        makeVAD (off, count, _size) stride =
            let n = fromIntegral count
                t = GL.Float
                s = fromIntegral stride
                o = (offsetPtr off)
            in GL.VertexArrayDescriptor n t s o

requestShader :: ShaderResource -> Renderer ShaderProgram
requestShader = requestRenderResource loadedShaders loadShaders addShader
    where
        loadShaders :: ShaderResource -> Renderer ShaderProgram
        loadShaders shader = do
            logRenderMf "loadShader: {0}" [show shader]
            sProg <- io $! simpleShaderProgram (encodeString $ vert'src shader) (encodeString $ frag'src shader)
            return $! sProg


requestMesh :: Mesh Vertex4342 -> Renderer (VBO, EBO)
requestMesh = requestRenderResource loadedMeshes loadMesh addMesh
    where
        loadMesh :: Mesh Vertex4342 -> Renderer (VBO, EBO)
        loadMesh mesh = io $ do
            vbo <- makeBuffer GL.ArrayBuffer $ vertices $ mesh
            ebo <- bufferIndices $ map fromIntegral $ indices mesh
            return $! (vbo, ebo)


requestTexture :: TextureResource -> Renderer (GL.TextureObject)
requestTexture = requestRenderResource loadedTextures loadTexture' addTexture
    where
        loadTexture' :: TextureResource -> Renderer (GL.TextureObject)
        loadTexture' tex = io $ do
            res <- Tex.readTexture . encodeString $ tex
            GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') -- TODO Def
            printErrorMsg $ "tex: " ++ (show res )
            case res of
                Left msg  -> error msg
                Right obj -> return obj

---------------------------------------------------------------------------------------------------

addMesh :: (Mesh Vertex4342, (VBO, EBO)) -> Renderer ()
addMesh m = modify $! \st -> st{ loadedMeshes = m:(loadedMeshes st) }

addShader :: (ShaderResource, ShaderProgram) -> Renderer ()
addShader s = modify $! \st -> st{ loadedShaders = s:(loadedShaders st) }

addTexture :: (TextureResource, GL.TextureObject) -> Renderer ()
addTexture t = modify $! \st -> st{ loadedTextures = t:(loadedTextures st) }

addDefinition :: (RenderDefinition, VAO) -> Renderer ()
addDefinition d = modify $ \st -> st{ loadedDefinitions = d:(loadedDefinitions st) }

---------------------------------------------------------------------------------------------------

(!=) :: (AsUniform u) => String -> u -> UniShader ()
name != uni = do
    sp <- asks shaderEnv'Program
    io $ uni `asUniform` getUniform sp name

shaderEnv :: UniShader (ShaderEnv)
shaderEnv = ask

mapTextureSamplers :: [(GL.TextureObject, (GLuint, String))] -> UniShader ()
mapTextureSamplers texObjs = 
    let texUnitToUniform = texObjs^..traverse._2
    in do
        sp <- asks shaderEnv'Program
        io $ mapM_ (\(i, n) -> i `asUniform` getUniform sp n) texUnitToUniform
