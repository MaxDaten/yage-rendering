{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Yage.Rendering.Backend.Renderer (
      module Types
    , runRenderer, renderFrame
    , (!=), shaderEnv
    , logRenderM
    , version
    ) where
---------------------------------------------------------------------------------------------------
import           Yage.Prelude                            hiding (log)

import           Foreign.Ptr                             (nullPtr)
import           Data.List                               (groupBy, map)

import           Control.Monad                           (forM_, mapM_, when)
import           Control.Monad.Reader                    (ask, runReaderT)
import           Control.Monad.RWS.Strict                (runRWST)

import           Graphics.GLUtil                         hiding (makeVAO,
                                                          offset0)
import qualified Graphics.Rendering.OpenGL               as GL
import           Graphics.Rendering.OpenGL.GL            (($=))

import           Linear                                  (V2(..))
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Shader

import           Yage.Rendering.Backend.Renderer.Lenses  as Types
import           Yage.Rendering.Backend.Renderer.Logging
import           Yage.Rendering.Backend.Renderer.Types   as Types
{-=================================================================================================-}

import           Paths_yage_rendering


data RenderBatch r = RenderBatch
    { withBatch     :: ([r] -> Renderer ()) -> Renderer ()
    , perItemAction :: r -> Renderer ()
    , batch         :: [r]
    }


-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> RenderSettings -> IO (a, (), RenderLog)
runRenderer renderer env = runRWST render env ()
    where
        render = do
            beforeRender
            a <- renderer
            afterRender
            return a


--renderView :: RenderView -> [ViewEntity] -> Renderer ()
--renderView view vdefs = renderFrame view vdefs >> afterFrame



--afterFrame :: Renderer ()
--afterFrame = return ()


renderFrame :: RenderView -> [ViewEntity] -> Renderer ()
renderFrame view vdefs = do
    --- beforeRender

    (_a, _time) <- ioTime $ doRender view vdefs
    return ()
    --let stats = RenderStatistics
    --        { lastObjectCount    = -1
    --        , lastRenderDuration = renderTime
    --        , lastTriangleCount  = -1 -- sum $! map (triCount . model) $ entities scene
    --        , loadedShadersCount = shCount
    --        , loadedMeshesCount  = mshCount
    --        }
    --logRenderM $ show stats

    -- afterRender


doRender :: RenderView -> [ViewEntity] -> Renderer ()
doRender view vdefs =
    let batches = createShaderBatches view vdefs
    in forM_ batches $ renderBatch view



renderBatch :: RenderView -> RenderBatch ViewEntity -> Renderer ()
renderBatch view RenderBatch{..} = withBatch $
    mapM_ (perItemAction >> renderViewEntity view)


createShaderBatches :: RenderView -> [ViewEntity] -> [RenderBatch ViewEntity]
createShaderBatches _view vdefs =
    let shaderGroups = groupBy sameShader vdefs
    in map mkShaderBatch shaderGroups
    where
        sameShader :: ViewEntity -> ViewEntity -> Bool
        sameShader a b = a^.vdRenderData.shaderProgram.to program 
                         == b^.vdRenderData.shaderProgram.to program

        mkShaderBatch :: [ViewEntity] -> RenderBatch ViewEntity
        mkShaderBatch []         = error "empty RenderBatch: should be at least one for all items"
        mkShaderBatch defs@(v:_) =
            let batchShader = v^.vdRenderData.shaderProgram
            in RenderBatch
                { withBatch     = \m -> withShader batchShader (const $ m defs)
                , perItemAction = \_ -> return ()
                , batch         = defs
                }



beforeRender :: Renderer ()
beforeRender = setupFrame


setupFrame :: Renderer ()
setupFrame = do
    --clearC <- undefined --  TODO view $ reRenderConfig.rcConfClearColor
    wire   <- view $ reRenderConfig.rcConfWireframe
    target <- view reRenderTarget
    io $! do
        --GL.clearColor  $= realToFrac <$> clearC
        GL.depthFunc   $= Just GL.Less    -- TODO to init
        GL.depthMask   $= GL.Enabled      -- TODO to init
        GL.blend       $= GL.Enabled      -- TODO to init/render target
        GL.blendFunc   $= (GL.SrcAlpha, GL.OneMinusSrcAlpha) -- TODO to init/render target
        GL.polygonMode $= if wire then (GL.Line, GL.Line) else (GL.Fill, GL.Fill)
        --GL.cullFace    $= Just GL.Back
        
        -- TODO to renderdef or shader?
        GL.pointSize   $= 2.0
        -- line width greater than 1 leads to an InvalidValue
        -- http://lwjgl.org/forum/index.php?action=printpage;topic=3106.0
        GL.lineWidth   $= 1.0

        -- for gl_PointSize in shader
        -- GL.vertexProgramPointSize   $= GL.Enabled

        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        when (target^.targetDirty) $
            let factor = fromIntegral . floor $ target^.targetFactor
                V2 w h = fromIntegral <$> target^.targetSize
                V2 x y = fromIntegral <$> target^.targetXY
            in GL.viewport $= ( GL.Position (factor * x) (factor * y)
                              , GL.Size (factor * w) (factor * h))


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()
    --updateStatistics


---------------------------------------------------------------------------------------------------

renderViewEntity :: RenderView -> ViewEntity -> Renderer ()
renderViewEntity _view vdef =
    let rdata = vdef^.vdRenderData
    in do
        checkErr "start rendering"
        io $! withVAO (rdata^.vao) . withTexturesAt GL.Texture2D (rdata^.to tUnits) $! do

            checkErr "preuniform"
            let (shaderDef, shaderEnv) = vdef^.vdUniformDef
            -- runUniform (udefs >> mapTextureSamplers texObjs) shaderEnv -- why no texture samplers anymore?
            runUniform shaderDef $ shaderEnv & seViewDef .~ vdef

            checkErr "postuniform"

            drawNow (rdata^.drawMode) rdata
            checkErr "after draw"
        logCountObj
        logCountTriangles (rdata^.elementCount)
        checkErr "end render"
    where
        checkErr msg = io $ throwErrorMsg msg

        tUnits d = over (mapped._2) (^._1) (d^.texObjs)

        drawNow mode@GL.Triangles rdata = GL.drawElements mode (getCnt mode rdata) GL.UnsignedInt nullPtr
        drawNow mode@GL.Points    rdata = GL.drawElements mode (getCnt mode rdata) GL.UnsignedInt nullPtr
        drawNow mode@GL.Lines     rdata = GL.drawElements mode (getCnt mode rdata) GL.UnsignedInt nullPtr
        drawNow mode _ = error $ format "primitive mode {0} not supported" [show mode]

        getCnt GL.Triangles rdata = fromIntegral $ 3 * rdata^.elementCount
        getCnt GL.Points    rdata = fromIntegral $ 3 * rdata^.elementCount
        getCnt GL.Lines     rdata = fromIntegral $ 3 * rdata^.elementCount -- TODO missing 2 lines in cube
        getCnt _ _ = error "the impossible happend"


---------------------------------------------------------------------------------------------------
{-- TODO INVESTIGATE
mapTextureSamplers :: [(GL.TextureObject, (GL.GLuint, String))] -> ShaderDefinition ()
mapTextureSamplers texObjs =
    let texUnitToUniform = texObjs^..traverse._2
    in do
        sp <- asks shaderEnv'Program
        io $ mapM_ (\(i, n) -> i `asUniform` getUniform sp n) texUnitToUniform
--}

---------------------------------------------------------------------------------------------------

-- | runs the renderer in the given environment to render one frame.

runUniform :: ShaderDefinition a -> ShaderEnv -> IO a
runUniform = runReaderT

---------------------------------------------------------------------------------------------------

infixr 2 !=
(!=) :: (AsUniform u) => String -> u -> ShaderDefinition ()
name != uni = do
    sp <- view seProgram
    io $ 
        uni `asUniform` getUniform sp name 
            `catch` \(e::SomeException) -> print $ format "warning: {0}" [show e]

shaderEnv :: ShaderDefinition ShaderEnv
shaderEnv = ask
