{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Yage.Rendering.Backend.Renderer (
      module Types
    , runRenderer
    , renderView
    , (!=), shaderEnv
    , logRenderM
    , version
    ) where
---------------------------------------------------------------------------------------------------
import           Control.Lens                            hiding (indices)
import           Yage.Prelude                            hiding (log)

import           Data.List                               (groupBy, map)

import           Control.Monad                           (forM_, mapM_)
import           Control.Monad.Reader                    (ask, runReaderT)
import           Control.Monad.RWS.Strict                (runRWST)

import           Graphics.GLUtil                         hiding (makeVAO,
                                                          offset0)
import qualified Graphics.Rendering.OpenGL               as GL
import           Graphics.Rendering.OpenGL.GL            (($=))
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



renderView :: RenderView -> [ViewDefinition] -> Renderer ()
renderView view vdefs = renderFrame view vdefs >> afterFrame



afterFrame :: Renderer ()
afterFrame = return ()


renderFrame :: RenderView -> [ViewDefinition] -> Renderer ()
renderFrame view vdefs = do
    beforeRender

    (_a, _time) <- ioTime $ doRender view vdefs
    --let stats = RenderStatistics
    --        { lastObjectCount    = -1
    --        , lastRenderDuration = renderTime
    --        , lastTriangleCount  = -1 -- sum $! map (triCount . model) $ entities scene
    --        , loadedShadersCount = shCount
    --        , loadedMeshesCount  = mshCount
    --        }
    --logRenderM $ show stats

    afterRender


doRender :: RenderView -> [ViewDefinition] -> Renderer ()
doRender view vdefs =
    let batches = createShaderBatches view vdefs
    in forM_ batches $ renderBatch view



renderBatch :: RenderView -> RenderBatch ViewDefinition -> Renderer ()
renderBatch view RenderBatch{..} = withBatch $
    mapM_ (perItemAction >> renderViewDefinition view)


createShaderBatches :: RenderView -> [ViewDefinition] -> [RenderBatch ViewDefinition]
createShaderBatches _view vdefs =
    let shaderGroups = groupBy sameShader vdefs
    in map mkShaderBatch shaderGroups
    where
        sameShader :: ViewDefinition -> ViewDefinition -> Bool
        sameShader a b = (a^.vdRenderData.to (program . shaderProgram)) == (b^.vdRenderData.to (program . shaderProgram))

        mkShaderBatch :: [ViewDefinition] -> RenderBatch ViewDefinition
        mkShaderBatch []         = error "empty RenderBatch: should be at least one for all items"
        mkShaderBatch defs@(v:_) =
            let batchShader = v^.vdRenderData.to shaderProgram
            in RenderBatch
                { withBatch     = \m -> withShader batchShader (const $ m defs)
                , perItemAction = \_ -> return ()
                , batch         = defs
                }



beforeRender :: Renderer ()
beforeRender = setupFrame


setupFrame :: Renderer ()
setupFrame = do
    clearC <- view $ reRenderConfig.rcConfClearColor
    wire   <- view $ reRenderConfig.rcConfWireframe
    target <- view reRenderTarget
    io $! do
        GL.clearColor $= fmap realToFrac clearC
        GL.depthFunc $= Just GL.Less    -- TODO to init
        GL.depthMask $= GL.Enabled      -- TODO to init
        GL.blend     $= GL.Enabled      -- TODO to init/render target
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha) -- TODO to init/render target
        GL.polygonMode $= if wire then (GL.Line, GL.Line) else (GL.Fill, GL.Fill)

        GL.clear [GL.ColorBuffer, GL.DepthBuffer]


        let (w, h) = target^.targetSize
            r      = floor $ target^.targetRatio
        GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral (r * w)) (fromIntegral (r * h)))


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()
    --updateStatistics


---------------------------------------------------------------------------------------------------

renderViewDefinition :: RenderView -> ViewDefinition -> Renderer ()
renderViewDefinition _view vdef =
    let rdata = vdef^.vdRenderData
    in do
        checkErr "start rendering"
        io $! withVAO (rdata^.to vao) . withTexturesAt GL.Texture2D (tUnits rdata) $! do

            checkErr "preuniform"
            let (shaderDef, shaderEnv) = vdef^.vdUniformDef
            -- runUniform (udefs >> mapTextureSamplers texObjs) shaderEnv -- why no texture samplers anymore?
            runUniform shaderDef $ shaderEnv & seViewDef .~ vdef

            checkErr "postuniform"

            drawIndexedTris . fromIntegral $ triangleCount rdata
            checkErr "after draw"
        logCountObj
        logCountTriangles (triangleCount rdata)
        checkErr "end render"
    where
        checkErr msg = io $ throwErrorMsg msg

        tUnits d = over (mapped._2) (^._1) (texObjs d)


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
-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> RenderEnv -> IO (a, (), RenderLog)
runRenderer renderer env = runRWST renderer env ()

runUniform :: ShaderDefinition a -> ShaderEnv -> IO a
runUniform = runReaderT

---------------------------------------------------------------------------------------------------

infixr 2 !=
(!=) :: (AsUniform u) => String -> u -> ShaderDefinition ()
name != uni = do
    sp <- view seProgram
    io $ uni `asUniform` getUniform sp name

shaderEnv :: ShaderDefinition ShaderEnv
shaderEnv = ask
