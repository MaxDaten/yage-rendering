{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, TupleSections, GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
module Yage.Rendering (
      module GLReExports
    , module Types
    , runRenderer
    , renderView
    , (!=), shaderEnv
    , logRenderM
    , version
    ) where
---------------------------------------------------------------------------------------------------
import             Yage.Prelude                    hiding (log)
import             Control.Lens                    hiding (indices)

import             Data.List                       (head, map, groupBy, (++))
import qualified   Data.HashMap.Strict             as M

import             Control.Monad.RWS.Strict        (gets, modify, asks, runRWST)
import             Control.Monad.Reader            (runReaderT, ask)
import             Control.Monad                   (mapM, mapM_, forM_)

import             Graphics.GLUtil                 hiding (makeVAO, offset0)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))
---------------------------------------------------------------------------------------------------
import             Yage.Rendering.Types            as Types
import             Yage.Rendering.Shader
import             Yage.Rendering.VertexSpec
import             Yage.Rendering.Utils
import             Yage.Rendering.Logging
import             Yage.Rendering.Internal.RenderWorldTypes
{-=================================================================================================-}

import Paths_yage_rendering


data RenderBatch r = RenderBatch
    { withBatch         :: ([r] -> Renderer ()) -> Renderer ()
    , perItemAction     :: r -> Renderer ()
    , batch             :: [r]
    }


renderView :: RenderView -> [ViewDefinition] -> Renderer ()
renderView view vdefs = renderFrame view vdefs >> afterFrame



afterFrame :: Renderer ()
afterFrame = io $ do
    return ()


renderFrame :: RenderView -> [ViewDefinition] -> Renderer ()
renderFrame view vdefs = do
    beforeRender
    
    (_, _) <- ioTime $ doRender view vdefs

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
createShaderBatches view vdefs = 
    let shaderGroups = groupBy sameShader vdefs
    in map mkShaderBatch shaderGroups
    where
        sameShader :: ViewDefinition -> ViewDefinition -> Bool
        sameShader a b = (a^.vdRenderData.to (program . shaderProgram)) == (b^.vdRenderData.to (program . shaderProgram))

        mkShaderBatch :: [ViewDefinition] -> RenderBatch ViewDefinition
        mkShaderBatch defs@(v:vs) =
            let batchShader = v^.vdRenderData.to shaderProgram
            in RenderBatch
                { withBatch     = \m -> withShader batchShader (const $ m defs)
                , perItemAction = \_ -> return ()
                , batch         = defs
                }



beforeRender :: Renderer ()
beforeRender = do
    setupFrame


setupFrame :: Renderer ()
setupFrame = do
    clearC <- asks $ confClearColor . envConfig
    wire   <- asks $ confWireframe . envConfig
    target <- asks renderTarget
    io $! do
        GL.clearColor $= fmap realToFrac clearC
        GL.depthFunc $= Just GL.Less    -- TODO to init
        GL.depthMask $= GL.Enabled      -- TODO to init
        GL.blend     $= GL.Enabled      -- TODO to init/render target
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha) -- TODO to init/render target
        GL.polygonMode $= if wire then (GL.Line, GL.Line) else (GL.Fill, GL.Fill)
        
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]


        let (w, h) = target'size target
            r      = floor $ target'ratio target
        GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()
    --withWindow $ \win -> io . endDraw $ win
    --updateStatistics
            

---------------------------------------------------------------------------------------------------

renderViewDefinition :: RenderView -> ViewDefinition -> Renderer ()
renderViewDefinition view vdef = 
    let rdata = vdef^.vdRenderData
    in do
        checkErr "start rendering"
        io $! withVAO (rdata^.to vao) . withTexturesAt GL.Texture2D (tUnits rdata) $! do
            
            checkErr "preuniform"
            let uniDef = uniform'def $ vdef^.vdUniformDef
            -- runUniform (udefs >> mapTextureSamplers texObjs) shaderEnv -- why no texture samplers anymore?
            runUniform uniDef shaderEnv
            
            checkErr "postuniform"
            
            drawIndexedTris . fromIntegral $ triangleCount rdata
            checkErr "after draw"
        logCountObj
        logCountTriangles (triangleCount rdata)
        checkErr "end render"
    where
        checkErr msg = io $ throwErrorMsg $ msg -- ++ (show $ (fromRenderable r :: Maybe RenderEntity))
            
        tUnits d = over (mapped._2) (^._1) (texObjs d)
            
        shaderEnv = ShaderEnv 
            { shaderEnv'Program           = shaderProgram $ vdef^.vdRenderData
            , shaderEnv'CurrentRenderable = undefined
            , shaderEnv'CurrentScene      = undefined -- scene
            }

---------------------------------------------------------------------------------------------------


-- | runs the renderer in the given environment to render one frame.
-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> RenderEnv -> IO (a, (), RenderLog)
runRenderer renderer env = runRWST renderer env ()

runUniform :: UniShader a -> ShaderEnv -> IO a
runUniform u env = runReaderT u env

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
