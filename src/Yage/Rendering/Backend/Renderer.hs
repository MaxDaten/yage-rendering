{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Yage.Rendering.Backend.Renderer (
      module Types
    , runRenderer, renderFrame
    , (!=), shaderEnv, runUniform
    , logRenderM
    , version
    , withFramebuffer, withShader, withTexturesAt
    ) where
---------------------------------------------------------------------------------------------------
import           Yage.Prelude                            hiding (log)

import           Foreign.Ptr                             (nullPtr)
import           Data.List                               (groupBy, map)

import           Control.Monad                           (forM_, mapM_, when)
import           Control.Monad.Reader                    (ask, runReaderT)
import           Control.Monad.RWS.Strict                (runRWST)

import qualified Graphics.GLUtil                         as GLU hiding (makeVAO, offset0)
import           Graphics.GLUtil                         (ShaderProgram(..))
import qualified Graphics.Rendering.OpenGL               as GL
import           Graphics.Rendering.OpenGL.GL            (($=))

import           Linear                                  (V2(..))
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Backend.Framebuffer

import           Yage.Rendering.Backend.Renderer.Lenses  as Types
import           Yage.Rendering.Backend.Renderer.Logging
import           Yage.Rendering.Backend.Renderer.Types   as Types
{-=================================================================================================-}

import           Paths_yage_rendering



-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> IO (a, RenderLog)
runRenderer renderer = do
    (a, _st, rlog) <- runRWST render () initRenderState
    return (a, rlog)
    where
        render = do
            beforeRender
            a <- renderer
            afterRender
            return a




renderToFramebuffer :: [RenderData] -> Framebuffer tex rbuff -> Renderer ()
renderToFramebuffer rdata toFramebuffer = 
    withFramebuffer toFramebuffer DrawTarget $ \_fb -> 
        renderFrame rdata


renderFrame :: [RenderData] -> Renderer ()
renderFrame rdata = do
    forM_ rdata renderRenderData




{--

data RenderBatch r = RenderBatch
    { withBatch     :: ([r] -> Renderer ()) -> Renderer ()
    , perItemAction :: r -> Renderer ()
    , batch         :: [r]
    }

doRender :: RenderView -> [RenderData] -> Renderer ()
doRender view vdefs =
    let batches = createShaderBatches view vdefs
    in forM_ batches $ renderBatch view

renderBatch :: RenderView -> RenderBatch RenderData -> Renderer ()
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
--}



beforeRender :: Renderer ()
beforeRender = return ()

{--

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
--}


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()
    --updateStatistics


---------------------------------------------------------------------------------------------------

renderRenderData :: RenderData -> Renderer ()
renderRenderData rdata = do
    checkErr "start rendering"
    mshader <- use currentShader
    withVAO (rdata^.vao) . withTexturesAt GL.Texture2D (rdata^.textureChannels) $! do

        checkErr "preuniform"
        -- runUniform (udefs >> mapTextureSamplers texObjs) shaderEnv -- why no texture samplers anymore?
        when (isJust mshader) $ runUniform (rdata^.uniformDefs) (mshader^?!_Just) -- ugly

        checkErr "postuniform"

        drawNow (rdata^.drawMode) rdata
        checkErr "after draw"
    logCountObj
    logCountTriangles (rdata^.elementCount)
    checkErr "end render"
    where
        checkErr msg = io $ GLU.throwErrorMsg msg

        drawNow mode@GL.Triangles rdata = io $ GL.drawElements mode (getCnt mode rdata) GL.UnsignedInt nullPtr
        drawNow mode@GL.Points    rdata = io $ GL.drawElements mode (getCnt mode rdata) GL.UnsignedInt nullPtr
        drawNow mode@GL.Lines     rdata = io $ GL.drawElements mode (getCnt mode rdata) GL.UnsignedInt nullPtr
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

runUniform :: ShaderDefinition a -> ShaderProgram -> Renderer a
runUniform = runReaderT

---------------------------------------------------------------------------------------------------

infixr 2 !=
(!=) :: (GLU.AsUniform u) => String -> u -> ShaderDefinition ()
name != uni = do
    sp <- ask
    io $ 
        uni `GLU.asUniform` GLU.getUniform sp name 
            `catch` \(e::SomeException) -> print $ format "warning: {0}" [show e]

shaderEnv :: ShaderDefinition ShaderProgram
shaderEnv = ask



-- | the current bound fbo is NOT restored (lack of support by the OpenGL lib),
-- instead the default is restored 
withFramebuffer :: Framebuffer tex rbuff -> FBOTarget -> (Framebuffer tex rbuff -> Renderer a) -> Renderer a
withFramebuffer fb@(Framebuffer fbo _) t action = 
    let target = getGLTarget t in do
    -- old <- return GL.FramebufferObject 0 -- TODO get real git glGetIntegerv GL_FRAMEBUFFER_BINDING
    --currentFramebuffer ?= fb
    io $ GL.bindFramebuffer target $= fbo
    
    result <- action fb
    
    io $ GL.bindFramebuffer target $= GL.defaultFramebufferObject
    --currentFramebuffer .= Nothing
    return result


withShader :: ShaderProgram -> (ShaderProgram -> Renderer a) -> Renderer a
withShader shader m = do
    currentShader ?= shader
    io $! GL.currentProgram $= Just (program shader)
    
    res <- m shader
    
    io $! GL.currentProgram $= Nothing
    currentShader .= Nothing
    return res


-- https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil

withVAO :: GL.VertexArrayObject -> Renderer r -> Renderer r
withVAO v ma = do
    io $ GL.bindVertexArrayObject $= Just v
    r <- ma
    io $ GL.bindVertexArrayObject $= Nothing
    return r


-- from GLUtil do pull it into my Renderer monad
withTexturesAt :: GL.BindableTextureTarget t
               => t -> [TextureAssignment]-> Renderer a -> Renderer a
withTexturesAt tt ts m = do 
    mapM_ aux ts
    r <- m
    mapM_ (cleanup . (^._2._1)) ts
    return r
    where 
        aux (t,(i,_)) = io $ do 
            GL.activeTexture $= GL.TextureUnit i
            GL.textureBinding tt $= Just t
        cleanup i = io $ do 
            GL.activeTexture $= GL.TextureUnit i
            GL.textureBinding tt $= Nothing
