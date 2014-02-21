{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Yage.Rendering.Backend.Renderer (
      module Types
    , runRenderer, renderFrame
    , logRenderM
    , version
    , withFramebuffer, withShader, withTexturesAt
    ) where
---------------------------------------------------------------------------------------------------
import           Yage.Prelude                            hiding (log)

import           Control.Monad                           (forM_, mapM_, when)
import           Control.Monad.RWS.Strict                (runRWST)

import           Graphics.GLUtil                         (ShaderProgram(..))
import qualified Graphics.Rendering.OpenGL               as GL
import           Graphics.Rendering.OpenGL.GL            (($=))
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Backend.Framebuffer

import           Yage.Rendering.Backend.Renderer.Lenses  as Types
import           Yage.Rendering.Backend.Renderer.Logging
import           Yage.Rendering.Backend.Renderer.Types   as Types
import           Yage.Rendering.Uniforms
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



{--
renderToFramebuffer :: UniformFields (PlainRec urec) => [RenderSet urec] -> Framebuffer tex rbuff -> Renderer ()
renderToFramebuffer rdata toFramebuffer = 
    withFramebuffer toFramebuffer DrawTarget $ \_fb -> 
        renderFrame rdata
--}


renderFrame :: UniformFields (Uniforms urec) => [RenderSet urec] -> Renderer ()
renderFrame rdata = do
    forM_ rdata renderRenderSet




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


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()
    --updateStatistics


---------------------------------------------------------------------------------------------------

renderRenderSet :: UniformFields (PlainRec urec) => RenderSet urec -> Renderer ()
renderRenderSet rset = do
    mshader <- use currentShader
    withVAO (rset^.vao) . withTexturesAt GL.Texture2D (rset^.textureChannels) $! do

        -- runUniform (udefs >> mapTextureSamplers texObjs) shaderEnv -- why no texture samplers anymore?
        when (isJust mshader) $ io $ setUniforms (mshader^?!_Just) (rset^.uniformDefs)


        drawNow (rset^.drawMode) rset
    logCountObj
    logCountTriangles (rset^.vertexCount `div` 3)
    where
        -- checkErr msg = io $ GLU.throwErrorMsg msg

        drawNow mode rset = io $ GL.drawArrays mode 0 (rset^.vertexCount)
        --drawNow mode@GL.Triangles rset = io $ GL.drawElements mode (getCnt mode rset) GL.UnsignedInt nullPtr
        --drawNow mode@GL.Points    rset = io $ GL.drawElements mode (getCnt mode rset) GL.UnsignedInt nullPtr
        --drawNow mode@GL.Lines     rset = io $ GL.drawElements mode (getCnt mode rset) GL.UnsignedInt nullPtr
        --drawNow mode _ = error $ format "primitive mode {0} not supported" [show mode]


---------------------------------------------------------------------------------------------------
{-- TODO INVESTIGATE
mapTextureSamplers :: [(GL.TextureObject, (GL.GLuint, String))] -> ShaderDefinition ()
mapTextureSamplers texObjs =
    let texUnitToUniform = texObjs^..traverse._2
    in do
        sp <- asks shaderEnv'Program
        io $ mapM_ (\(i, n) -> i `asUniform` getUniform sp n) texUnitToUniform
--}


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
