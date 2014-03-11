{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Yage.Rendering.Backend.Renderer where
---------------------------------------------------------------------------------------------------
import           Yage.Prelude                            hiding (log)
import           Yage.Lens

import           Foreign.Ptr                             (nullPtr)
import           Control.Monad.RWS.Strict                (RWST, runRWST)

import           Graphics.GLUtil                         (ShaderProgram(..))
import qualified Graphics.Rendering.OpenGL               as GL
import           Graphics.Rendering.OpenGL.GL            (($=))
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Backend.Framebuffer

import           Yage.Rendering.Uniforms
import           Yage.Rendering.Types hiding (vertexCount)
import           Yage.Rendering.Lenses
{-=================================================================================================-}

-- import           Paths_yage_rendering


data RenderLog = RenderLog 
    { _rlLogObjCount :: !Int
    , _rlLogTriCount :: !Int
    , _rlLog         :: ![String]
    } deriving (Show, Eq)

makeLenses ''RenderLog

data RenderState = RenderState
    { -- _currentFramebuffer :: Maybe GLFramebuffer
     _currentShader      :: Maybe ShaderProgram
    }

makeLenses ''RenderState

type Renderer = RWST () RenderLog RenderState IO

---------------------------------------------------------------------------------------------------  
type TextureAssignment = (GL.TextureObject, (GL.GLuint, String))
---------------------------------------------------------------------------------------------------

data RenderSet urec = RenderSet -- TODO rename RenderResource
    { _vao             :: GL.VertexArrayObject
    , _uniformDefs     :: PlainRec urec
    , _textureChannels :: [TextureAssignment]
    , _vertexCount     :: !GL.GLsizei
    , _setDrawSettings :: !GLDrawSettings
    }

makeLenses ''RenderSet




data FramebufferSetup u = FramebufferSetup
    { framebuffer       :: GL.FramebufferObject
    , fbShader          :: ShaderProgram
    , fbGlobalUniforms  :: Uniforms u
    , globalTextures    :: [TextureAssignment]
    , preRendering      :: Renderer ()
    , postRendering     :: Renderer ()
    }



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

withFramebufferSetup :: UniformFields (Uniforms u) => FramebufferSetup u -> Renderer a -> Renderer a
withFramebufferSetup FramebufferSetup{..} ma = do
    withFramebuffer framebuffer DrawTarget $ do
     withShader fbShader                   $ \sh -> do
      withTexturesAt GL.Texture2D globalTextures $ do
        preRendering
        io $ setUniforms sh fbGlobalUniforms
        r <- ma
        postRendering
        return r



renderFrame :: UniformFields (Uniforms urec) => [RenderSet urec] -> Renderer ()
renderFrame rdata = forM_ rdata renderRenderSet



beforeRender :: Renderer ()
beforeRender = return ()


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = return ()


---------------------------------------------------------------------------------------------------

renderRenderSet :: UniformFields (Uniforms urec) => RenderSet urec -> Renderer ()
renderRenderSet rset = do
    mshader <- use currentShader

    when (isJust mshader) $!
        io $ setUniforms (mshader^?!_Just) (rset^.uniformDefs)
    
    withVAO (rset^.vao) . withTexturesAt GL.Texture2D (rset^.textureChannels) $!
        drawNow (rset^.setDrawSettings.renderMode) rset
    logCountObj
    logCountTriangles (rset^.vertexCount `div` 3)
    where
        -- checkErr msg = io $ GLU.throwErrorMsg msg

        drawNow mode rset = io $ do
            GL.cullFace $= rset^.setDrawSettings.cullFace
            GL.drawElements mode (rset^.vertexCount) GL.UnsignedInt nullPtr
        --drawNow mode rset = io $ GL.drawArrays mode 0 (rset^.vertexCount)

---------------------------------------------------------------------------------------------------

-- | the current bound fbo is NOT restored (lack of support by the OpenGL lib),
-- instead the default is restored 
withFramebuffer :: GL.FramebufferObject -> FBOTarget -> Renderer a -> Renderer a
withFramebuffer fbo t action = 
    let target = getGLTarget t in do
    -- old <- return GL.FramebufferObject 0 -- TODO get real git glGetIntegerv GL_FRAMEBUFFER_BINDING
    --currentFramebuffer ?= fb
    io $ GL.bindFramebuffer target $= fbo
    
    result <- action
    
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
withTexturesAt target assigment m = do 
    mapM_ set assigment
    r <- m
    mapM_ (cleanup . (^._2._1)) assigment
    return r
    where 
        set (tex,(unitId,_name)) = io $ do
            -- .0print $ format "set texture: {0} - {1}" [show unitId, show name]
            GL.activeTexture $= GL.TextureUnit unitId
            GL.textureBinding target $= Just tex
        cleanup unitId = io $ do 
            GL.activeTexture $= GL.TextureUnit unitId
            GL.textureBinding target $= Nothing

---------------------------------------------------------------------------------------------------

initRenderState :: RenderState
initRenderState = RenderState Nothing


emptyRenderLog :: RenderLog
emptyRenderLog = mempty


instance Show (RenderSet urec) where
    show RenderSet{..} = "RenderSet: { vao: {0}, texs: {1}, mode: {2}, elem# {3} }"

instance Monoid RenderLog where
    mempty = RenderLog 0 0 []
    mappend (RenderLog ca ta la) (RenderLog cb tb lb) = RenderLog (ca + cb) (ta + tb) (mappend la lb)

---------------------------------------------------------------------------------------------------

logRenderM :: String -> Renderer ()
logRenderM msg = scribe rlLog [msg]

logCountObj :: Renderer ()
logCountObj = scribe rlLogObjCount 1

logCountTriangles :: (Integral i) => i -> Renderer ()
logCountTriangles = scribe rlLogTriCount . fromIntegral

logRenderMf :: String -> [String] -> Renderer ()
logRenderMf msg args = logRenderM $ format msg args

isEmptyRenderLog :: RenderLog -> Bool
isEmptyRenderLog = (mempty ==)
