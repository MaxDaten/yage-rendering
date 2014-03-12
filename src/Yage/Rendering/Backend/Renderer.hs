{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
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

import qualified Graphics.Rendering.OpenGL               as GL
#ifdef GL_ERRCHECK
import qualified Graphics.GLUtil.GLError                 as GLE
#endif
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
data TextureAssignment = forall t. (GL.BindableTextureTarget t, Show t) => 
    TextureAssignment !GL.TextureObject !t !GL.GLuint String

instance Show TextureAssignment where
    show (TextureAssignment tobj target unit name ) = format "TextureAssignment: {0} {1} {2} {3}" [show tobj, show target, show unit, name]
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
        render = checkErrorOf "runRenderer" $ do
            beforeRender
            a <- renderer
            afterRender
            return a

withFramebufferSetup :: UniformFields (Uniforms u) => FramebufferSetup u -> Renderer a -> Renderer a
withFramebufferSetup FramebufferSetup{..} ma = do
    withFramebuffer framebuffer DrawTarget $ do
     withShader fbShader                   $ \sh -> do
      withTexturesAt globalTextures $ do
        preRendering
        io $ setUniforms sh fbGlobalUniforms
        r <- ma
        postRendering
        return r



renderFrame :: UniformFields (Uniforms urec) => [RenderSet urec] -> Renderer ()
renderFrame rdata = forM_ rdata renderRenderSet



beforeRender :: Renderer ()
beforeRender = checkError "beforeRender: "


---------------------------------------------------------------------------------------------------


afterRender :: Renderer ()
afterRender = checkError "afterRender: "


---------------------------------------------------------------------------------------------------

renderRenderSet :: UniformFields (Uniforms urec) => RenderSet urec -> Renderer ()
renderRenderSet rset = checkErrorOf ("renderRenderSet: " ++ show (rset^.vao)) $ do
    mshader <- use currentShader

    when (isJust mshader) $!
        io $ setUniforms (mshader^?!_Just) (rset^.uniformDefs)
    
    withVAO (rset^.vao) . withTexturesAt (rset^.textureChannels) $!
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
withShader shader m = checkErrorOf ("withShader" ++ show shader) $ do
    currentShader ?= shader
    io $! GL.currentProgram $= Just (program shader)
    
    res <- m shader
    
    io $! GL.currentProgram $= Nothing
    currentShader .= Nothing
    return res


-- https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil

withVAO :: GL.VertexArrayObject -> Renderer r -> Renderer r
withVAO v ma = checkErrorOf "withVAO" $ do
    io $ GL.bindVertexArrayObject $= Just v
    r <- ma
    io $ GL.bindVertexArrayObject $= Nothing
    return r


-- from GLUtil do pull it into my Renderer monad
withTexturesAt :: [TextureAssignment]-> Renderer a -> Renderer a
withTexturesAt assigments m = do 
    mapM_ set assigments
    r <- m
    mapM_ cleanup assigments
    return r
    where
    set a@(TextureAssignment tobj target unitId _name) = 
        checkErrorOf ("activeTexture" ++ show a) $ io $ do
            GL.activeTexture $= GL.TextureUnit unitId
            GL.textureBinding target $= Just tobj
    cleanup a@(TextureAssignment _tobj target unitId _name)= 
        checkErrorOf ("cleanupTexture" ++ show a) $ io $ do 
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

checkError :: (MonadIO m) => String -> m ()
#ifdef GL_ERRCHECK
checkError = io . GLE.printErrorMsg
#else
checkError _ = return ()
#endif
{-# INLINE checkError #-}

checkErrorOf :: (MonadIO m) => String -> m a -> m a
#ifdef GL_ERRCHECK
checkErrorOf msg ma = do {x <- ma; (io $ GLE.printErrorMsg msg); return x}
#else
checkErrorOf _ = id
#endif
{-# INLINE checkErrorOf #-}

deriving instance Show ShaderProgram