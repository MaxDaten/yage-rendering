{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}

module Yage.Rendering.Backend.Renderer where
---------------------------------------------------------------------------------------------------
import           Yage.Prelude                            hiding (log)
import           Yage.Lens

import qualified Data.Map.Strict                         as M
import           Data.Foldable                           ( traverse_ )
import           Control.Monad.RWS.Strict                ( RWST, runRWST )
import           Control.Monad                           ( MonadPlus(..) )

import qualified Graphics.Rendering.OpenGL               as GL
#ifdef GL_ERRCHECK
import qualified Graphics.GLUtil.GLError                 as GLE
#endif
import           Graphics.Rendering.OpenGL.GL            (($=))
import           Graphics.GLUtil.ShaderProgram
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Backend.Framebuffer

import           Yage.Rendering.Shader
import           Yage.Rendering.Types
{-=================================================================================================-}

-- import           Paths_yage_rendering


data RenderLog = RenderLog 
    { _rlLogObjCount :: !Int
    , _rlLogTriCount :: !Int
    , _rlLog         :: ![String]
    } deriving (Show, Eq)

makeLenses ''RenderLog


{--
Textureing
--}


type TextureField = String

data TextureItem = TextureItem
    { _itemTexObj   :: !GL.TextureObject 
    , _itemKey      :: !TextureField
    -- , _itemTarget   :: !TexTarget
    } deriving ( Show, Eq, Ord )

makeLenses ''TextureItem


data TextureUnit = TextureUnit
    { _unitGL    :: !GL.TextureUnit                  -- | internal unitId, once created, never changed
    , _unitSlot  :: !(Maybe TextureItem)
    } deriving ( Show, Eq, Ord )

makeLenses ''TextureUnit


data TextureAssignments = TextureAssignments 
    { _freeUnits      :: ![TextureUnit]
    , _slottedUnits   :: !(Map TextureField TextureUnit)
    } deriving ( Show )

makeLenses ''TextureAssignments

{--
RenderState
--}

data RenderState = RenderState
    { _rStCurrentShader   :: !(Maybe ShaderProgram)
    , _rStTextures        :: !TextureAssignments
    }

makeLenses ''RenderState

type Renderer = RWST () RenderLog RenderState IO
---------------------------------------------------------------------------------------------------  

data GLTextureItem = forall t. (GL.BindableTextureTarget t, Show t) => GLTextureItem t TextureItem

deriving instance Show GLTextureItem
---------------------------------------------------------------------------------------------------

-- RenderSet is a ready to go data structure
-- with all loaded opengl objects
data RenderSet u = RenderSet
    { _rsVao             :: !GL.VertexArrayObject
    , _rsUniforms        :: !(Uniforms u)
    , _rsTextures        :: ![GLTextureItem]
    , _rsVertexCount     :: !GL.GLsizei
    , _rsDrawSettings    :: !GLDrawSettings
    }

makeLenses ''RenderSet



data FramebufferSetup u = FramebufferSetup
    { framebuffer       :: !GL.FramebufferObject
    , fbShader          :: !(ShaderProgram)
    , fbUniforms        :: !(Uniforms u)
    , fbTextures        :: ![GLTextureItem]
    , fbPreFrame        :: Renderer ()
    , fbPostFrame       :: Renderer ()
    }



-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> IO (a, RenderLog)
runRenderer renderer = do
    GL.TextureUnit maxTextureUnits <- GL.get GL.maxTextureUnit
    (a, _st, rlog) <- runRWST render () $ initRenderState ( fromIntegral maxTextureUnits )
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
        withTextures fbTextures            $ do
            fbPreFrame
            io $ setUniforms sh $ fbUniforms
            r <- ma
            fbPostFrame
            return r



renderFrame :: UniformFields (Uniforms urec) => [RenderSet urec] -> Renderer ()
renderFrame = mapM_ renderRenderSet


---------------------------------------------------------------------------------------------------


beforeRender :: Renderer ()
beforeRender = checkError "beforeRender: "


afterRender :: Renderer ()
afterRender = checkError "afterRender: "


---------------------------------------------------------------------------------------------------

renderRenderSet :: UniformFields (Uniforms u) => RenderSet u -> Renderer ()
renderRenderSet RenderSet{..} = checkErrorOf (format "renderRenderSet: {0}" [ show _rsVao ] ) $!
    withVAO _rsVao . withTextures _rsTextures $ do
            -- set element-wise uniform fields
            mShader <- use rStCurrentShader
            traverse_ setShaderFields mShader

            io $ GL.cullFace $= _rsDrawSettings^.cullFace
            io $ GL.drawArrays (_rsDrawSettings^.renderMode) 0 _rsVertexCount
        
            logCountObj
            logCountTriangles (_rsVertexCount `div` 3)

    where
    setShaderFields :: ShaderProgram -> Renderer ()
    setShaderFields shader = do
        texMapping <- use $ rStTextures.slottedUnits
        io $ setUniforms shader _rsUniforms
        _ <- io $ M.traverseWithKey (setTextureSampler shader) texMapping
        return ()

    setTextureSampler shader name unit = setUniform shader name (unit^.unitGL)

---------------------------------------------------------------------------------------------------

-- | the current bound fbo is NOT restored (lack of support by the OpenGL lib),
-- instead the default is restored 
withFramebuffer :: GL.FramebufferObject -> FBOTarget -> Renderer a -> Renderer a
withFramebuffer fbo t action =
    let target = toGLTarget t in do
    -- old <- return GL.FramebufferObject 0 -- TODO get real git glGetIntegerv GL_FRAMEBUFFER_BINDING
    --currentFramebuffer ?= fb
    io $ GL.bindFramebuffer target $= fbo
    
    result <- action
    
    io $ GL.bindFramebuffer target $= GL.defaultFramebufferObject
    --currentFramebuffer .= Nothing
    return result
    where
    toGLTarget :: FBOTarget -> GL.FramebufferTarget
    toGLTarget DrawTarget        = GL.DrawFramebuffer 
    toGLTarget ReadTarget        = GL.ReadFramebuffer
    toGLTarget FramebufferTarget = GL.Framebuffer



withShader :: ShaderProgram -> (ShaderProgram -> Renderer a) -> Renderer a
withShader shader m = checkErrorOf ("withShader" ++ show shader) $ do
    rStCurrentShader ?= shader
    io $! GL.currentProgram $= Just (program shader)
    
    res <- m shader
    
    io $! GL.currentProgram $= Nothing
    rStCurrentShader .= Nothing
    return res


-- https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil

withVAO :: GL.VertexArrayObject -> Renderer a -> Renderer a
withVAO v ma = checkErrorOf "withVAO" $ do
    io $ GL.bindVertexArrayObject $= Just v
    r <- ma
    io $ GL.bindVertexArrayObject $= Nothing
    return r


{--
# Texturing
--}

withTextures :: [GLTextureItem] -> Renderer a -> Renderer a
withTextures texs ma = 
    withAssignedItems (map getItem texs) $ \units ->
        withActiveUnits (zip units texs) ma
    where
    getItem (GLTextureItem _ item) = item


withActiveUnits :: [(TextureUnit, GLTextureItem)] -> Renderer a -> Renderer a
withActiveUnits units ma = do
    forM_ units $ activateUnit
    a <- ma
    forM_ units deactivateUnit
    return a

    where
    
    activateUnit ( (TextureUnit _ Nothing), _)   = error "withActiveUnits:activateUnit: invalid empty TextureUnit"
    activateUnit ( unit@(TextureUnit _ (Just item)), tex ) = 
        case tex of
        GLTextureItem target _ -> do
            io $ GL.activeTexture         GL.$= unit^.unitGL 
            io $ GL.textureBinding target GL.$= item^.itemTexObj.to Just

    deactivateUnit (unit, tex) =
        case tex of
        GLTextureItem target _ -> do
            io $ GL.activeTexture         GL.$= unit^.unitGL
            io $ GL.textureBinding target GL.$= Nothing




withAssignedItems :: [TextureItem] -> ( [TextureUnit] -> Renderer a ) -> Renderer a
withAssignedItems items ma = do
    units <- forM items assignTextureUnit 
    a     <- ma units
    forM_ units releaseTextureUnit
    return a


assignTextureUnit :: TextureItem -> Renderer TextureUnit
assignTextureUnit slot@TextureItem{..} = 
    lookupUnit _itemKey <|> slotNextFreeUnit slot


releaseTextureUnit :: TextureUnit -> Renderer ()
releaseTextureUnit unit =
    releaseUnit unit


lookupUnit :: TextureField -> Renderer TextureUnit
lookupUnit key = zoom rStTextures $
    returning =<< (uses slottedUnits $ M.lookup key)
    where
    returning (Just unit) = return unit
    returning Nothing     = mzero



slotNextFreeUnit :: TextureItem -> Renderer TextureUnit
slotNextFreeUnit slot = do
    unit <- takeFreeUnit
    return $ unit & unitSlot ?~ slot


takeFreeUnit :: Renderer TextureUnit
takeFreeUnit = zoom rStTextures $ do
    (unit:queue) <- use $ freeUnits
    freeUnits   .= queue
    return unit 


putFreeUnit :: TextureUnit -> Renderer ()
putFreeUnit (TextureUnit _ Nothing) = error "putFreeUnit: not a free unit"
putFreeUnit unit = rStTextures.freeUnits %= (:) unit


releaseUnit :: TextureUnit -> Renderer ()
releaseUnit      ( TextureUnit _   Nothing     ) = error "invalid releaseUnit: unit with empty slot"
releaseUnit unit@( TextureUnit _ ( Just slot ) ) = do
    rStTextures.slottedUnits.at ( slot^.itemKey ) .= Nothing
    putFreeUnit $ unit & unitSlot     .~ Nothing




initTextureUnits :: Int -> TextureAssignments
initTextureUnits unitCount =
    TextureAssignments { _freeUnits     = map newEmptyUnit [0..unitCount-1] 
                       , _slottedUnits  = M.empty
                       }


newEmptyUnit :: Int -> TextureUnit
newEmptyUnit i = TextureUnit (GL.TextureUnit $ fromIntegral i) Nothing


---------------------------------------------------------------------------------------------------

initRenderState :: Int -> RenderState
initRenderState = RenderState Nothing . initTextureUnits


emptyRenderLog :: RenderLog
emptyRenderLog = mempty


instance Show ( RenderSet urec ) where
    show RenderSet{..} = 
        format "RenderSet: { vao: {0}, texs: {1}, mode: {2}, elem# {3} }"
               [ show _rsVao, show _rsTextures, show _rsDrawSettings, show _rsVertexCount ]

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
