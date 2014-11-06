{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}

module Yage.Rendering.Backend.Renderer where
---------------------------------------------------------------------------------------------------
import           Yage.Prelude                            hiding (log, traceM, last)
import           Yage.Lens
import           Foreign.Ptr                             ( nullPtr, plusPtr )
import qualified Data.Vector.Storable                    as VS
import qualified Data.Map.Strict                         as M
import           Data.Foldable                           ( traverse_ )
import           Control.Monad.RWS.Strict                ( RWST, runRWST )

import qualified Yage.Core.OpenGL                        as GL

import           Graphics.Rendering.OpenGL.GL            (($=))
import           Graphics.GLUtil.ShaderProgram
---------------------------------------------------------------------------------------------------
import           Yage.Rendering.Backend.Framebuffer

import           Yage.Rendering.Shader
import           Yage.Rendering.RenderEntity
{-=================================================================================================-}

-- import           Paths_yage_rendering


data RenderLog = RenderLog
    { _rlLogObjCount :: !Int
    , _rlLogTriCount :: !Int
    , _rlTrace       :: ![String]
    , _rlGLDebug     :: ![String]
    } deriving (Show, Eq, Generic)

makeLenses ''RenderLog


{--
Texturing
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
    , _rsUniforms        :: !u
    , _rsTextures        :: ![GLTextureItem]
    , _rsVertexCount     :: !GL.GLsizei
    , _rsIndexRanges     :: ![(Int, Int)]
    , _rsDrawSettings    :: !GLDrawSettings
    } deriving (Generic)

makeLenses ''RenderSet


data FramebufferSetup u = FramebufferSetup
    { framebuffer       :: !GL.FramebufferObject
    , fbShader          :: !(ShaderProgram)
    , fbUniforms        :: !u
    , fbTextures        :: ![GLTextureItem]
    , fbPreFrame        :: Renderer ()
    , fbPostFrame       :: Renderer ()
    } deriving (Generic)


-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> IO (a, RenderLog)
runRenderer renderer = do
    GL.TextureUnit maxTextureUnits <- return $ GL.TextureUnit 16 -- FIXME: GL.get GL.maxTextureUnit crashes here
    (a, _st, rlog) <- runRWST render () $ initRenderState ( fromIntegral maxTextureUnits )
    return (a, rlog)
    where
        render = GL.checkErrorOf "runRenderer" $ do
            beforeRender
            a <- renderer
            afterRender
            return a


withFramebufferSetup :: UniformFields (Uniforms u) => FramebufferSetup (Uniforms u) -> Renderer a -> Renderer a
withFramebufferSetup FramebufferSetup{..} ma = do
    withFramebuffer framebuffer DrawTarget $ do
     withShader fbShader                   $ \sh -> do
        withTextures fbTextures            $ do
            fbPreFrame
            io $ setUniforms sh $ fbUniforms
            r <- ma
            fbPostFrame
            return r



renderFrame :: UniformFields (Uniforms urec) => Seq (RenderSet (Uniforms urec)) -> Renderer ()
renderFrame = mapM_ renderRenderSet


---------------------------------------------------------------------------------------------------


beforeRender :: Renderer ()
beforeRender = GL.checkError "beforeRender: "


afterRender :: Renderer ()
afterRender = GL.checkError "afterRender: "


---------------------------------------------------------------------------------------------------

renderRenderSet :: UniformFields (Uniforms u) => RenderSet (Uniforms u) -> Renderer ()
renderRenderSet set@RenderSet{..} = GL.checkErrorOf (unpack $ format "renderRenderSet: {}" ( Only $ Shown _rsVao ) ) $! do
    traceM $ unpack $ format "render: {}" (Only $ Shown set)
    withVAO _rsVao . withTextures _rsTextures $ do
            -- set element-wise uniform fields
            mShader <- use rStCurrentShader
            traverse_ setShaderFields mShader

            io $ GL.cullFace $= _rsDrawSettings^.cullFace
            -- todo sub elements with own texture settings
            let msg = unpack $ format "multiDrawElements {}" (Only $ Shown _rsIndexRanges)
            GL.checkErrorOf msg $ io . withMultiDrawIndices _rsIndexRanges $ \indexCountPtr byteOffsetPtr drawCount ->
                GL.multiDrawElements ( _rsDrawSettings^.renderMode ) indexCountPtr GL.UnsignedInt byteOffsetPtr drawCount

            logCountObj
            logCountTriangles (_rsVertexCount `div` 3)

    where

    withMultiDrawIndices ranges ma =
        let countV      = VS.fromList $ map (\(start, end) -> fromIntegral $ end - start + 1) ranges
            byteOffsetV = VS.fromList $ map (\(start, _) -> nullPtr `plusPtr` (start*4)) ranges
            blockCnt    = fromIntegral $ length ranges
        in VS.unsafeWith countV $ \cntPtr ->
            VS.unsafeWith byteOffsetV $ \offPtr ->
                ma cntPtr offPtr blockCnt

    setShaderFields :: ShaderProgram -> Renderer ()
    setShaderFields shader = do
        -- set all uniform fields (excluding texture units)
        io $ GL.checkErrorOf "setUniforms" $ setUniforms shader _rsUniforms

        -- set all asigned textures to it's sampler
        texMapping <- use $ rStTextures.slottedUnits
        _ <- M.traverseWithKey (setTextureSampler shader) texMapping
        return ()

    setTextureSampler shader name unit = do
        let msg = unpack $ format "setTextureSampler: {} {}" (Shown name, Shown unit)
        traceM msg
        io $ GL.checkErrorOf msg $ setUniform shader name (unit^.unitGL)

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
withShader shader m = GL.checkErrorOf ("withShader" ++ show shader) $ do
    rStCurrentShader ?= shader
    io $! GL.currentProgram $= Just (program shader)

    res <- m shader

    io $! GL.currentProgram $= Nothing
    rStCurrentShader .= Nothing
    return res


-- https://github.com/acowley/GLUtil/blob/master/src/Graphics/GLUtil

withVAO :: GL.VertexArrayObject -> Renderer a -> Renderer a
withVAO v ma = GL.checkErrorOf "withVAO" $ do
    io $ GL.bindVertexArrayObject $= Just v
    r <- ma
    io $ GL.bindVertexArrayObject $= Nothing
    return r


{--
# Texturing
--}

withTextures :: [ GLTextureItem ] -> Renderer a -> Renderer a
withTextures texs ma =
    withAssignedItems (map getItem texs) $ \units ->
        withActiveUnits (zip units texs) ma
    where
    getItem (GLTextureItem _ item) = item


withActiveUnits :: [(TextureUnit, GLTextureItem)] -> Renderer a -> Renderer a
withActiveUnits units ma = do
    let msg = unpack $ format "withActiveUnits: {}" (Only $ Shown units)
    traceM msg
    forM_ units $ GL.checkErrorOf msg . activateUnit
    a <- ma
    forM_ units $ GL.checkErrorOf msg . deactivateUnit
    return a

    where

    activateUnit ( (TextureUnit _ Nothing), _)   = error "withActiveUnits:activateUnit: invalid empty TextureUnit"
    activateUnit ( unit@(TextureUnit _ (Just item)), tex ) =
        case tex of
        GLTextureItem target _ -> GL.checkErrorOf (unpack $ format "activateUnit: {}" (Only $ Shown unit)) $ do
            io $ GL.activeTexture         GL.$= unit^.unitGL
            io $ GL.textureBinding target GL.$= ( Just $ item^.itemTexObj )

    deactivateUnit (unit, tex) =
        case tex of
        GLTextureItem target _ -> GL.checkErrorOf (unpack $ format "deactivateUnit: {}" (Only $ Shown unit)) $ do
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
    unit <- takeFreeUnit <&> unitSlot ?~ slot
    rStTextures.slottedUnits.at ( slot^.itemKey ) ?= unit
    return unit


takeFreeUnit :: Renderer TextureUnit
takeFreeUnit = zoom rStTextures $ do
    (unit:queue) <- use $ freeUnits
    freeUnits   .= queue
    return unit


putFreeUnit :: TextureUnit -> Renderer ()
putFreeUnit (TextureUnit _ (Just _)) = error "putFreeUnit: not a free unit"
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
    show RenderSet{..} = show $
        format "RenderSet: { vao: {}, texs: {}, mode: {}, elem# {} }"
               ( Shown _rsVao, Shown _rsTextures, Shown _rsDrawSettings, Shown _rsVertexCount )

instance Monoid RenderLog where
    mempty = RenderLog 0 0 [] []
    mappend (RenderLog ca ta trc dbg) (RenderLog cb tb trc' dbg') = RenderLog (ca + cb) (ta + tb) (mappend trc trc') (mappend dbg dbg')

instance NFData RenderLog where rnf = genericRnf

---------------------------------------------------------------------------------------------------

traceM :: String -> Renderer ()
traceM msg = scribe rlTrace [msg]

logCountObj :: Renderer ()
logCountObj = scribe rlLogObjCount 1

logCountTriangles :: (Integral i) => i -> Renderer ()
logCountTriangles = scribe rlLogTriCount . fromIntegral

