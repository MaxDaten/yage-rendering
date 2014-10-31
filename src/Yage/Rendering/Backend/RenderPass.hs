{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
module Yage.Rendering.Backend.RenderPass where

import Yage.Prelude
import Yage.Lens
import qualified Yage.Core.OpenGL as GL

import Yage.Rendering.Shader

import Yage.Rendering.Backend.Framebuffer
import Yage.Rendering.Backend.Renderer
import Yage.Geometry.D2.Rectangle



import Yage.Rendering.Resources.ResTypes



type MultipleRenderTargets mrt = FramebufferSpec mrt RenderTargets
newtype SingleRenderTarget = SingleRenderTarget Texture

type DefaultRenderTarget = DefaultFramebuffer RenderTargets

type TargetSlot = ByteString
data RenderTarget mrt = RenderTarget TargetSlot mrt

data PassDescr target prog where
    PassDescr :: { _passTarget         :: RenderTarget target
                 , _passShader         :: prog
                 , _passPreRendering   :: Renderer ()
                 , _passPostRendering  :: Renderer ()
                 } -> PassDescr target prog

makeLenses ''PassDescr


mkRenderPass :: ( UniformFields (Uniforms fbU), UniformFields (Uniforms entU) ) =>
             FramebufferSetup (Uniforms fbU) -> Seq (RenderSet (Uniforms entU)) -> Renderer ()
mkRenderPass fboSetup rSets = withFramebufferSetup fboSetup (renderFrame rSets)


-- | can be used to reuse TextureBuffer, this is also a waring ;)
mkSingleTextureTarget :: Texture -> RenderTarget SingleRenderTarget
mkSingleTextureTarget tex
    | tex^.isTextureBuffer = RenderTarget (tex^.textureId ++ "-fbo") $ SingleRenderTarget tex
    | otherwise = error "mkTextureTarget: not a TextureBuffer!"


mkSingleTargetFromSpec :: ByteString -> BufferSpec -> RenderTarget SingleRenderTarget
mkSingleTargetFromSpec name spec = RenderTarget (name ++ "-fbo")
    $ SingleRenderTarget
    $ mkTextureBuffer (name ++ "-buffer") GL.Texture2D spec
        & textureConfig.texConfFiltering.texMipmapFilter  .~ Nothing
        & textureConfig.texConfFiltering.texMinFilter     .~ GL.Linear'
        & textureConfig.texConfFiltering.texMagFilter     .~ GL.Linear'
        & textureConfig.texConfWrapping.texWrapRepetition .~ GL.Mirrored
        & textureConfig.texConfWrapping.texWrapClamping   .~ GL.Clamp



renderTargets :: Getter (PassDescr mrt p) mrt
renderTargets = to getter where
    getter PassDescr{_passTarget} = let RenderTarget _ mrt = _passTarget in mrt


targetTexture :: Lens' (RenderTarget SingleRenderTarget) Texture
targetTexture = lens getter setter where
    getter (RenderTarget _ (SingleRenderTarget tex)) = tex
    setter (RenderTarget slot _) tex = RenderTarget slot $ SingleRenderTarget tex


defaultRenderTarget :: RenderTarget (DefaultFramebuffer a)
defaultRenderTarget = RenderTarget "default" DefaultFramebuffer

instance FramebufferSpec SingleRenderTarget RenderTargets where
    fboColors (SingleRenderTarget texture) =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D texture 0
        ]

instance GetRectangle (RenderTarget SingleRenderTarget) Int where
    asRectangle = to getter where
        getter (RenderTarget _ target) = target^.asRectangle


instance GetRectangle (SingleRenderTarget) Int where
    asRectangle = to getter where
        getter (SingleRenderTarget tex) = tex^.asRectangle


instance HasTextureSpec SingleRenderTarget where
    textureSpec = to getter where
        getter (SingleRenderTarget tex) = tex^.textureSpec
