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

data PassDescr target perFrame perEntity vertex where
    PassDescr :: { passTarget         :: RenderTarget target
                 , passShader         :: ShaderResource
                 , passPerFrameData   :: perFrame
                 -- , passPerEntityData  :: RenderEntity vr entU entT -> ShaderData entU entT
                 , passPreRendering   :: Renderer ()
                 , passPostRendering  :: Renderer ()
                 } -> PassDescr target perFrame perEntity vertex


mkRenderPass :: ( UniformFields (Uniforms fbU), UniformFields (Uniforms entU) ) =>
             FramebufferSetup (Uniforms fbU) -> [RenderSet (Uniforms entU)] -> Renderer ()
mkRenderPass fboSetup rSets = withFramebufferSetup fboSetup (renderFrame rSets)


mkTextureTarget :: Texture -> RenderTarget SingleRenderTarget
mkTextureTarget tex 
    | isTextureBuffer tex = RenderTarget (tex^.textureId ++ "-fbo") $ SingleRenderTarget tex
    | otherwise = error "mkTextureTarget: not a TextureBuffer!"


renderTargets :: PassDescr mrt f e v -> mrt
renderTargets PassDescr{passTarget} = let RenderTarget _ mrt = passTarget in mrt 


textureTarget :: Getter (RenderTarget SingleRenderTarget) Texture
textureTarget = to getter where
    getter (RenderTarget _ (SingleRenderTarget tex)) = tex


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
