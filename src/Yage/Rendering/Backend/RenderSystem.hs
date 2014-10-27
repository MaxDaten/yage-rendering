{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeOperators    #-}
module Yage.Rendering.Backend.RenderSystem
    ( module Yage.Rendering.Backend.RenderSystem
    , module Renderer
    , module Pass
    , module Framebuffer

    , module Vertex
    , module Shader
    , module Entity
    , module Resources
    ) where


import           Yage.Prelude                           hiding (pass)
import           Yage.Lens

import           Control.Monad.RWS                      hiding (forM, pass)

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.RenderPass      as Pass
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer

import           Yage.Rendering.Vertex                  as Vertex
import           Yage.Rendering.Shader                  as Shader
import           Yage.Rendering.RenderEntity            as Entity
import           Yage.Rendering.Resources               as Resources



type RenderSystem = RWST () RStatistics GLResources IO


data RStatistics = RStatistics
    { _resourcingTime :: !Double
    , _renderingTime  :: !Double
    , _resourceLog    :: ![String]
    , _renderLog      :: !RenderLog
    } deriving ( Show )

makeLenses ''RStatistics



runRenderSystem :: (MonadIO m) => RenderSystem () -> GLResources -> m (GLResources, RStatistics)
runRenderSystem sys res = io $ execRWST sys () res



-- TODO individual settings
mkRenderSystem :: Renderer () -> RenderSystem ()
mkRenderSystem toRender = do
    ((_, rlog), time) <- io $ ioTime $ Renderer.runRenderer toRender
    scribe renderingTime time
    scribe renderLog rlog



runRenderPass :: ( MultipleRenderTargets mrt, ViableVertex (Vertex vr)
                 , IsShaderData entU entT
                 , IsShaderData frameU frameT
                 , u <: frameU, t <: frameT
                 , u <: entU, t <: entT
                 ) =>
                  PassDescr mrt (Shader u t (Vertex vr))
               -> ShaderData frameU frameT
               -> Seq (RenderEntity (Vertex vr) (ShaderData entU entT))
               -> RenderSystem ()
runRenderPass pass frameData entities = do
    -- transform all Renderables into RenderSets
    (setup, renderSets) <- managePassResoures
    mkRenderSystem $ mkRenderPass setup renderSets

    where
    managePassResoures = do
        -- get current loaded resource state
        res <- get

        -- load resources from framebuffer setup and all entities
        ((results, res', reslog), time) <- ioTime $ runResourceManager res $
            (,) <$> (requestFramebufferSetup pass frameData)
                <*> (forM entities $ requestRenderSet $ pass^.passShader.shaderProgram)

        -- write resource loading log
        scribe resourceLog reslog
        scribe resourcingTime time

        -- update resource state with newly loaded resources
        put res'
        return results


instance Monoid RStatistics where
    mempty = RStatistics 0 0 mempty mempty
    mappend (RStatistics a b c d) (RStatistics a' b' c' d') = RStatistics (a + a') (b + b') (mappend c c') (mappend d d')
