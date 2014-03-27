{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module Yage.Rendering
    ( module Types
    , module Yage.Rendering
    , module Lenses
    
    , module Mesh
    , module Vertex
    , module Uniforms
    , module Viewport
    
    , module LinExport
    , module Framebuffer
    , module ResourceManager
    , module Pass
    ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math

import           Control.Monad.RWS                      hiding (forM)

import           Linear                                 as LinExport

import           Yage.Rendering.Backend.Renderer        as Renderer
import           Yage.Rendering.Backend.RenderPass      as Pass
import           Yage.Rendering.Backend.Framebuffer     as Framebuffer
import           Yage.Rendering.Lenses                  as Lenses
import           Yage.Rendering.ResourceManager
import           Yage.Rendering.ResourceManager         as ResourceManager (GLResources, initialGLRenderResources)
import           Yage.Rendering.Types                   as Types

import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.Uniforms                as Uniforms
import           Yage.Rendering.Vertex                  as Vertex
import           Yage.Rendering.Viewport                as Viewport

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


runRenderPass :: (Show ident, MultipleRenderTargets mrt, Renderable ent vr, UniformFields (Uniforms globals), UniformFields (Uniforms locals) ) => 
              PassDescr ident mrt ent globals locals -> [ent] -> RenderSystem ()
runRenderPass passDescr@PassDescr{..} entities = do
    (setup, rSets) <- managePassResoures
    mkRenderSystem $ mkRenderPass setup rSets

    where
    managePassResoures = do
        res <- get 
        ((results, res', reslog), time) <- ioTime $ runResourceManager res $ 
            (,) <$> (requestFramebufferSetup passDescr)
                <*> (forM entities $ \e -> requestRenderSet passShader (passEntityUniforms e) (renderDefinition e))
        scribe resourceLog reslog
        scribe resourcingTime time
        put res'
        return results


instance Monoid RStatistics where
    mempty = RStatistics 0 0 mempty mempty
    mappend (RStatistics a b c d) (RStatistics a' b' c' d') = RStatistics (a + a') (b + b') (mappend c c') (mappend d d')