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
    
    , module Mesh
    , module Vertex
    , module Shader
    , module Viewport
    
    , module LinExport
    , module Framebuffer
    , module Resources
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
import           Yage.Rendering.Types                   as Types

import           Yage.Rendering.Mesh                    as Mesh
import           Yage.Rendering.Shader                  as Shader
import           Yage.Rendering.Vertex                  as Vertex
import           Yage.Rendering.Viewport                as Viewport
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


runRenderPass :: ( Show ident, MultipleRenderTargets mrt, ViableVertex (Vertex vr)
                 , HasShaderData entU entT, HasShaderData frameU frameT ) => 
              PassDescr ident mrt vr frameU frameT entU entT -> [RenderEntity vr entU entT] -> RenderSystem ()
runRenderPass passDescr@PassDescr{..} entities = do
    -- transform all Renderables into RenderSets
    (setup, renderSets) <- managePassResoures
    mkRenderSystem $ mkRenderPass setup renderSets

    where
    managePassResoures = do
        -- get current loaded resource state
        res <- get

        -- load resources from framebuffer setup and all entities
        ((results, res', reslog), time) <- ioTime $ runResourceManager res $ 
            (,) <$> (requestFramebufferSetup passDescr)
                <*> (forM entities $ requestRenderSet passShader)
        
        -- write resource loading log
        scribe resourceLog reslog
        scribe resourcingTime time

        -- update resource state with newly loaded resources
        put res'
        return results


instance Monoid RStatistics where
    mempty = RStatistics 0 0 mempty mempty
    mappend (RStatistics a b c d) (RStatistics a' b' c' d') = RStatistics (a + a') (b + b') (mappend c c') (mappend d d')

