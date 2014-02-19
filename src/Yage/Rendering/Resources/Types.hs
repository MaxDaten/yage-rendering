{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE Rank2Types                         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE FunctionalDependencies             #-}
{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE RecordWildCards                    #-}
{-# LANGUAGE NamedFieldPuns                     #-}
{-# LANGUAGE ScopedTypeVariables                #-}
module Yage.Rendering.Resources.Types where

import           Yage.Prelude

import           Control.Monad.RWS

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.GLUtil



--import           Yage.Rendering.Types
import           Yage.Rendering.Mesh
import           Yage.Rendering.Resources.ResTypes
import           Yage.Rendering.Backend.Framebuffer

-- | container for all possible entities to render


--class (Monad rm, Ord def) => ResourceManaging rm def res | rm -> res where
--    requestResource :: def -> rm res
--    --requestFramebuffer   :: (Ord fbId)   => (fbId, a) -> rm fbhandler
--    --requestTexture       :: (Ord texId)  => texId  -> rm texhandler
--    --requestRenderbuffer  :: (Ord buffId) => buffId -> rm buffhandler
--    --requestShader        :: (Ord shId)   => shId   -> rm shhandler
--    --requestGeometry      :: (Ord geoId)  => geoId  -> rm geohandler
--    ---- | RenderItem is like the combound of a render-program and render-data
--    ----  e.g. shader and geometry 
--    --requestRenderItem    :: (Ord geoId, Ord shId) => (geoId, shId) -> rm iid

type GLVertexbuffer        = (MeshHash, GL.BufferObject)
type GLVertexArray         = VAO
type GLShader              = ShaderProgram
type GLTexture             = (GLBufferSpec, GL.TextureObject)
type GLRenderbuffer        = (GLBufferSpec, GL.RenderbufferObject)
type GLFramebuffer         = Framebuffer TextureResource RenderbufferResource


data GLResources = GLResources
    { _loadedShaders       :: (Map ShaderResource           GLShader            )
    , _loadedVertexBuffer  :: (Map MeshId                   GLVertexbuffer      )
    , _loadedVertexArrays  :: (Map (MeshId, ShaderResource) GLVertexArray       )
    , _loadedTextures      :: (Map TextureResource          GLTexture           )
    , _loadedRenderbuffers :: (Map RenderbufferResource     GLRenderbuffer      )
    , _compiledFBOs        :: (Map String                   GLFramebuffer       )
    }

makeLenses ''GLResources

{--
loadedShaders :: Lens' GLResources (Map ShaderResource GLShader)
loadedShaders f res@GLResources{_loadedShaders} 
    = fmap (\s -> res{_loadedShaders = s}) (f _loadedShaders)

--loadedVertexBuffer :: Lens' GLResources (Map MeshId (GLVertexbuffer vr))
--loadedVertexBuffer f res@(GLResources sh vb va ts rb fb) 
--    = fmap (\b -> GLResources sh b va ts rb fb) 
--           (f (get res))
--    where get res = _hole

loadedVertexArrays :: Lens' GLResources (Map (MeshId, ShaderResource) GLVertexArray)
loadedVertexArrays f res@GLResources{_loadedVertexArrays}
    = fmap (\a -> res{_loadedVertexArrays = a}) (f _loadedVertexArrays)

loadedTextures :: Lens' GLResources (Map TextureResource GLTexture)
loadedTextures f res@GLResources{_loadedTextures}
    = fmap (\t -> res{_loadedTextures = t}) (f _loadedTextures)


loadedRenderbuffers :: Lens' GLResources (Map RenderbufferResource GLRenderbuffer)
loadedRenderbuffers f res@GLResources{_loadedRenderbuffers}
    = fmap (\r -> res{_loadedRenderbuffers = r}) (f _loadedRenderbuffers)

compiledFBOs :: Lens' GLResources (Map String GLFramebuffer)
compiledFBOs f res@GLResources{_compiledFBOs}
    = fmap (\r -> res{_compiledFBOs = r}) (f _compiledFBOs)
--}

--instance Monoid GLRenderResources where
--    mappend (GLRenderResources sA vA aA tA fA rA) (GLRenderResources sB vB aB tB fB rB)
--           = GLRenderResources (sA `union` sB) (vA `union` vB) (aA `union` aB) (tA `union` tB) (fA `union` fB) (rA `union` rB)
--    mempty = GLRenderResources mempty mempty mempty mempty mempty mempty


type ResourceManager = RWST () [String] GLResources IO


