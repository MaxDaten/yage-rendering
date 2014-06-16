{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE FlexibleContexts            #-}

module Yage.Rendering.Mesh
  ( module Yage.Rendering.Mesh
  , module E
  ) where

import           Yage.Prelude                        hiding (Index, toList)
import           Yage.Lens                           hiding (snoc)
import qualified Yage.Text                           as TF

import qualified Data.Vector.Storable                as VS
import qualified Data.Vector                         as V
import           Data.Foldable                       (toList)
import           Data.List                           (mapAccumL)
import qualified Data.Digest.XXHash                  as XH
import qualified Data.Vector.Storable.ByteString     as BS

import           Data.Bits

import Yage.Geometry
import Yage.Geometry.Elements as E (Triangle(..))
---------------------------------------------------------------------------------------------------



type MeshHash = XH.XXHash
type MeshId   = Text


-- TODO: Materials to component
data MeshComponent = MeshComponent
  { _componentId             :: !MeshId
  , _componentIndexBuffer    :: !(VS.Vector Int) 
  , _componentHash           :: !MeshHash
  } deriving ( Typeable, Generic )


-- TODO: smart vertex book-keeping? 
data Mesh v = Mesh
    { _meshId             :: !MeshId
    , _meshVertexBuffer   :: !(VS.Vector v)
    , _meshComponents     :: !(Map MeshId MeshComponent)
    , _meshHash           :: !MeshHash
    } deriving ( Typeable, Generic )

makeLenses ''MeshComponent
makeLenses ''Mesh



instance Storable v => Show (Mesh v) where
    show Mesh{..} = TF.unpack $ 
      TF.format "Mesh { id = {}, #vertexBuffer = {}, meshHash = {}, components = {} }"
               ( TF.Shown _meshId
               , TF.Shown $ VS.length _meshVertexBuffer
               , TF.hex _meshHash
               , TF.Shown _meshComponents 
               )

instance Show MeshComponent where
  show MeshComponent{..} = TF.unpack $
    TF.format "MeshComponent { id = {}, #indexBuffer = {}, hash = {} }"
      ( TF.Shown _componentId
      , TF.Shown $ VS.length _componentIndexBuffer
      , TF.hex _componentHash
      )

instance Eq (Mesh v) where
    a == b = _meshId a == _meshId b



instance Ord (Mesh v) where
    compare a b = compare (_meshId a) (_meshId b)



vertexCount :: Storable v => Getter (Mesh v) Int
vertexCount = meshVertexBuffer.to VS.length

componentCount :: Getter (Mesh v) Int
componentCount = meshComponents.to (lengthOf traverse)

indexCount :: Getter MeshComponent Int
indexCount = componentIndexBuffer.to (VS.length)

meshComponentsHash :: Getter (Mesh v) MeshHash
meshComponentsHash = meshComponents.to compHashes where
    compHashes compMap = XH.xxHash . pack $ concatMap octets $ compMap^..traverse.componentHash


-- | concat of the indices of all `MeshComponent`s 
concatMeshIndices :: Getter (Mesh v) (VS.Vector Int)
concatMeshIndices = meshComponents.to concatIndices where
    concatIndices compMap = VS.concat $ compMap^..traverse.componentIndexBuffer


meshIndexRanges :: Getter (Mesh v) [(Int, Int)]
meshIndexRanges = meshComponents.to ranges where
    ranges compMap = snd $ mapAccumL (\pos len -> (pos+len, (pos, pos+len-1))) 0 $ 
                        filter (>0) $ compMap^..traverse.componentIndexBuffer.to VS.length

---------------------------------------------------------------------------------------------------


meshVertices :: Storable v => Lens' (Mesh v) (VS.Vector v)
meshVertices = lens _meshVertexBuffer setter where
  setter mesh verts = mesh & meshVertexBuffer .~ verts
                           & meshHash .~ hashVector verts 


mkFromVerticesF :: ( Storable v, Foldable f ) => MeshId -> f v -> Mesh v
mkFromVerticesF ident = mkFromVertices ident . VS.fromList . toList 


mkFromVertices :: Storable v => MeshId -> VS.Vector v -> Mesh v
mkFromVertices ident verts =
  emptyMesh & meshId               .~ ident 
            & meshVertices         .~ verts
            & meshComponents.at "" ?~ (makeComponent "" ( VS.generate (VS.length verts) id ))



makeComponent :: MeshId -> VS.Vector Int -> MeshComponent
makeComponent ident indices = MeshComponent ident indices (hashVector indices) 


-- | builds a mesh from geometry

-- preserving the elements but flattens the surfaces
-- a component for the indices is created at the root ("")
meshFromTriGeo :: (Storable v) => MeshId -> TriGeo v -> Mesh v
meshFromTriGeo ident geo@Geometry{..} =
    emptyMesh & meshId                .~ ident
              & meshVertices          .~ (VS.convert _geoVertices)
              & meshComponents.at ""  ?~ makeComponent "" (geo^.flattenIndices)


-- | unified empty mesh with "" identifier
emptyMesh :: Storable v => Mesh v
emptyMesh = Mesh "" VS.empty mempty 0


-- | appends a new component to a mesh
--
-- keeps id
-- hash is recalculated
-- component indices are recalculated
-- component id is prepended with meshId and a dot `.` (meshId.componentId)
-- component indices aren't bound-checked against the given vertices
appendComponent :: Storable v => Mesh v -> ( MeshComponent, VS.Vector v ) -> Mesh v
appendComponent mesh (comp, verts) =
    mesh & meshVertices <>~ verts
         & meshComponents.at (comp^.componentId) ?~ componentToAdd
    where
    componentToAdd =
        comp &  componentId          %~ ((mesh^.meshId) `snoc` '.' ++)
             &  componentIndexBuffer %~ VS.map (+ VS.length (mesh^.meshVertices) )


appendGeometry :: Storable v => Mesh v -> (MeshId, TriGeo v) -> Mesh v
appendGeometry mesh (ident, geo) = 
    let idxs  = geo^.flattenIndices
        verts = VS.convert $ geo^.geoVertices
    in mesh `appendComponent` (makeComponent ident idxs, verts)

{--
Utilities
--}

-- | colapse surfaces 
flattenIndices :: Getter (TriGeo v) (VS.Vector Int)
flattenIndices = to $ VS.concatMap (VS.fromList . toList) . VS.convert . flatten . _geoSurfaces
  where flatten = V.foldr (V.++) V.empty


hashVector :: Storable a => VS.Vector a -> XH.XXHash
hashVector = XH.xxHash' . BS.vectorToByteString

-- stolen from http://www.haskell.org/pipermail/beginners/2010-October/005571.html
octets :: Word32 -> [Word8]
octets w = 
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

