{-# LANGUAGE RecordWildCards #-}
module Yage.Resources where

import             Yage.Prelude

import             Linear                          (V3(..), V4(..))
import             Data.List                       (length)
import             Linear.Quaternion               (Quaternion)
import             Graphics.Rendering.OpenGL       (GLfloat)
import             Foreign.Storable

---------------------------------------------------------------------------------------------------

type Orientation = Quaternion GLfloat
type Scale = V3 GLfloat

--type Vertex = V4 GLfloat
type Position = V4 GLfloat
type Normal = V3 GLfloat
type Color = V4 GLfloat
type Index = Int

data Vertex = Vertex 
    { position :: Position
    , normal   :: Normal
    , color    :: Color
    } deriving (Show, Eq)

instance Storable Vertex where
    sizeOf _ = sizeOf (undefined::Position) + sizeOf (undefined::Normal) + sizeOf (undefined::Color)
    alignment _ = alignment (undefined::Position)
    peek ptr = 
        Vertex 
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr (sizeOf (undefined :: Position))
            <*> peekByteOff ptr (sizeOf (undefined :: Position) + sizeOf (undefined :: Normal))

    poke ptr Vertex{..} = do
        pokeByteOff ptr 0 position
        pokeByteOff ptr (sizeOf (undefined :: Position)) normal
        pokeByteOff ptr (sizeOf (undefined :: Position) + sizeOf (undefined :: Normal)) color

data TriMesh = TriMesh
    { meshId   :: !String
    , vertices :: ![Vertex]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show)

instance Eq TriMesh where
    a == b = meshId a == meshId b

instance Ord TriMesh where
    compare a b = compare (meshId a) (meshId b)

mkTriMesh :: String -> [Vertex] -> [Index] -> TriMesh
-- TODO some assertions for invalid meshes
mkTriMesh id vs ixs = TriMesh id vs ixs $ (length ixs) `quot` 3

---------------------------------------------------------------------------------------------------

data YageShaderResource = YageShaderResource
    { vert  :: FilePath
    , frag  :: FilePath
    } deriving (Show, Eq, Ord)

data RenderDefinition = RenderDefinition
    { defs :: (TriMesh, YageShaderResource)
    } deriving (Show, Eq, Ord)


