{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NoMonomorphismRestriction  #-} -- evil but just for _offset _count _stride

module Yage.Rendering.VertexSpec
    ( Vertex(..), _position, _normal, _color
    , Position4f, Normal3f, Color4f
    , Vertex434
    , VertexDef, _name, _layout, _data
    , MemoryLayout, _offset, _stride, _count
    , define, toDef, (^:=)
    ) where
---------------------------------------------------------------------------------------------------
import             Yage.Prelude
import             Control.Lens
---------------------------------------------------------------------------------------------------
import             Data.Data
import             Data.List                       (map)
---------------------------------------------------------------------------------------------------
import             Foreign.Storable
import             Foreign.SomeStorable
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), V4(..))
import             Graphics.Rendering.OpenGL       (GLfloat)
---------------------------------------------------------------------------------------------------

type Position4f = V4 GLfloat
type Normal3f   = V3 GLfloat
type Color4f    = V4 GLfloat
type Vertex434  = Vertex Position4f Normal3f Color4f

data Vertex p n c = Vertex 
    { __position      :: p
    , __normal        :: n
    , __color         :: c
    } deriving (Show, Eq, Data, Typeable)

makeLenses ''Vertex


instance (Storable p, Storable n, Storable c) => Storable (Vertex p n c) where
    sizeOf _    = sizeOf (undefined::p) + sizeOf (undefined::n) + sizeOf (undefined::c)
    alignment _ = alignment (undefined::p)
    peek ptr = 
        Vertex 
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr (sizeOf (undefined :: p))
            <*> peekByteOff ptr (sizeOf (undefined :: p) + sizeOf (undefined :: n))

    poke ptr Vertex{..} = do
        pokeByteOff ptr 0 __position
        pokeByteOff ptr (sizeOf (undefined :: p)) __normal
        pokeByteOff ptr (sizeOf (undefined :: p) + sizeOf (undefined :: n)) __color



type VertexDef = (String, MemoryLayout, SomeStorable)
type MemoryLayout = (Int, Int, Int) -- | (offset, count, stride)

_name   = _1
_layout = _2
_data   = _3
_offset = _1
_count  = _2
_stride = _3

define :: s -> [Getting VertexDef s VertexDef] -> [VertexDef]
define datum defs = map (\g -> datum^.g) defs & scanl1Of (traverse._2) offsetPlus
    where
        offsetPlus :: MemoryLayout -> MemoryLayout -> MemoryLayout 
        offsetPlus l1 l2 = set _offset (l1^._offset + l1^._stride + l2^._offset) l2


toDef :: (Typeable (t s), Traversable t, Storable (t s)) => String -> Getter (t s) VertexDef
toDef name = to $ \s -> (name, memoryLayout s, toStorable s)
    where 
        memoryLayout :: (Traversable t, Storable (t s)) => t s -> MemoryLayout
        memoryLayout s = (0, lengthOf traverse s, sizeOf s)

(^:=) :: (Typeable (t a), Traversable t, Storable (t a)) => String -> Getter s (t a) -> Getter s VertexDef
s ^:= l = l.toDef s
