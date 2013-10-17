{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-} -- evil but just for _offset _count _stride

module Yage.Rendering.VertexSpec
    ( Vertex(..), _position, _normal, _color
    , Position4f, Normal3f, Color4f
    , Vertex434
    , VertexSize
    , VertexMapDef
    , VertexDef, _vertMap, _vertSize
    , VertexAttribMapping, _attrName, _attrDef
    , VertexAttribDef, _attrOffset, _attrCount, _attrSize
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


type VertexMapDef s      = Getting VertexAttribMapping s VertexAttribMapping
type VertexSize          = Int
type VertexDef           = ([VertexAttribMapping], VertexSize)
type VertexAttribMapping = (String, VertexAttribDef)
type VertexAttribDef     = (Int, Int, Int) -- | (offset, count, size)

_attrName   = _1
_attrDef    = _2

_vertMap    = _1
_vertSize   = _2

_attrOffset = _1
_attrCount  = _2
_attrSize   = _3

define :: (Storable s) => [VertexMapDef s] -> s -> VertexDef
define defs datum = (, sizeOf datum) $ map (\g -> datum^.g) defs & scanl1Of (traverse._2) offsetPlus
    where
        offsetPlus :: VertexAttribDef -> VertexAttribDef -> VertexAttribDef 
        offsetPlus l1 l2 = set _attrOffset (l1^._attrOffset + l1^._attrSize + l2^._attrOffset) l2


toDef :: (Typeable (t s), Traversable t, Storable (t s)) => String -> Getter (t s) VertexAttribMapping
toDef name = to $ \s -> (name, memoryLayout s)
    where 
        memoryLayout :: (Traversable t, Storable (t s)) => t s -> VertexAttribDef
        memoryLayout s = (0, lengthOf traverse s, sizeOf s)

(^:=) :: (Typeable (t a), Traversable t, Storable (t a)) => String -> Getter s (t a) -> Getter s VertexAttribMapping
s ^:= l = l.toDef s
