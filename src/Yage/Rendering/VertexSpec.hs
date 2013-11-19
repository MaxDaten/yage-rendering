{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-} -- evil but just for _offset _count _stride
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}

module Yage.Rendering.VertexSpec
    ( Vertex(..), vPosition, vNormal, vColor, vTexture
    , Position4f, Position3f, Normal3f, Color4f, Texture2f
    
    , Vertex4342, Vertex2P2T4C

    , VertexBufferObject(..), VertexAttribute, (@=), VertexDescriptor(..)
    , makeVertexBufferF
    , module Foreign.Storable.Utils
    ) where

---------------------------------------------------------------------------------------------------
import             Yage.Prelude
import             Control.Lens                    
---------------------------------------------------------------------------------------------------
import             Data.Data
import             Data.List                       (map, scanl, length, scanl1, sum, concat, head, transpose)
import             Data.Traversable
---------------------------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Monad
---------------------------------------------------------------------------------------------------
import             Foreign.Storable
import             Foreign.Storable.Utils
import             Foreign
---------------------------------------------------------------------------------------------------
import             Linear                          (V2(..), V3(..), V4(..))
import             Graphics.Rendering.OpenGL       (GLfloat)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.GLUtil
{-=================================================================================================-}

type Position4f = V4 GLfloat
type Position3f = V3 GLfloat
type Position2f = V2 GLfloat
type Normal3f   = V3 GLfloat
type Color4f    = V4 GLfloat
type Texture2f  = V2 GLfloat
type Vertex4342 = Vertex Position4f Normal3f Color4f Texture2f
type Vertex2P2T4C = Vertex Position2f () Color4f Texture2f

data Vertex p n c t = Vertex 
    { _vPosition      :: p
    , _vNormal        :: n
    , _vColor         :: c
    , _vTexture       :: t
    } deriving (Show, Eq, Data, Typeable)

makeLenses ''Vertex



instance (Storable p, Storable n, Storable c, Storable t) => Storable (Vertex p n c t) where
    sizeOf _    = sizeOf (undefined::p) + sizeOf (undefined::n) + sizeOf (undefined::c)+ sizeOf (undefined::t)
    alignment _ = alignment (undefined::p)
    peek ptr = 
        Vertex 
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr (sizeOf (undefined :: p))
            <*> peekByteOff ptr (sizeOf (undefined :: p) + sizeOf (undefined :: n))
            <*> peekByteOff ptr (sizeOf (undefined :: p) + sizeOf (undefined :: n) + sizeOf (undefined :: c))

    poke ptr Vertex{..} = do
        pokeByteOff ptr 0 _vPosition
        pokeByteOff ptr (sizeOf (undefined :: p)) _vNormal
        pokeByteOff ptr (sizeOf (undefined :: p) + sizeOf (undefined :: n)) _vColor
        pokeByteOff ptr (sizeOf (undefined :: p) + sizeOf (undefined :: n) + sizeOf (undefined :: c)) _vTexture





-- the main reason for this box is to transpose the attribute-list with the data list
-- to write the data in a sequence
data ToGLBuffer = forall t a. (Show (t a), Traversable t, Storable (t a), HasVariableType (t a)) 
                => ToGLBuffer (t a)


data VertexDescriptor = forall a. VertexDescriptor
    { attrName :: String
    , vad      :: GL.VertexArrayDescriptor a
    }

data VertexAttribute = VertexAttribute
    { attributeName    :: String
    , attributeData    :: [ToGLBuffer]
    }

makeLenses ''VertexAttribute

data VertexBufferObject = forall a. VertexBufferObject
    { attribVADs  :: [VertexDescriptor] 
    , vbo         :: GL.BufferObject
    }


makeVertexAttribute :: (Show (t a), Traversable t, Storable (t a), HasVariableType (t a)) 
                    => String -> [t a] -> VertexAttribute
makeVertexAttribute name aData = VertexAttribute name (map ToGLBuffer aData)

infixr 2 @=
(@=) :: (Show (t a), Traversable t, Storable (t a), HasVariableType (t a)) 
     => String -> [t a] -> VertexAttribute
(@=) = makeVertexAttribute


makeVertexBufferF :: [VertexAttribute] -> IO VertexBufferObject
makeVertexBufferF attrs = 
    let sizes       = map attribSize attrs
        offsets     = scanl1 (+) sizes
        stride      = sum sizes
        elems       = vertexCount . head $ attrs
        vadMapping  = map (toVAD stride) $ zip offsets attrs
    in allocaBytes (stride * elems) $ \ptr -> do
            _ <- makeComposition ptr attrs
            VertexBufferObject vadMapping <$> fromPtr GL.ArrayBuffer (stride * elems) ptr

    where
        attribSize :: VertexAttribute -> Int
        attribSize (VertexAttribute _ (ToGLBuffer a:_)) = sizeOf a
        attribSize (VertexAttribute _ []) = error "missing attribute data"

        vertexCount :: VertexAttribute -> Int
        vertexCount (VertexAttribute _ as) = length as 
        
        toVAD :: Int -> (Int, VertexAttribute) -> VertexDescriptor
        toVAD stride (off, VertexAttribute name (ToGLBuffer a:_)) =
            let num     = fromIntegral $ lengthOf traverse a
                dType   = variableDataType . variableType $ a
                vad     = GL.VertexArrayDescriptor num dType (fromIntegral stride) (offsetPtr off) 
            in VertexDescriptor name vad
        toVAD _      (_, VertexAttribute _ []) = error "missing attribute data"

        -- group the seperated vertex data together
        -- [[v0..vn], [c0..cn]] -> [[v0, c0]..[vn, cn]]
        -- this is the reason we need the ToGLBuffer type
        makeComposition :: Ptr a -> [VertexAttribute] -> IO (Ptr a)
        makeComposition ptr attrs = 
            let grouped = concat . transpose $ map attributeData attrs
            in foldM pokeElem ptr grouped

        -- pokeElem :: Ptr a -> ToGLBuffer -> IO (Ptr a)
        pokeElem ptr (ToGLBuffer a) = do
            poke (castPtr ptr) a
            return $ ptr `plusPtr` sizeOf a


{--
verts :: [V3 GL.GLfloat]
verts = [V3 0 0 0]

texs :: [V2 GL.GLfloat]
texs  = [V2 0 0]

color :: [V4 GL.GLfloat]
color = [V4 0 0 0 0]

main = do
    vbo <- makeVertexBufferF
                [ "position" @= (verts :: [V3 GL.GLfloat])
                , "texture"  @= (texs  :: [V2 GL.GLfloat])
                , "color"    @= (color :: [V4 GL.GLfloat])
                ]
    vbo `seq` print "end"
--}

deriving instance Show VertexBufferObject
deriving instance Show VertexDescriptor