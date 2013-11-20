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
    ( module Yage.Rendering.VertexSpec
    , module Foreign.Storable.Utils
    ) where

---------------------------------------------------------------------------------------------------
import             Yage.Prelude
import             Control.Lens                    
---------------------------------------------------------------------------------------------------
import             Data.Data
import             Data.List                       (map, scanl, take, iterate, length, scanl1, sum, concat, head, transpose)
import             Data.Traversable                hiding (mapM)
---------------------------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Monad
---------------------------------------------------------------------------------------------------
import             Foreign.Storable
import             Foreign.Storable.Utils
import             Foreign                         hiding (void)
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
    let (offsets, elems, stride) = attribsLayout attrs
        vadMapping               = attribsToVAD attrs
    in allocaBytes (stride * elems) $ \ptr -> do
            pokeAttribs ptr attrs
            VertexBufferObject vadMapping <$> fromPtr GL.ArrayBuffer (stride * elems) ptr
 

attribsToVAD :: [VertexAttribute] -> [VertexDescriptor]
attribsToVAD attrs = 
    let (offsets, elems, stride) = attribsLayout attrs
    in map (toVAD stride) $ zip offsets attrs
    where
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
pokeAttribs :: Ptr a -> [VertexAttribute] -> IO ()
pokeAttribs ptr attrs = 
    let grouped = concat . transpose $ map attributeData attrs
    in foldM_ pokeElem ptr grouped
    where 
        pokeElem :: Ptr a -> ToGLBuffer -> IO (Ptr a)
        pokeElem ptr (ToGLBuffer a) = do
            poke (castPtr ptr) a
            return $ ptr `plusPtr` sizeOf a

peekAttribs :: (Storable p, Storable n, Storable c, Storable t)
            => Ptr (Vertex p n c t) -> Layout -> IO ([Vertex p n c t])
peekAttribs ptr (_, num, stride) = 
    let offsets  = take num $ iterate (stride+) 0
    in mapM (peek . (ptr `plusPtr`)) offsets



  
type Offset = Int
type Elems  = Int
type Stride = Int     
type Layout = ([Offset], Elems, Stride)        

attribsLayout :: [VertexAttribute] -> Layout
attribsLayout attrs =
    let sizes       = map attribSize attrs
        offsets     = scanl (+) 0 sizes
        stride      = sum sizes
        elems       = vertexCount . head $ attrs
    in (offsets, elems, stride)
    where
        attribSize :: VertexAttribute -> Int
        attribSize (VertexAttribute _ (ToGLBuffer a:_)) = sizeOf a
        attribSize (VertexAttribute _ []) = error "missing attribute data"

        vertexCount :: VertexAttribute -> Int
        vertexCount (VertexAttribute _ as) = length as 

deriving instance Show VertexBufferObject
deriving instance Show VertexDescriptor