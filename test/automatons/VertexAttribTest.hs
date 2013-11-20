{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module VertexAttribTest
    ( vertexAttribSpec
    ) where


import Foreign.Storable
import Foreign.Marshal.Alloc


import Control.Lens
import Linear
import Test.Hspec

import Yage.Rendering.VertexSpec

fixureVertex :: Vertex4342
fixureVertex = Vertex
                { _vPosition  = V4 2.0 3.0 5.0 7.0 
                , _vNormal    = V3 11.0 13.0 17.0
                , _vColor     = V4 19.0 23.0 29.0 31.0
                , _vTexture   = V2 1.0 0.0
                }

fixureVertices = replicate 10 fixureVertex 

fixureAttribs   = [ "in_vert_position" @= fixureVertices^..traverse.vPosition
                  , "in_vert_normal"   @= fixureVertices^..traverse.vNormal
                  , "in_vert_color"    @= fixureVertices^..traverse.vColor
                  , "in_vert_texture"  @= fixureVertices^..traverse.vTexture
                  ]

floatSize = sizeOf(undefined::Float)
fixureLayout = ([ 0
                , 4*floatSize
                , (4 + 3) * floatSize
                , (4 + 3 + 4) * floatSize
                , (4 + 3 + 4 + 2) * floatSize
                ], 10, (4 + 3 + 4 + 2) * floatSize)


vertexAttribSpec :: Spec
vertexAttribSpec = do
    describe "vertex attributes" $ do
        it "calculates a correct memory layout for attribues" $
            attribsLayout fixureAttribs `shouldBe` fixureLayout

        it "pokes and peeks VertexAttributes correctly" $ do
            let lay@(_, elems, stride) = attribsLayout fixureAttribs

            allocaBytes (stride * elems) $ \ptr -> do
                pokeAttribs ptr fixureAttribs
                verts <- peekAttribs ptr lay :: IO ([Vertex4342])
                verts `shouldBe` fixureVertices

