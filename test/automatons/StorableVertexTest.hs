module StorableVertexTest
    ( storableVertexSpecs
    ) where


import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

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

storableVertexSpecs :: Spec
storableVertexSpecs = do
    describe "storable vertex" $ do

        it "can store a vertex to pointer and read it from this pointer" $ do
            ptr <- malloc :: IO (Ptr Vertex4342)
            poke ptr fixureVertex
            rVertex <- peek ptr
            free ptr
            rVertex `shouldBe` fixureVertex

