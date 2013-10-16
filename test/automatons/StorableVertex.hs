module StorableVertex
    (vertexSpecs
    ) where


import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

import Linear
import Test.Hspec

import Yage.Rendering.VertexSpec

fixureVertex :: Vertex434
fixureVertex = Vertex
                { __position  = V4 2.0 3.0 5.0 7.0 
                , __normal    = V3 11.0 13.0 17.0
                , __color     = V4 19.0 23.0 29.0 31.0
                }

vertexSpecs :: Spec
vertexSpecs = do
    describe "vertex definition" $ do
        let v4Size = sizeOf (undefined::V4 Float)
            v3Size = sizeOf (undefined::V3 Float)
            fixureDef = [("pos", (0, 4, v4Size)), ("norm", (v4Size, 3, v3Size)), ("col", (v3Size+v4Size, 4, v4Size))]
            v = fixureVertex
            defs = [ "pos"  ^:= _position
                   , "norm" ^:= _normal
                   , "col"  ^:= _color
                   ]
        
        it "generates a bunch of definitions" $ do
            let spec = define v defs
                        
            spec `shouldBe` fixureDef

    describe "storable vertex" $ do

        it "can store a vertex to pointer and read it from this pointer" $ do
            ptr <- malloc :: IO (Ptr Vertex434)
            poke ptr fixureVertex
            rVertex <- peek ptr
            free ptr
            rVertex `shouldBe` fixureVertex

