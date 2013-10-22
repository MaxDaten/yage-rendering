module MathNormalTest where

import Test.Hspec

import Linear (V3(..))
import Yage.Math

xAxis, yAxis, zAxis :: V3 Float
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

normalCalculation :: Spec
normalCalculation = do
    describe "typical x- y- z- plain normals colinear to the axis" $ do
        
        it "calculates a correct x-plain normal" $ do
            let theNormal = normal yAxis zAxis
            theNormal `shouldBe` xAxis

        it "calculates a correct y-plain normal" $ do
            let theNormal = normal (negate xAxis) zAxis
            theNormal `shouldBe` yAxis

        it "calculates a correct z-plain normal" $ do
            let theNormal = normal yAxis (negate xAxis)
            theNormal `shouldBe` zAxis

        it ("calculates the plain normal form with it's normal " ++
            "n == x-Axis from 3 points on a x-colinear plain") $ do
                let [v1, v2, v3] = [V3 3 5 1, V3 3 0 0, V3 3 0 8]
                    (n, _, _) = plainNormalForm v1 v2 v3
                n `shouldBe` xAxis
        
        it ("calculates the plain normal form with it's normal " ++
            "n == y-Axis from 3 points on a y-colinear plain") $ do
                let [v1, v2, v3] = [V3 (-1) 5 0, V3 0 5 0, V3 0 5 1]
                    (n, _, _) = plainNormalForm v1 v2 v3
                n `shouldBe` yAxis

        it ("calculates the plain normal form with it's normal " ++
            "n == z-Axis from 3 points on a z-colinear plain") $ do
                let [v1, v2, v3] = [V3 (1) 0 5, V3 0 0 5, V3 0 (1) 5]
                    (n, _, _) = plainNormalForm v1 v2 v3
                n `shouldBe` zAxis