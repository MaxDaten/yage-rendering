{-# OPTIONS_GHC -fno-warn-orphans #-}
module TransformationTest where


import Test.Hspec
import Test.QuickCheck

import Control.Applicative
import Linear

import Yage.Rendering.Transformation

transformationProps :: Spec
transformationProps = do
    describe "Properties of Transformation" $
        describe "inverseTransformation" $
            it "is `id` when applied twice" $ property $
                \t -> nearZero $ ( inverseTransformation ( inverseTransformation t ) ) - ( t :: Transformation Double )


instance Arbitrary a => Arbitrary (Transformation a) where
    arbitrary = Transformation <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Quaternion a) where
    arbitrary = Quaternion <$> arbitrary <*> arbitrary